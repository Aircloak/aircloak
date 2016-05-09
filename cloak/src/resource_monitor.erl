%% @doc Periodically checks OS and BEAM metrics, and reports anomalies by
%%      setting the alarm.
-module(resource_monitor).
-behaviour(gen_server).

%% API
-export([
  start_link/0
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("cloak.hrl").

-record(state, {
  %% We signal a 300 MB change. Correction is 1 KB per calculation interval (1 minute).
  swap_trend = trend_detector:new(300, 0.001) :: trend_detector:trend_detector(),
  disk_usage_trends = dict:new(),
  %% We watch for 10MB atom table usage increase. No correction is used since atoms are not GC-ed.
  atom_usage_trend = trend_detector:new(10, 0) :: trend_detector:trend_detector(),
  %% We signal a 300 MB change. Correction is 1 KB per calculation interval (1 minute).
  binary_usage_trend = trend_detector:new(300, 0.001) :: trend_detector:trend_detector(),
  %% We signal a 300 MB change. Correction is 1 KB per calculation interval (1 minute).
  ets_usage_trend = trend_detector:new(300, 0.001) :: trend_detector:trend_detector(),
  %% ETS tables are very limited (max 1400 tables by default)
  ets_tables_trend = trend_detector:new(200, 0) :: trend_detector:trend_detector(),
  %% In general processes are cheap, but we want to detect a continuously growing number.
  processes_trend = trend_detector:new(100000, 1) :: trend_detector:trend_detector(),
  last_disk_usage = disksup:get_disk_data() :: [{string(), number(), number()}],
  num_of_cpu_cores = num_of_cpu_cores() :: pos_integer()
}).

-define(DISK_CHECK_INTERVAL, (timer:hours(6))).
-define(TREND_CHECK_INTERVAL, (timer:minutes(1))).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the server
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  erlang:send_after(?TREND_CHECK_INTERVAL, self(), analyze_usage),
  erlang:send_after(?DISK_CHECK_INTERVAL, self(), estimate_disk_usage),
  {ok, #state{}}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(analyze_usage, State) ->
  analyze_cpu(State#state.num_of_cpu_cores),
  State1 = analyze_beam(analyze_disk(analyze_memory(State))),
  erlang:send_after(?TREND_CHECK_INTERVAL, self(), analyze_usage),
  {noreply, State1};
handle_info(estimate_disk_usage, #state{last_disk_usage=LastDiskUsage} = State) ->
  CurrentDiskUsage = disksup:get_disk_data(),
  lists:foreach(
    fun({Partition, _, Percentage}) ->
      PreviousUsage = case lists:keyfind(Partition, 1, LastDiskUsage) of
        false -> Percentage;
        {Partition, _, Previous} -> Previous
      end,
      %% Magical 0.01 compensates for zero or negative numbers. Given an interval of 12 hours,
      %% a no change in disk usage will estimate to 5000 days.
      Increase = max(0.01, Percentage - PreviousUsage),
      EstimatedOutOfSpace = round((100 / Increase) * (?DISK_CHECK_INTERVAL / timer:hours(24))),
      if
        EstimatedOutOfSpace < 31 ->
          ?ERROR("Estimated disk expiry for ~s is ~p days", [Partition, EstimatedOutOfSpace]);
        EstimatedOutOfSpace < 365 ->
          ?WARN("Estimated disk expiry for ~s is ~p days", [Partition, EstimatedOutOfSpace]);
        true ->
          ?INFO("Estimated disk expiry for ~s is ~p days", [Partition, EstimatedOutOfSpace])
      end
    end,
    CurrentDiskUsage
  ),
  erlang:send_after(?DISK_CHECK_INTERVAL, self(), estimate_disk_usage),
  {noreply, State#state{last_disk_usage=CurrentDiskUsage}};
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

analyze_memory(#state{swap_trend=SwapTrend} = State) ->
  MemoryUsage = memsup:get_system_memory_data(),
  % Convert to MBs
  TotalSwap = round(proplists:get_value(total_swap, MemoryUsage, 0) / 1000000),
  FreeSwap = round(proplists:get_value(free_swap, MemoryUsage, 0) / 1000000),
  UsedSwap = TotalSwap - FreeSwap,
  SwapTrend1 = add_to_trend(UsedSwap,
    {swap_usage_increase, [{used_mb, UsedSwap}, {total_mb, TotalSwap}]}, SwapTrend),
  State#state{swap_trend=SwapTrend1}.

analyze_disk(#state{disk_usage_trends=DiskUsageTrends} = State) ->
  DiskUsageTrends1 = lists:foldl(
    fun({Partition, _, UsedPercent}, TrendsAcc) ->
      PartitionTrend = case dict:find(Partition, TrendsAcc) of
        {ok, Existing} ->
          add_to_trend(UsedPercent,
              {disk_usage_increase, [{partition, Partition}, {used_percent, UsedPercent}]}, Existing);
        error ->
          % We signal a 10 percent change in disk usage.
          trend_detector:new(UsedPercent, 10, 0.001)
      end,
      dict:store(Partition, PartitionTrend, TrendsAcc)
    end,
    DiskUsageTrends,
    disksup:get_disk_data()
  ),
  State#state{disk_usage_trends=DiskUsageTrends1}.

analyze_beam(#state{
      atom_usage_trend=AtomUsageTrend,
      binary_usage_trend=BinaryUsageTrend,
      ets_usage_trend=EtsUsageTrend,
      ets_tables_trend=EtsTablesTrend,
      processes_trend=ProcessesTrend
    } = State) ->
  Memory = erlang:memory(),
  AtomUsedMb = round(proplists:get_value(atom_used, Memory, 0) / 1000000),
  BinaryUsedMb = round(proplists:get_value(binary, Memory, 0) / 1000000),
  EtsUsedMb = round(proplists:get_value(ets, Memory, 0) / 1000000),
  EtsTablesNum = length(ets:all()),
  ProcessesNum = length(erlang:processes()),
  State#state{
    atom_usage_trend=add_to_trend(AtomUsedMb,
      {atom_usage_increase, [{used_mb, AtomUsedMb}]}, AtomUsageTrend),
    binary_usage_trend=add_to_trend(BinaryUsedMb,
      {binary_usage_increase, [{used_mb, BinaryUsedMb}]}, BinaryUsageTrend),
    ets_usage_trend=add_to_trend(EtsUsedMb,
      {ets_usage_increase, [{used_mb, EtsUsedMb}]}, EtsUsageTrend),
    ets_tables_trend=add_to_trend(EtsTablesNum,
      {ets_tables_increase, [{num_tables, EtsTablesNum}]}, EtsTablesTrend),
    processes_trend=add_to_trend(ProcessesNum,
      {processes_increase, [{num_processes, ProcessesNum}]}, ProcessesTrend)
  }.

add_to_trend(Value, Alarm, Trend) ->
  Trend1 = trend_detector:add(Value, Trend),
  case trend_detector:threshold_reached(Trend1) of
    false -> Trend1;
    true ->
      alarm_handler:set_alarm(Alarm),
      trend_detector:clear(Trend1)
  end.

analyze_cpu(NumOfCpuCores) ->
  % Dividing by magical 256 gives top like values. See cpu_sup docs
  % (http://erlang.org/doc/man/cpu_sup.html) for more info.
  LoadAvg = cpu_sup:avg5() / 256,
  case (LoadAvg / NumOfCpuCores) > 0.9 of
    true -> set_alarm({high_cpu_usage, {load_avg_5, LoadAvg}});
    false -> unset_alarm(high_cpu_usage)
  end.

num_of_cpu_cores() ->
  case erlang:system_info(logical_processors_available) of
    X when is_integer(X) -> X;
    Other ->
      %% Hacky, but hopefully doesn't happen in production.
      %% If we can't get the system info we'll rely on number of schedulers.
      NumSchedulers = erlang:system_info(schedulers_online),
      ?INFO("erlang:system_info(logical_processors_available) returned ~p, using number of schedulers (~p) instead",
        [Other, NumSchedulers]),
      NumSchedulers
  end.

set_alarm({AlarmId, AlarmDescr}) ->
  case get(AlarmId) of
    set -> ok;
    undefined ->
      alarm_handler:set_alarm({AlarmId, AlarmDescr}),
      put(AlarmId, set)
  end.

unset_alarm(AlarmId) ->
  case get(AlarmId) of
    set ->
      alarm_handler:clear_alarm(AlarmId),
      erase(AlarmId);
    _ -> ok
  end.
