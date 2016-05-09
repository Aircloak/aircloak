%% @doc Testing the {@link queued_worker} queued computation implementation.
-module(queued_worker_test).
-behavour(queued_worker).
-proper(simple).

-include_lib("proper/include/proper.hrl").

-export([
  run_job/1
]).

-define(MAX_ITEMS, 40).
-define(TIMEOUT, 100).
-define(MAX_CONCURRENCY, 20).
-define(RUN_TIMEOUT, (?TIMEOUT * ?MAX_ITEMS * 10)).


%% ---------------------------------------------------------------------
%% PropEr Tests
%% ---------------------------------------------------------------------

%% Test that not more than requested concurrent executions are performed and that all requests are
%% processed.
prop_queued_computation() ->
  numtests(10,
    ?FORALL({NumberOfItems, MaxConcurrency}, g_parameters(),
     ?TRAPEXIT(
       measure("Number of items", NumberOfItems,
         measure("Maximum concurrency", MaxConcurrency,
           measure("Items/Concurrency", NumberOfItems / MaxConcurrency,
             process_items(NumberOfItems, MaxConcurrency))))))).


%% ---------------------------------------------------------------------
%% Generators
%% ---------------------------------------------------------------------

g_parameters() ->
  ?LET(NumberOfItems, integer(1, ?MAX_ITEMS),
    ?LET(MaxConcurrency, integer(1, ?MAX_CONCURRENCY),
      return({NumberOfItems, MaxConcurrency}))).


%% ---------------------------------------------------------------------
%% queued_worker callback
%% ---------------------------------------------------------------------

run_job({ProcessCounterPid, CollectorPid}) ->
  add_concurrent_job(ProcessCounterPid),
  CollectorPid ! run_job,
  timer:sleep(?TIMEOUT),
  remove_concurrent_job(ProcessCounterPid),
  ok.


%% ---------------------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------------------

process_items(NumberOfItems, MaxConcurrency) ->
  ProcessCounterPid = start_max_concurrent_processings(),
  queued_worker:start_link(?MODULE, ?MODULE, MaxConcurrency),
  CollectorPid = start_collector_process(self(), NumberOfItems),
  run_all_jobs(ProcessCounterPid, CollectorPid, NumberOfItems),
  receive
    all_collected ->
      queued_worker:stop(?MODULE),
      Max = stop_max_concurrent_processings_process(ProcessCounterPid),
      %% This assumes that we spawn the jobs fast enough to really fill the queue.
      ((NumberOfItems >= MaxConcurrency) andalso (Max =:= MaxConcurrency)) orelse
        ((NumberOfItems < MaxConcurrency) andalso (Max =< NumberOfItems))
  after ?RUN_TIMEOUT ->
    false
  end.

run_all_jobs(_ProcessCounterPid, _CollectorPid, 0) ->
  ok;
run_all_jobs(ProcessCounterPid, CollectorPid, N) ->
  queued_worker:run(?MODULE, {ProcessCounterPid, CollectorPid}),
  run_all_jobs(ProcessCounterPid, CollectorPid, N-1).

start_collector_process(Master, NumberOfItems) ->
  spawn_link(fun () -> collector_process(Master, NumberOfItems) end).

collector_process(Master, 0) ->
  Master ! all_collected;
collector_process(Master, N) ->
  receive
    run_job -> collector_process(Master, N-1)
  end.

start_max_concurrent_processings() ->
  spawn_link(fun () -> max_concurrent_processings(0, 0) end).

stop_max_concurrent_processings_process(Pid) ->
  Pid ! {stop, self()},
  receive
    {concurrent_processings, Pid, Max} -> Max
  end.

add_concurrent_job(CounterPid) ->
  CounterPid ! {add_job, self()},
  receive
    {add_job_done, CounterPid} -> ok
  end.

remove_concurrent_job(CounterPid) ->
  CounterPid ! {remove_job, self()},
  receive
    {remove_job_done, CounterPid} -> ok
  end.

max_concurrent_processings(N, Max) ->
  receive
    {add_job, Client} ->
      Client ! {add_job_done, self()},
      max_concurrent_processings(N+1, max(Max, N+1));
    {remove_job, Client} ->
      Client ! {remove_job_done, self()},
      max_concurrent_processings(N-1, Max);
    {stop, Pid} ->
      Pid ! {concurrent_processings, self(), Max}
  end.
