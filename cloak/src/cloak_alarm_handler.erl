%% @doc Custom event handler for alarms. We need this to use different log levels
%%      for different alarms.
-module(cloak_alarm_handler).
-behaviour(gen_event).

%% API
-export([
  install/0
]).

%% Callbacks
-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).

-include("cloak.hrl").

-define(REPORT_ALARM(Type, Alarm), ?Type("Alarm set: ~p", [Alarm])).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Replaces default alarm handler with our own.
-spec install() -> ok.
install() ->
  case cloak_conf:get_val(alarm_handler, install) of
    true ->
      gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {?MODULE, undefined});
    false ->
      ok
  end.


%% -------------------------------------------------------------------
%% gen_event callbacks
%% -------------------------------------------------------------------

%% @hidden
-spec init({undefined, any()}) -> {ok, undefined}.
init({undefined, _OldHandler}) ->
  {ok, undefined}.

%% @hidden
-spec handle_event(any(), undefined) -> {ok, undefined}.
handle_event({set_alarm, Alarm}, State) ->
  case log_level(Alarm) of
    info -> ?REPORT_ALARM(INFO, Alarm);
    warning -> ?REPORT_ALARM(WARNING, Alarm);
    % log_level/1 does not produce error/critical at the moment.  The code is commented out as
    % dialyzer dislikes unused clauses/branches.
    %error -> ?REPORT_ALARM(ERROR, Alarm);
    %critical -> ?REPORT_ALARM(CRITICAL, Alarm);
    alert -> ?REPORT_ALARM(ALERT, Alarm)
  end,
  {ok, State};
handle_event({clear_alarm, Alarm}, State) ->
  ?INFO("Alarm cleared ~p", [Alarm]),
  {ok, State};
handle_event(_, State)-> {ok, State}.

%% @hidden
-spec handle_call(any(), undefined) -> {ok, {error, bad_query}, undefined}.
handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

%% @hidden
-spec handle_info(any(), undefined) -> {ok, undefined}.
handle_info(_, State) -> {ok, State}.

%% @hidden
-spec code_change(any(), undefined, any()) -> {ok, undefined}.
code_change(_, State, _) -> {ok, State}.

%% @hidden
-spec terminate(any(), undefined) -> ok | {atom(), undefined}.
terminate(swap, State) -> {?MODULE, State};
terminate(_Reason,_State)  -> ok.


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

log_level({system_memory_high_watermark, _}) -> alert;
log_level({swap_usage_increase, _}) -> warning;
log_level({disk_usage_increase, _}) -> warning;
log_level({high_cpu_usage, _}) -> warning;
log_level({atom_usage_increase, _}) -> warning;
log_level({binary_usage_increase, _}) -> warning;
log_level({ets_usage_increase, _}) -> warning;
log_level({ets_tables_increase, _}) -> warning;
log_level({processes_increase, _}) -> warning;
log_level(_) -> info.
