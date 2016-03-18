%% @doc This module implements the gen_event server used for error logging.
%%
%%      We provide our own facility to generate the log messages to ensure that no sensitive data is included
%%      in the error logs.
-module(cloak_error_logger_handler).
-behaviour(gen_event).

-include("cloak.hrl").

%% API
-export([
  start/0
]).

%% gen_event callback functions
-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Unregister all error_logger event handlers and register this error logger as the only event handler.
-spec start() -> ok.
start() ->
  [ check_delete_result(error_logger:delete_report_handler(Handler)) ||
      Handler <- gen_event:which_handlers(error_logger) ],
  ok = error_logger:add_report_handler(?MODULE).


%% -------------------------------------------------------------------
%% gen_event callback functions
%% -------------------------------------------------------------------

init(_) ->
  {ok, undefined}.

handle_event(Event, State) ->
  case cloak_conf:in_development() of
    false -> handle_event_in_production(Event);
    true ->  handle_event_in_development(Event)
  end,
  {ok, State}.

handle_call(_Msg, State) ->
  ?ERROR("unallowed call to error logger"),
  {ok, undefined, State}.

handle_info(_Msg, State) ->
  ?ERROR("unallowed message to error logger"),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

%% It is critical for our cloaks that we can remove any other error_logger handler from the system (to ensure
%% that we do not leak information).  Thus we cannot continue if we cannot remove an error_logger handler.
check_delete_result({'EXIT', Reason}) ->
  ?CRITICAL("cannot remove error_logger handler (~p)", [Reason]),
  throw(unrecoverable);
check_delete_result({error, Reason}) ->
  ?CRITICAL("cannot remove error_logger handler (~p)", [Reason]),
  throw(unrecoverable);
check_delete_result(_) ->
  ok.

handle_event_in_development(Event) ->
  % Somewhat hacky way to log through lager's default error logger handler.
  {ok, LagerState} = error_logger_lager_h:init([undefined, undefined]),
  error_logger_lager_h:handle_event(Event, LagerState).

handle_event_in_production({error, _Gleader, {_Pid, _Format, _Data}}) ->
  lager:error("The system produced an error message");
handle_event_in_production({
      error_report,
      _Gleader,
      {_Pid, crash_report, [[{_Reason, {Module, Function, _Parameters}}|_]|_]}
    }) when is_atom(Module) andalso is_atom(Function) ->
  lager:critical("The system produced a crash report in ~p:~p", [Module, Function]);
handle_event_in_production({error_report, _Gleader, {_Pid, _Type, _Report}}) ->
  lager:error("The system produced an error report");
handle_event_in_production({warning_msg, _Gleader, {_Pid, _Format, _Data}}) ->
  lager:warning("The system produced a warning message");
handle_event_in_production({warning_report, _Gleader, {_Pid, _Type, _Report}}) ->
  lager:warning("The system produced a warning report");
handle_event_in_production({info_msg, _Gleader, {_Pid, _Format, _Data}}) ->
  lager:info("The system produced an info message");
handle_event_in_production({info_report, _Gleader, {_Pid, _Type, _Report}}) ->
  lager:info("The system produced an info report");
handle_event_in_production(_) ->
  ?ERROR("unknown event to ~p", [?MODULE]).
