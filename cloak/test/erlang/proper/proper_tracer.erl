%% @doc Helper for better PropEr output. Can be used only with stateful tests
%%      (which use `run_commands' and `run_parallel_commands').
%%      Do not use directly. Instead, use the `TRACEFAIL' macro from `prop_tracer.hrl'.
-module(proper_tracer).
-behaviour(gen_server).

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
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/ms_transform.hrl").


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init({Module, TestProcessPid}) ->
  %% Set up a trace of the calling process (property test).
  %% We only trace `postcondition` calls because that's all we need to understand
  %% what was happening.
  erlang:trace(TestProcessPid, true, [call]),
  erlang:trace_pattern({Module, postcondition, 3}, dbg:fun2ms(fun(_) -> return_trace() end)),
  {ok, []}.

%% @hidden
handle_call(print_history, _, State) ->
  print_command_sequence(State),
  print_detailed_history(State),
  {reply, ok, State};
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info({trace, _, call, {_Module, Fun, Args}}, State) ->
  %% Push the function call to the state
  {noreply, [{Fun, Args, undefined} | State]};
handle_info({trace, _, return_from, {_Module, Fun, Arity}, Result}, State) ->
  State1 = case State of
    [{Fun, Args, undefined} | Rest] when length(Args) =:= Arity ->
      %% This return info corresponds to the pushed function call -> append return value
      [{Fun, Args, {ok, Result}} | Rest];
    Other -> Other
  end,
  {noreply, State1};
handle_info(_, State) ->
  {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

print_command_sequence(TracedCalls) ->
  io:format("Command sequence: ~n~n"),
  lists:foldr(
        fun
          ({postcondition, [_State, Command, _Result], PostConditionResult}, _) ->
            io:format("~W", [Command, 10]),
            %% Mark the failed postcondition
            Mark = case PostConditionResult of
              {ok, false} -> " <-- FAILED";
              _ -> ""
            end,
            io:format("~s~n", [Mark]),
            undefined;
          (_, _) -> undefined
        end,
        undefined,
        TracedCalls
      ).

print_detailed_history(TracedCalls) ->
  io:format("~n~nDetailed history: ~n~n"),
  io:format("~s~n~n", [string:copies("-", 60)]),
  lists:foldr(
        fun
          ({postcondition, [State, Command, CommandResult], Result}, _) ->
            PostconditionResult = case Result of
              {ok, X} -> io_lib:format("~p", [X]);
              undefined -> "undefined result"
            end,
            io:format("State: ~p~n~n", [State]),
            io:format("Command: ~p~n~n", [Command]),
            io:format("Result: ~p~n~n", [CommandResult]),
            io:format("Postcondition: ~s~n~n", [PostconditionResult]),
            io:format("~s~n~n", [string:copies("-", 60)]),
            undefined;
          (_, _) -> undefined
        end,
        undefined,
        TracedCalls
      ).
