%% This macro can be used in a stateful test instead of the ?TRAPEXIT/?WHENFAIL combination.
%% The macro will start a process which traces postcondition calls and prints the debug
%% output if the test fails. See full_* tests for example usage.
-define(TRACEFAIL(Commands, TestCode, Prop),
      ?TRAPEXIT(
        begin
          {ok, Pid} = gen_server:start_link(proper_tracer, {?MODULE, self()}, []),
          {History, State, Result} = TestCode,
          ?WHENFAIL(
                begin
                  io:format("~n~s~n~n", [string:copies("=", 80)]),
                  io:format("Result: ~p~n~n", [Result]),
                  gen_server:call(Pid, print_history, infinity),
                  io:format("PropEr state: ~p~n~n", [State]),
                  io:format("PropEr commands: ~p~n~n", [Commands]),
                  io:format("PropEr history: ~p~n~n", [History]),
                  io:format("~s~n~n", [string:copies("=", 80)])
                end,
                Prop
              )
        end
      )
    ).
