% Various helpers that can be used in EUnit tests.
%
% To use these macros, add the following line to your test section:
%     -include("eunit_helpers.hrl").

% Tests that a message has been sent to this process. On success returns the
% received message so it can be further analyzed, if needed.
-define(assertReceived(Pattern, Timeout),
  (fun() ->
    receive Pattern = ReceivedMessage -> ReceivedMessage
    after Timeout ->
      erlang:error({
        message_not_received,
        [
          {module, ?MODULE},
          {line, ?LINE},
          {pattern, (??Pattern)},
          {message_queue,
            % This is wrapped inside a lambda to avoid polluting the context
            % with a local Flush variable.
            (fun() ->
              % Simple self-recursive lambda that flushes the message queue.
              Flush = fun(F) -> receive M -> [M | F(F)] after 0 -> [] end end,
              Flush(Flush)
            end)()
          }
        ]
      })
    end
  end)()
).

% Tests that a message has not been sent to this process.
-define(assertNotReceived(Pattern, Timeout),
  begin
    receive Pattern ->
      erlang:error({
        message_received, [
          {module, ?MODULE},
          {line, ?LINE},
          {pattern, (??Pattern)}
        ]
      })
    after Timeout -> ok
    end
  end
).
