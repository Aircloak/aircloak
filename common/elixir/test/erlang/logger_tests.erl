-module(logger_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("elixir_logger.hrl").

-define(CAPTURE_LOG(Level, Message),
    'Elixir.ExUnit.CaptureLog':capture_log(fun() -> ?Level(Message) end)).

logger_test_() ->
  {
    setup,
    fun() ->
      PreviousLoggerLevel = 'Elixir.Logger':level(),
      'Elixir.Logger':configure([{level, debug}]),
      'Elixir.Logger':configure_backend(console, [{colors, [{enabled, false}]}]),
      PreviousLoggerLevel
    end,
    fun(PreviousLoggerLevel) ->
      'Elixir.Logger':configure([{level, PreviousLoggerLevel}]),
      'Elixir.Logger':configure_backend(console, [{colors, [{enabled, 'Elixir.IO.ANSI':'enabled?'()}]}])
    end,
    [
      ?_assertEqual(<<"[debug] foo\n">>, ?CAPTURE_LOG(DEBUG, "foo")),
      ?_assertEqual(<<"[info] foo\n">>, ?CAPTURE_LOG(INFO, "foo")),
      ?_assertEqual(<<"[warn] foo\n">>, ?CAPTURE_LOG(WARN, "foo")),
      ?_assertEqual(<<"[error] foo\n">>, ?CAPTURE_LOG(ERROR, "foo"))
    ]
  }.

-endif.
