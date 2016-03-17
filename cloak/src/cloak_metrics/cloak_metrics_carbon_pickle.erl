%% @doc This is a simplified pickle serializer that takes statistics as input
%%      and produces format suitable for Graphite pickle version.<br/>
%%      Graphite pickle is not very well documented, so this implementation
%%      is based on the StatsD fork. A short format description can be found
%%      <a target="_blank" href="https://github.com/aronatkins/statsd/blob/2c6b83b0d9bcb19466b3da419bb6c2cc06f9ccfb/docs/graphite_pickle.md">here</a>,
%%      with reference implementation in JavaScript <a target="_blank" href="https://github.com/aronatkins/statsd/blob/2c6b83b0d9bcb19466b3da419bb6c2cc06f9ccfb/backends/graphite.js#L95-L147">here</a>.

-module(cloak_metrics_carbon_pickle).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([encode/1]).

%% Types
-type input() :: nonempty_list({Name::binary(), {Timestamp::pos_integer(), Value::number()}}).

%% Special characters according to pickle protocol
-define(MARK, "(").
-define(STOP, ".").
-define(LONG, "L").
-define(STRING, "S").
-define(APPEND, "a").
-define(LIST, "l").
-define(TUPLE, "t").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec encode(input()) -> binary().
%% @doc Encodes input data point to Graphite compliant pickle format.
encode(Data) ->
  Payload = iolist_to_binary(convert(Data)),
  Size = byte_size(Payload),
  <<
    Size:32/big-integer,
    Payload/binary
  >>.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

convert([_ | _] = DataPoints) ->
  [?MARK, ?LIST, lists:map(fun(DataPoint) -> [convert(DataPoint), ?APPEND] end, DataPoints), ?STOP];

convert({Name, {Timestamp, Value}}) ->
  to_pickle({Name, {Timestamp, valid_value(Value)}}).

valid_value(Value) when is_integer(Value) -> integer_to_binary(Value);
valid_value(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 2}]).

to_pickle(Tuple) when is_tuple(Tuple) ->
  [?MARK, lists:map(fun to_pickle/1, tuple_to_list(Tuple)), ?TUPLE];

to_pickle(String) when is_binary(String) ->
  [?STRING, "'", String, "'\n"];

to_pickle(Int) when is_integer(Int) ->
  [?LONG, integer_to_list(Int), "L\n"].


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

  test_pickle(Expected, Input) ->
    << Size:32/big-integer, Contents/binary >> = encode(Input),
    ?assertEqual(Expected, Contents),
    ?assertEqual(byte_size(Expected), Size).

  pickle_test() ->
    test_pickle(<<"(l(S'name'\n(L1L\nS'2'\ntta.">>, [{<<"name">>, {1, 2}}]),
    test_pickle(<<"(l(S'name'\n(L1L\nS'3.14'\ntta.">>, [{<<"name">>, {1, 3.14}}]).

-endif.
