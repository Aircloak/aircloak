%% @doc Diverse utility functions, used throughout the
%%      cloak-core application.
-module(cloak_util).

%% API
-export([
  host_from_node/1,
  join/2,
  stringify/1,
  binarify/1,
  node_connection_descriptor/1,
  destructure_parsed_json/1,
  index_of/2,
  timestamp_to_epoch/1,
  epoch_to_timestamp/1,
  timestamp_to_int/1,
  int_to_timestamp/1,
  timestamp_to_datetime/1,
  datetime_to_int/1,
  int_to_datetime/1,
  dedupe/1,
  all_visible_nodes/0,
  all_other_visible_nodes/0,
  randomize_list/1,
  chunks/2,
  chunked_foldl/4
]).

-include("user_data.hrl").

-export_type([
  deep_proplist/0
]).

-type deep_proplist_value() :: term() | [term()] | deep_proplist().
-type deep_proplist() :: [{term(), deep_proplist_value()}].

% We need the relative offset of the current epoch when converting SQL timestamps to integers.
% This is the value of "calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})".
-define(OS_TIMESTAMP_EPOCH, 62167219200).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Given a node name as returned
%%      by node/0 or nodes/0, this
%%      function returns the host part of
%%      the name or 127.0.0.1 for the local node
%%
%%      Example: given ``cloak@super-cloak31.aircloak.com''
%%        you will get returned ``super-cloak31.aircloak.com''
-spec host_from_node(node()) -> nonempty_string().
host_from_node(Nodename) when Nodename =:= node() -> "127.0.0.1";
host_from_node(Nodename) ->
  [_Node, Host] = re:split(atom_to_list(Nodename), "@",
      [{return, list}, {parts, 2}]),
  Host.

%% @doc Joins a list by concatenating each
%%      individual element with a separator.
%%      It returns the new list as an iolist
%%      without any flattening.
-spec join(iolist(), any()) -> iolist().
join([], _Separator) -> [];
join([LastItem], _Separator) -> LastItem;
join([Item | Rest], Separator) ->
  [Item, Separator, join(Rest, Separator)].

%% @doc Converts common erlang types to strings
-spec stringify(binary() | atom() | integer() | list()) -> list().
stringify(Binary) when is_binary(Binary) -> binary_to_list(Binary);
stringify(Atom) when is_atom(Atom) -> atom_to_list(Atom);
stringify(Number) when is_integer(Number) -> integer_to_list(Number);
stringify(String) when is_list(String) -> String.

%% @doc Converts common erlang types to binary
-spec binarify(binary() | atom() | integer() | list() | term()) -> binary().
binarify(Binary) when is_binary(Binary) -> Binary;
binarify(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1);
binarify(Number) when is_integer(Number) -> integer_to_binary(Number);
binarify(String) when is_list(String) -> list_to_binary(String);
binarify(Term) -> term_to_binary(Term).

%% @doc Given a node and its hostname, assembles a node
%%      descriptor that can be used to connect to the
%%      particular nodes Postgres database.
-spec node_connection_descriptor(node()) -> [{host, nonempty_string()},...].
-ifdef(TEST).
node_connection_descriptor(Node) ->
  [{host, host_from_node(Node)}, {node, Node}].
-else.
node_connection_descriptor(Node) ->
  [{host, host_from_node(Node)}].
-endif.

%% @doc Transforms deep mochijson2 structure into a plain property list.
%%      Essentially, this transforms `{struct, [...]}' into `[...]' in the
%%      entire hierarchy.
-spec destructure_parsed_json(mochijson2:json_object()) -> deep_proplist().
destructure_parsed_json({struct, Properties}) -> destructure_parsed_json(Properties);
destructure_parsed_json({Field, Value}) -> {Field, destructure_parsed_json(Value)};
destructure_parsed_json(List) when is_list(List) -> [destructure_parsed_json(Element) || Element <- List];
destructure_parsed_json(Value) -> Value.

%% @doc Returns the 1-based position of element in a list, or undefined if element is not found.
-spec index_of(term(), [term()]) -> pos_integer() | undefined.
index_of(Element, List) -> index_of(Element, 1, List).

%% @doc Converts Erlang timestamp to epoch time (number of seconds since midnight, 1.1.1970.)
-spec timestamp_to_epoch(erlang:timestamp()) -> non_neg_integer().
timestamp_to_epoch({MegaSec, Sec, _MicroSecond}) ->
  MegaSec * 1000000 + Sec.

%% @doc Converts epoch time to Erlang timestamp
-spec epoch_to_timestamp(non_neg_integer()) -> erlang:timestamp().
epoch_to_timestamp(Epoch) ->
  {Epoch div 1000000, Epoch rem 1000000, 0}.

%% @doc Converts Erlang timestamp to integer (number of microseconds since midnight, 1.1.1970.)
-spec timestamp_to_int(erlang:timestamp()) -> non_neg_integer().
timestamp_to_int({MegaSec, Sec, MicroSecond}) ->
  (MegaSec * 1000000 + Sec) * 1000000 + MicroSecond.

%% @doc Converts integer to Erlang timestamp
-spec int_to_timestamp(non_neg_integer()) -> erlang:timestamp().
int_to_timestamp(MicroSeconds) ->
  {MicroSeconds div 1000000000000, MicroSeconds div 1000000 rem 1000000, MicroSeconds rem 1000000}.

%% @doc Converts Erlang timestamp to format that can be used in SQL queries.
-spec timestamp_to_datetime(erlang:timestamp()) -> datetime().
timestamp_to_datetime({_, _, MicroSec} = Timestamp) ->
  {Date, {Hour, Minute, Second}} = calendar:now_to_universal_time(Timestamp),
  {Date, {Hour, Minute, Second + MicroSec / 1000000}}.

%% @doc Converts a datetime tuple to an integer timestamp.
-spec datetime_to_int(datetime()) -> non_neg_integer().
datetime_to_int({{Year, Month, Day}, {Hour, Min, Sec}}) when is_float(Sec) ->
    SecInt = trunc(Sec),
    MicroSec = round((Sec - SecInt) * 1000000),
    Seconds = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, SecInt}}),
    (Seconds - ?OS_TIMESTAMP_EPOCH) * 1000000 + MicroSec;
datetime_to_int({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    Seconds = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}),
    (Seconds - ?OS_TIMESTAMP_EPOCH) * 1000000.

%% @doc Converts an integer timestamp to a datetime tuple.
-spec int_to_datetime(non_neg_integer()) -> datetime().
int_to_datetime(Timestamp) ->
  timestamp_to_datetime(int_to_timestamp(Timestamp)).

%% @doc Returns a deduplicated version of a list preserving the original
%%      order of the list
-spec dedupe([any()]) -> [any()].
dedupe(Elements) ->
  {ReversedDedupedList,_} = lists:foldl(fun(Element, {AccList, DupeSet}=Acc) ->
        case sets:is_element(Element, DupeSet) of
          true -> Acc;
          false -> {[Element|AccList], sets:add_element(Element, DupeSet)}
        end
      end, {[], sets:new()}, Elements),
  lists:reverse(ReversedDedupedList).

%% @doc Returns a list of all nodes visible from the local node,
%%      including the local node itself.
-spec all_visible_nodes() -> [atom()].
all_visible_nodes() ->
  nodes([visible, this]).

%% @doc Returns all nodes that are visible from the local node,
%%      with the exception of the local node itself.
-spec all_other_visible_nodes() -> [atom()].
all_other_visible_nodes() ->
  nodes([visible]).

%% @doc Returns a list with the same elements that were in the
%%      original list, but in a shuffled order.
-spec randomize_list([A]) -> [A].
randomize_list(List) ->
  ZippedList = lists:zip([random_util:uniform() || _ <- lists:seq(1, length(List))], List),
  SortedZippedList = lists:keysort(1, ZippedList),
  [Element || {_Order, Element} <- SortedZippedList].


%% @doc Splits the list in n chunks of the given size. The last chunk is not padded.
-spec chunks([any()], pos_integer()) -> [[any()]].
chunks([], _) -> [];
chunks(List, ChunkSize) ->
  {Chunk, Rest} = take_chunk(List, ChunkSize),
  [Chunk | chunks(Rest, ChunkSize)].

%% @doc Executes the callback function for each chunk in the input list.
-spec chunked_foldl(fun(([any()], any()) -> any()), any(), [any()], pos_integer()) -> any().
chunked_foldl(_, Acc, [], _) -> Acc;
chunked_foldl(Callback, Acc, List, ChunkSize) ->
  {Chunk, Rest} = take_chunk(List, ChunkSize),
  Acc1 = Callback(Chunk, Acc),
  chunked_foldl(Callback, Acc1, Rest, ChunkSize).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

index_of(_, _, []) -> undefined;
index_of(Element, Pos, [Element | _]) -> Pos;
index_of(Element, Pos, [_ | Rest]) -> index_of(Element, Pos+1, Rest).

take_chunk([], _) -> {[], []};
take_chunk([El | Rest], 1) -> {[El], Rest};
take_chunk([El | Rest], N) when N > 1 ->
  {ChunkRest, Rest1} = take_chunk(Rest, N - 1),
  {[El | ChunkRest], Rest1}.


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

randomize_list_test() ->
  meck:new(random_util),
  RandomSequence = [
    0.1, 0.3, 0.2,
    0.3, 0.2, 0.1,
    0.2, 0.1, 0.3
  ],
  meck:sequence(random_util, uniform, 0, RandomSequence),
  List = [a, b, c],
  [?assertNotEqual(List, randomize_list(List)) || _ <- [1, 2, 3]],
  meck:validate(random_util),
  meck:unload().

dedupe_test_() ->
  [
    ?_assertEqual([1,2,3,4], dedupe([1,2,3,4])),
    ?_assertEqual([1,2,3,4], dedupe([1,1,2,1,3,1,4,1,4,3])),
    ?_assertEqual([], dedupe([]))
  ].

host_from_node_test_() ->
  [
    ?_assertEqual("Machine1", host_from_node('cloak1@Machine1')),
    ?_assertEqual("127.0.0.1", host_from_node(node()))
  ].

join_test_() ->
  [
    {"A single item should not be joined",
        ?_assertEqual("item", lists:flatten(join(["item"], ", ")))
    },
    {"Multiple items should be joined",
        ?_assertEqual("item1, item2", lists:flatten(join(["item1", "item2"], ", ")))
    },
    {"Should only join the outermost levels of iolists",
        ?_assertEqual("item1, item2", lists:flatten(join([["item1"], "item2"], ", ")))
    }
  ].

stringify_test_() ->
  [
    ?_assertEqual("hello", stringify("hello")),
    ?_assertEqual("hello", stringify(<<"hello">>)),
    ?_assertEqual("hello", stringify("hello")),
    ?_assertEqual("1", stringify(1))
  ].


destructure_test_() ->
  [
    ?_assertEqual(1, destructure_parsed_json(1)),
    ?_assertEqual([1], destructure_parsed_json([1])),
    ?_assertEqual([{<<"a">>, 1}], destructure_parsed_json({struct, [{<<"a">>, 1}]})),
    ?_assertEqual(
          [1, [{<<"a">>, 1}], 2],
          destructure_parsed_json([1, {struct, [{<<"a">>, 1}]}, 2])
        )
  ].

index_of_test_() ->
  [
    ?_assertEqual(1, index_of(a, [a,b,c])),
    ?_assertEqual(2, index_of(b, [a,b,c])),
    ?_assertEqual(3, index_of(c, [a,b,c])),
    ?_assertEqual(undefined, index_of(d, [a,b,c])),
    ?_assertEqual(undefined, index_of(a, []))
  ].

epoch_conversion_test() ->
  ZeroTimestamp = epoch_to_timestamp(0),
  ?assertEqual({{1970,1,1}, {0,0,0}}, calendar:now_to_universal_time(ZeroTimestamp)),
  ?assertEqual(timestamp_to_epoch(ZeroTimestamp), 0),
  Now = {MegaSec, Sec, _} = os:timestamp(),
  ?assertEqual({MegaSec, Sec, 0}, epoch_to_timestamp(timestamp_to_epoch(Now))).

timestamp_to_datetime_test() ->
  ZeroTimestamp = {Mega, Sec, _} = epoch_to_timestamp(0),
  ?assertEqual({{1970,1,1}, {0,0,0.0}}, timestamp_to_datetime(ZeroTimestamp)),
  ?assertEqual({{1970,1,1}, {0,0,1.5}}, timestamp_to_datetime({Mega, Sec, 1500000})).

chunk_size_test_() ->
  [
    ?_assertEqual([], chunks([], 3)),
    ?_assertEqual([[1], [2], [3]], chunks([1, 2, 3], 1)),
    ?_assertEqual([[1, 2]], chunks([1, 2], 3)),
    ?_assertEqual([[1, 2, 3]], chunks([1, 2, 3], 3)),
    ?_assertEqual([[1, 2, 3], [4]], chunks([1, 2, 3, 4], 3)),
    ?_assertEqual([[1, 2, 3], [4, 5, 6], [7, 8]], chunks(lists:seq(1, 8), 3)),
    ?_assertEqual([[1, 2, 3], [4, 5, 6], [7, 8, 9]], chunks(lists:seq(1, 9), 3))
  ].

folded_chunks(List, ChunkSize) ->
  lists:reverse(chunked_foldl(
        fun(Chunk, Acc) -> [Chunk | Acc] end,
        [],
        List,
        ChunkSize
      )).

chunk_foldl_test_() ->
  [
    ?_assertEqual([], folded_chunks([], 3)),
    ?_assertEqual([[1], [2], [3]], folded_chunks([1, 2, 3], 1)),
    ?_assertEqual([[1, 2]], folded_chunks([1, 2], 3)),
    ?_assertEqual([[1, 2, 3]], folded_chunks([1, 2, 3], 3)),
    ?_assertEqual([[1, 2, 3], [4]], folded_chunks([1, 2, 3, 4], 3)),
    ?_assertEqual([[1, 2, 3], [4, 5, 6], [7, 8]], folded_chunks(lists:seq(1, 8), 3)),
    ?_assertEqual([[1, 2, 3], [4, 5, 6], [7, 8, 9]], folded_chunks(lists:seq(1, 9), 3))
  ].

-endif.
