%% @doc Data structure that collect individula datapoints (numbers) and
%%      can produce number of occurences of each discrete datapoint.
-module(cloak_metrics_datapoints).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
  new/0, new/1, add/2, occurences/1
]).

%% Types
-type datapoints() :: gb_trees:tree(number(), pos_integer()).

-export_type([datapoints/0]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec new() -> datapoints().
%% @doc Creates new instance.
new() -> gb_trees:empty().

-spec new([Value::number()]) -> datapoints().
%% @doc Creates new instance.
new(Values) -> lists:foldl(fun add/2, new(), Values).

-spec add(Value::number(), datapoints()) -> datapoints().
%% @doc Adds a value to the structure.
add(Value, Datapoints) ->
  case gb_trees:lookup(Value, Datapoints) of
    none ->
      gb_trees:insert(Value, 1, Datapoints);
    {value, Count} ->
      gb_trees:update(Value, Count + 1, Datapoints)
  end.

-spec occurences(datapoints()) -> [{Value::number(), Count::pos_integer()}].
%% @doc Produces a list of tuples, with each tuple containing a discrete
%%      datapoint and corresponding number of occurences. The list is sorted
%%      on datapoint values in ascending order.
occurences(Datapoints) -> gb_trees:to_list(Datapoints).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

empty_test() ->
  ?assertEqual([], occurences(new())).

sort_test() ->
  ?assertEqual([], occurences(new())),
  ?assertEqual([{1, 1}, {2, 1}, {3, 1}], occurences(new([3, 2, 1]))),
  ?assertEqual([{1, 3}, {2, 1}, {3, 1}, {4, 1}], occurences(new([4, 1, 2, 1, 1, 3]))).

-endif.
