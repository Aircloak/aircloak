%% @doc Returns first k values from the unordered list. Elements are compared using standard comparison
%%      operators. The resulting list is in the ascending order.
%%      The implementation is based on the
%%      <a target="_blank" href="http://stackoverflow.com/questions/4956593/optimal-algorithm-for-returning-top-k-values-from-an-array-of-length-n">min-heap approach</a>.
%%      Some quick measuring on a million elements list, with k=100, shows an improvement of ~75% compared to
%%      lists:sort.
%%
-module(k_select).

%% API
-export([
  get/2
]).

-include("cloak.hrl").

-type element() :: term().

-record(state, {
  k :: non_neg_integer(),
  processed=0 :: non_neg_integer(),
  collected=gb_trees:empty() :: gb_trees:gb_tree(),
  smallest :: undefined | {smallest, element()}
}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec get(non_neg_integer(), [element()]) -> [element()].
%% @doc Gets largest k elements from the list.
get(0, _) -> [];
get(K, List) -> process(List, #state{k=K}).

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

process([], #state{collected=Collected}) ->
  [Element || {{Element, _}, _} <- gb_trees:to_list(Collected)];
process([Element | Rest], State) ->
  State1 = process_element(Element, State),
  process(Rest, State1#state{processed=State1#state.processed + 1}).

process_element(Element, State) ->
  case gb_trees:size(State#state.collected) >= State#state.k of
    false ->
      % We didn't yet collect K elements -> just append to collected tree
      State#state{collected = add_to_collected(Element, State)};
    true ->
      % We collected K elements -> compare with the smallest one
      case Element =< smallest_collected(State) of
        true ->
          % Element is smaller than any of K collected -> just continue
          State;
        false ->
          % Element fits in the K collected -> add it and compute new smallest element. The second
          % part is an optimization so we avoid calling gb_trees:take_smallest for every input element.
          % With this approach we are recalculating the smallest element only when we change the collection,
          % which on a list of million elements reduces calls to take_smallest by two orders of magnitude.
          Collected1 = add_to_collected(Element, State),
          {{Smallest2, _}, _, Collected2} = gb_trees:take_smallest(Collected1),
          State#state{smallest={smallest, Smallest2}, collected=Collected2}
      end
  end.

smallest_collected(#state{smallest = {smallest, Smallest}}) -> Smallest;
smallest_collected(#state{collected = Collected}) ->
  {{Smallest, _}, _, _} = gb_trees:take_smallest(Collected),
  Smallest.

add_to_collected(Element, #state{processed=Processed, collected=Collected}) ->
  % We use {Element, Processed} as the id to ensure uniqueness, and keep result of > comparison correct.
  % This ensures the proper ordering in the tree structure.
  gb_trees:insert({Element, Processed}, undefined, Collected).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

edge_cases_test() ->
  ?assertEqual([], get(0, [])),
  ?assertEqual([], get(0, [1,2,3,4,5])),
  ?assertEqual([], get(1, [])),
  ?assertEqual([1], get(10, [1])),
  ?assertEqual([1,2,3], lists:sort(get(10, [1,2,3]))).

standard_test() ->
  ?assertEqual([2,3], lists:sort(get(2, [1,2,3]))),
  ?assertEqual([2,3], lists:sort(get(2, [0,0,0,0,1,2,3]))),
  ?assertEqual([2,3], lists:sort(get(2, [1,2,3,0,0,0,0]))),
  ?assertEqual([2,3], lists:sort(get(2, [0,0,1,0,2,0,3,0,0]))),
  ?assertEqual([1,2,3], lists:sort(get(3, [1,2,3]))),
  ?assertEqual([1,2,3], lists:sort(get(3, [0,0,0,0,1,2,3]))),
  ?assertEqual([1,2,3], lists:sort(get(3, [1,2,3,0,0,0,0]))),
  ?assertEqual([1,2,3], lists:sort(get(3, [0,0,1,0,2,0,3,0,0]))).

run_seq(K, N) ->
  ?assertEqual(lists:seq(N-K+1, N), lists:sort(get(K, lists:seq(1, N)))),
  ?assertEqual(lists:seq(N-K+1, N), lists:sort(get(K, lists:reverse(lists:seq(1, N))))).

seq_test() ->
  [
    run_seq(K, N) ||
      K <- lists:seq(1, 10),
      N <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100],
      N > K
  ].

random_test() ->
  TestFun = fun() ->
    N = random_util:uniform(100) + 950,
    K = random_util:uniform(100) + 50,
    Input = [random_util:uniform(500) || _ <- lists:seq(1, N)],
    Expected = lists:reverse(lists:sublist(lists:reverse(lists:sort(Input)), K)),
    ?assertEqual(Expected, lists:sort(get(K, Input)))
  end,
  [TestFun() || _ <- lists:seq(1,100)].

-endif.
