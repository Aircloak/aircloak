%% @doc Utilities related to random numbers
-module(random_util).

%% API
-export([
  uniform/0,
  uniform/1,
  uniform_s/1,
  uniform_s/2
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Wraps random:uniform/0, but ensures that random
%%      has been seeded with a strong random number.
%%      The conditional seeding has an overhead of 3ns
%%      when the process has already been seeded.
%%
%%      The overhead of using random_util:uniform/0
%%      as compared to random:uniform/0 is 25ns,
%%      and I consider this to be worthwhile, considering
%%      the guarantee that the numbers are strongly seeded.
uniform() ->
  conditionally_seed(),
  random:uniform().

%% @doc Wraps random:uniform/1.
%%      See the documentation of `uniform/0' for
%%      performance characteristics
uniform(N) ->
  conditionally_seed(),
  random:uniform(N).

%% @doc A wrapper of random:uniform_s/1.
%%      It allows us to consistently use the
%%      random_util throughout our code whenever
%%      we need random numbers.
%%
%%      The overhead of using the random_util
%%      version over purely calling the version
%%      in the random library is negligible.
%%      Tests on my development machine yield
%%      a performance overhead in the 0.02ns
%%      range.
uniform_s(Seed) ->
  random:uniform_s(Seed).

%% @doc A wrapper of random:uniform_s/2.
%%      See the documentation of uniform_s/1
uniform_s(N, Seed) ->
  random:uniform_s(N, Seed).


%% -------------------------------------------------------------------
%% Private
%% -------------------------------------------------------------------

%% @doc Seeds the process random number generator
%%      with a strong random number using randomness
%%      from the OS in order to provide less predictable
%%      random number sequences.
conditionally_seed() ->
  case get(seeded_with_strong_random) of
    undefined ->
      <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
      random:seed({A,B,C}),
      put(seeded_with_strong_random, true);
    true ->
      ok
  end.


%% -------------------------------------------------------------------
%% Test
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

conditionally_seed_test() ->
  conditionally_seed(),
  ?assertNotEqual(undefined, get(random_seed)),
  Seed = get(random_seed),
  conditionally_seed(),
  ?assertEqual(Seed, get(random_seed)).

uniform_test_() ->
  [
    ?_assertNotEqual(uniform(), uniform()),
    ?_assertEqual(uniform_s({1,2,3}), uniform_s({1,2,3}))
  ].

-endif.
