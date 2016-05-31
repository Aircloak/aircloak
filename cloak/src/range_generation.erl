%% @doc This module contains the algorithm to generate the ranges for
%%      a given property-label and property-string.
%%      A high-level description of the algorithm is in the Wiki.
-module(range_generation).

-export([
  generate/3
]).

-include("cloak.hrl").

-type noise_function() :: fun((integer()) -> integer()).
-type generated_range() :: {From :: integer() | neg_infinite, To :: integer() | infinite, Count :: non_neg_integer()}.


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% The interface to generate the ranges.
%% Given is a list of value-count pairs.  Returned is a list of ranges
%% including the real counts in those ranges and the noisy counts (which
%% have to be greater than T2 according to our algorithm).  In this code
%% T2 and DeltaT2 are arbitrary parameters of type integer resp. function
%% mapping an integer to an integer.  DeltaT2 is a function adding noise
%% to the given value.  T2 is a lower bound for the minimum size of the
%% range after adding noise.  We use this value to determine the ranges
%% and which range are returned at all.
%% The list of value-count pairs has to be sorted.
-spec generate([{integer(), pos_integer()}], noise_function(), pos_integer()) -> [generated_range()].
generate([], _, _) ->
  [];
generate(Values, DeltaT2, T2) ->
  {Min, Max} = initial_range(Values),
  InitialRanges = build_initial_ranges([], Max-Min, Min),
  CollectedRanges = sweep_through_values([], InitialRanges, Max, Values, DeltaT2, T2),
  fillin_gaps(CollectedRanges, Values, DeltaT2, T2).


%% -------------------------------------------------------------------
%% Find the initial ranges
%% -------------------------------------------------------------------

%% Find the initial range to start with (use doubling).
-spec initial_range([{integer(), non_neg_integer()}]) -> {neg_integer(), pos_integer()}.
initial_range([{Min, _}|_]=List) ->
  {Max, _} = lists:last(List),
  AbsMax = max(-Min, Max + 1),
  initial_range_abs(1, AbsMax).

%% Find the initial range to start with given the maximum of the absolute values.
initial_range_abs(N, AbsMax) when AbsMax =< N ->
  {-N, N};
initial_range_abs(N, AbsMax) ->
  initial_range_abs(N*2, AbsMax).


%% -------------------------------------------------------------------
%% Sweep algorithm
%% -------------------------------------------------------------------

%% Build the initial set of ranges.
%% Here we ensure that the longest ranges are on the bottom of the generated list.
build_initial_ranges(Acc, 0, _Min) ->
  Acc;
build_initial_ranges(Acc, N, Min) ->
  build_initial_ranges([{N, Min, Min+N, 0}|Acc], N div 2, Min).

%% Sweep through the values and generate the corresponding ranges.
%% Ranges are represented as tuples of the form
%%    {RangeSize, Start, End, AccumulatedUsers}.
%% We generate the ranges down to the maximum number required (thus having a size
%% of one in an initial step and then sweep through the values incrementing the
%% AccumulatedUsers as required.
sweep_through_values(Acc1, Ranges, Max, [], DeltaT2, T2) ->
  {Acc2, _} = process_ranges(Acc1, Ranges, Max, Max+1, 0, DeltaT2, T2),
  Acc2;
sweep_through_values(Acc1, Ranges1, Max, [{Value, Count}|Rest], DeltaT2, T2) ->
  {Acc2, Ranges2} = process_ranges(Acc1, Ranges1, Max, Value, Count, DeltaT2, T2),
  sweep_through_values(Acc2, Ranges2, Max, Rest, DeltaT2, T2).

%% Process the next value for the sweep-algorithm.  If ranges get closed, we check
%% if we want to create them in the output stream.  We start checking that with the
%% smallest ranges first, as we greedily try to find small ranges with enough users.
process_ranges(Acc, [], _GMax, _Value, _Count, _DeltaT2, _T2) ->
  {Acc, []};
process_ranges(Acc1, [{Size, Min, Max, RCount}|Rest1], GMax, Value, Count, DeltaT2, T2)
    when Value < Min ->
  {Acc2, Rest2} = process_ranges(Acc1, Rest1, GMax, Value, Count, DeltaT2, T2),
  {Acc2, [{Size, Min, Max, RCount}|Rest2]};
process_ranges(Acc1, [{Size, Min, Max, RCount}|Rest1], GMax, Value, Count, DeltaT2, T2)
    when Value < Max ->
  {Acc2, Rest2} = process_ranges(Acc1, Rest1, GMax, Value, Count, DeltaT2, T2),
  {Acc2, [{Size, Min, Max, RCount+Count}|Rest2]};
process_ranges(Acc1, [{Size, Min, Max, RCount}|Rest1]=Ranges1, GMax, Value, Count, DeltaT2, T2)
    when Max =< Value ->
  case DeltaT2(RCount) of
    NoiseCount when NoiseCount > T2 ->
      Acc2 = [{Min, Max, RCount}|Acc1],
      Ranges2 = stepup_ranges(Value, Max, GMax, Ranges1),
      process_ranges(Acc2, Ranges2, GMax, Value, Count, DeltaT2, T2);
    _Otherwise ->
      RangePrefix2 = stepup_ranges(Value, Max, GMax, [{Size, Min, Max, RCount}]),
      {Acc2, Rest2} = process_ranges(Acc1, RangePrefix2 ++ Rest1, GMax, Value, Count, DeltaT2, T2),
      {Acc2, Rest2}
  end.

%% Increase the ranges until they fit in the defined scheme.
%% We try to move the frames as far as possible.  If we move a frame after the maximum value in the input
%% (GMax), we remove that frame.  Thus we won't consider ranges of that size anymore.
%% REMARK(XXX): I think we might be possible to prove, that if a frame of size 2^k is moved past GMax, then
%% all frames 2^k' for k'>k get moved past GMax in the same step and thus can be removed, too.  As I haven't
%% proved this and this code is critical, I don't want to apply that optimization now.  This might speed up
%% things in practice, so if we ever have performance problems due to the range generation, someone might want
%% to look into that.
stepup_ranges(_Value, _FoundMax, _GMax, []) ->
  [];
stepup_ranges(Value, FoundMax, GMax, [{Size, Min, Max, _RCount}|Rest])
    when Min < FoundMax andalso FoundMax =< Max ->
  IncrFactor = max(1, (Value - Min) div Size),
  % the list comprehension is used to filter out ranges which are after the maximum value
  [{Size, NMin, NMax, 0} ||
    {NMin, NMax} <- [{Min + IncrFactor*Size, Max + IncrFactor*Size}], NMin < GMax
  ] ++ stepup_ranges(Value, FoundMax, GMax, Rest);
stepup_ranges(Value, FoundMax, GMax, [{Size, Min, Max, RCount}|Rest]) ->
  [{Size, Min, Max, RCount}|stepup_ranges(Value, FoundMax, GMax, Rest)].


%% -------------------------------------------------------------------
%% Fill in the gaps
%% -------------------------------------------------------------------

%% If we have no range, we have a special case.  The full range is already checked,
%% so do not report anything.
fillin_gaps([], _Values, _DeltaT2, _T2) ->
  [];
fillin_gaps(CollectedRanges, Values, DeltaT2, T2) ->
  RevRanges = lists:reverse(CollectedRanges),
  Min = -(1 bsl 63), % represents - infinity, works as we have 64-bit integers
  fillin_gaps(RevRanges, Min, 0, RevRanges, Values, DeltaT2, T2).

%% Fill in the gaps given a lower bound and a current count.
fillin_gaps(Acc, Min, Count, _Ranges, [], DeltaT2, T2) ->
  check_add_range(Acc, Min, infinite, Count, DeltaT2, T2);
fillin_gaps(Acc, Min, Count, Ranges, [{Value, _}|RestValues], DeltaT2, T2)
    when Value < Min ->
  fillin_gaps(Acc, Min, Count, Ranges, RestValues, DeltaT2, T2);
fillin_gaps(Acc, Min, Count, [{RMin, _RMax, _}|_]=Ranges,
    [{Value, VCount}|RestValues], DeltaT2, T2) when Value < RMin ->
  fillin_gaps(Acc, Min, Count+VCount, Ranges, RestValues, DeltaT2, T2);
fillin_gaps(Acc, Min, Count, [], [{_, VCount}|RestValues], DeltaT2, T2) ->
  fillin_gaps(Acc, Min, Count+VCount, [], RestValues, DeltaT2, T2);
fillin_gaps(Acc, Min, Count, [{RMin, RMax, _}|RestRanges], Values, DeltaT2, T2) ->
  fillin_gaps(check_add_range(Acc, Min, RMin, Count, DeltaT2, T2), RMax, 0, RestRanges, Values, DeltaT2, T2).

%% Check if we add a range (any range with zero users won't come past anonymization, so do not include it).
check_add_range(Acc, Min, Max, Count, DeltaT2, T2) when Count > 0 ->
  case DeltaT2(Count) of
    NoiseCount when NoiseCount > T2 ->
      [{real_min(Min), Max, Count}|Acc];
    _Otherwise ->
      Acc
  end;
check_add_range(Acc, _Min, _Max, _Count, _DeltaT2, _T2) ->
  Acc.

%% Real minimum value
real_min(Min) when Min == -(1 bsl 63) -> neg_infinite;
real_min(Min) -> Min.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% Trivial noise function.
deltaT2(N) -> N.

map_values(Values, InitialUser, UsersPerSet, UsersOverlap) ->
  {NewValues, _} = lists:foldl(
    fun({Value, Count}, {ValuesAcc, UserIdAcc}) ->
      NewValue = {Value, Count},
      {[NewValue | ValuesAcc], UserIdAcc + UsersPerSet - UsersOverlap}
    end,
    {[], InitialUser},
    Values
  ),
  lists:reverse(NewValues).

uniform_distribution_test(N, K) ->
  Values = map_values([{I, 1} || I <- lists:seq(-N, N-1)], -N, 1, 0),
  ResultRanges = [{I*K-N, I*K-N+K, K}
    || I <- lists:seq(0, 2 * (N div K) - 1)],
  ?assertEqual(ResultRanges, generate(Values, fun deltaT2/1, K-1)).

uniform_distribution1_test() ->
  uniform_distribution_test(128, 4).

uniform_distribution2_test() ->
  uniform_distribution_test(128, 2).

uniform_distribution3_test() ->
  uniform_distribution_test(128, 128).

peek_distribution1_test() ->
  Values = map_values([{-17, 128},{17, 128}], 0, 3, 0),
  ResultRanges127 = [{-17,-16,128}, {17,18,128}],
  ResultRanges255 = [{-32, 32, 256}],
  ?assertEqual(ResultRanges127, generate(Values, fun deltaT2/1, 127)),
  ?assertEqual(ResultRanges255, generate(Values, fun deltaT2/1, 255)).

peek_distribution2_test() ->
  Values = map_values([{-32, 64}, {-18, 64}, {-17, 128}, {-15, 64}, {15, 64}, {17, 128}, {18, 64}, {32, 64}],
      1000, 3, 0),
  ResultRanges = [
    {neg_infinite, -17, 128},
    {-17, -16, 128},
    {-16, 17, 128},
    {17, 18, 128},
    {18, infinite, 128}
  ],
  ?assertEqual(lists:sort(ResultRanges), lists:sort(generate(Values, fun deltaT2/1, 127))).

-endif.
