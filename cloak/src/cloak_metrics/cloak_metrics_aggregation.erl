%% @doc Performs aggregation of bucketed metrics (usually histograms), producing
%%      metrics such as average, median, and percentiles.
-module(cloak_metrics_aggregation).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
  aggregate/1
]).

%% Types
-type metric() :: {metric_type(), Value :: number()}.
-type metric_type() :: average | median | upper_75 | upper_90 | upper_99.

%% Internal
-define(STAT_TYPES, [average, {percentile, 50}, {percentile, 75}, {percentile, 90}, {percentile, 99}]).

-record(stats, {
  size,
  position,
  average,
  median,
  upper_75,
  upper_90,
  upper_99
}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec aggregate([cloak_metrics:bucket()]) -> [metric()].
%% @doc Produces metrics for given buckets.
aggregate([]) -> [];
aggregate(Buckets) ->
  case sort_buckets(Buckets) of
    [] -> [];
    SortedBuckets ->
      Stats = lists:foldl(fun visit/2, init(SortedBuckets), SortedBuckets),
      to_proplist(Stats)
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

sort_buckets(Buckets) ->
  lists:keysort(1, normalize_buckets(Buckets)).

% TODO: this is a quick hack. The bucketization function may return
% neg_infinite or infinite as bucket bounds, so we choose some numerical
% values instead.
normalize_buckets(Buckets) ->
  lists:filtermap(
        fun
          (Bucket) when element(3, Bucket) =< 0 -> false;
          (Bucket) when element(1, Bucket) =:= neg_infinite -> {true, setelement(1, Bucket, 0)};
          (Bucket) when element(2, Bucket) =:= infinite -> {true, setelement(2, Bucket, element(1, Bucket) * 2)};
          (Bucket) -> {true, Bucket}
        end,
        Buckets
      ).

total_size(Buckets) ->
  lists:foldl(fun(Bucket, Size) -> Size + bucket_size(Bucket) end, 0, Buckets).

bucket_size({_, _, NoisyCount}) -> NoisyCount.

init(Buckets) ->
  lists:foldl(fun init/2, #stats{size=total_size(Buckets), position=0}, ?STAT_TYPES).

init(average, Stats) -> Stats#stats{average = 0.0};
init(_, Stats) -> Stats.

visit({_, _, NoisyCount} = Bucket, #stats{position=Position} = Stats) ->
  lists:foldl(
        fun(StatType, StatsAcc) -> visit(StatType, Bucket, StatsAcc) end,
        Stats#stats{position=Position + NoisyCount},
        ?STAT_TYPES
      ).

% This is implemented as a macro, because this module works with records, which
% have no support for run-time dynamic field manipulation. Since we compute
% different percentiles, and store their values to different record fields,
% we have to use macro. Once we move to Erlang R17, we can use maps instead
% of records, and thus turn this into a proper function.
-define(visit_percentile(Perc, Field),
  % This clause is invoked per each percentile, once we're in a bucket that
  % contains the percentile's upper bound. The function assumes linear
  % distribution of values, and takes one from the bucket, depending on its
  % relative size (to the total size), and remaining percentage of elements
  % we need to reach full percentile.
  visit(
    {percentile, Perc}, {From, To, NoisyCount},
    #stats{Field=undefined, position=Position, size=Size} = Stats
  ) when Position/Size >= Perc/100 ->
    ThisBucket = NoisyCount / Size,
    StillNeeded = Perc/100 - (Position - NoisyCount)/Size,
    ApproximatePosition = StillNeeded / ThisBucket,
    ApproximateValue = (((To - 1) - From) * ApproximatePosition) + From,
    TwoDecimalsApproximation = round(ApproximateValue * 100) / 100,
    Stats#stats{Field=TwoDecimalsApproximation}
).

visit(average, {From, To, NoisyCount}, #stats{average=Average, size=Size} = Stats) ->
  Stats#stats{average=Average+((NoisyCount/Size)*(((To - 1)+From) / 2))};
?visit_percentile(50, median);
?visit_percentile(75, upper_75);
?visit_percentile(90, upper_90);
?visit_percentile(99, upper_99);
visit({percentile, _}, _, Stats) -> Stats.

to_proplist(Stats) ->
  lists:foldl(
        fun(StatType, Acc) -> [property(StatType, Stats) | Acc] end,
        [], ?STAT_TYPES
      ).

% See comment for visit_percentile macro above.
-define(property_percentile(Perc, Field),
  property({percentile, Perc}, #stats{Field=Value}) -> {Field, Value}
).

property(average, #stats{average=Average}) -> {average, Average};
?property_percentile(50, median);
?property_percentile(75, upper_75);
?property_percentile(90, upper_90);
?property_percentile(99, upper_99).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

calc_metric(Type, Buckets) ->
  Res = aggregate(Buckets),
  proplists:get_value(Type, Res).

average_test() ->
  ?assertEqual(undefined, calc_metric(average, [])),
  ?assertEqual(undefined, calc_metric(average, [{0, 3, 0}])),
  ?assertEqual(1.0, calc_metric(average, [{0, 3, 1}])),
  ?assertEqual(1.0, calc_metric(average, [{neg_infinite, 3, 1}])),
  ?assertEqual(1.0, calc_metric(average, [{1, infinite, 1}])),
  ?assertEqual(3.0, calc_metric(average, [{0, 3, 1}, {3, 6, 2}])).

median_test() ->
  ?assertEqual(undefined, calc_metric(median, [])),
  ?assertEqual(undefined, calc_metric(median, [{0, 3, 0}])),
  ?assertEqual(1.0, calc_metric(median, [{0, 3, 1}])),
  ?assertEqual(3.5, calc_metric(median, [{0, 3, 1}, {3, 6, 2}])),
  ?assertEqual(3.67, calc_metric(median, [{0, 3, 1}, {3, 6, 3}])),
  ?assertEqual(3.0, calc_metric(median, [{0, 3, 999}, {3, 6, 1000}])),
  ?assertEqual(1.0, calc_metric(median, [{0, 3, 1000}, {3, 6, 1}])),
  ?assertEqual(4.0, calc_metric(median, [{0, 3, 1}, {3, 6, 1000}])).

upper_75_test() ->
  ?assertEqual(undefined, calc_metric(upper_75, [])),
  ?assertEqual(undefined, calc_metric(upper_75, [{0, 3, 0}])),
  ?assertEqual(0.75, calc_metric(upper_75, [{0, 2, 1}])),
  ?assertEqual(2.5, calc_metric(upper_75, [{0, 2, 1}, {2, 4, 1}])),
  ?assertEqual(2.63, calc_metric(upper_75, [{0, 2, 1}, {2, 4, 2}])),
  ?assertEqual(0.94, calc_metric(upper_75, [{0, 2, 8}, {20, 40, 2}])),
  ?assertEqual(0.75, calc_metric(upper_75, [{0, 2, 1000}, {2, 4, 1}])),
  ?assertEqual(2.75, calc_metric(upper_75, [{0, 2, 1}, {2, 4, 1000}])).

upper_90_test() ->
  ?assertEqual(undefined, calc_metric(upper_90, [])),
  ?assertEqual(undefined, calc_metric(upper_90, [{0, 3, 0}])),
  ?assertEqual(0.9, calc_metric(upper_90, [{0, 2, 1}])),
  ?assertEqual(2.8, calc_metric(upper_90, [{0, 2, 1}, {2, 4, 1}])),
  ?assertEqual(2.85, calc_metric(upper_90, [{0, 2, 1}, {2, 4, 2}])),
  ?assertEqual(1.0, calc_metric(upper_90, [{0, 2, 9}, {20, 40, 1}])),
  ?assertEqual(0.9, calc_metric(upper_90, [{0, 2, 1000}, {2, 4, 1}])),
  ?assertEqual(2.9, calc_metric(upper_90, [{0, 2, 1}, {2, 4, 1000}])).

upper_99_test() ->
  ?assertEqual(undefined, calc_metric(upper_99, [])),
  ?assertEqual(undefined, calc_metric(upper_99, [{0, 3, 0}])),
  ?assertEqual(0.99, calc_metric(upper_99, [{0, 2, 1}])),
  ?assertEqual(2.98, calc_metric(upper_99, [{0, 2, 1}, {2, 4, 1}])),
  ?assertEqual(2.99, calc_metric(upper_99, [{0, 2, 1}, {2, 4, 2}])),
  ?assertEqual(1.0, calc_metric(upper_99, [{0, 2, 99}, {1000, 1002, 1}])),
  ?assertEqual(0.99, calc_metric(upper_99, [{0, 2, 1000}, {2, 4, 1}])),
  ?assertEqual(2.99, calc_metric(upper_99, [{0, 2, 1}, {2, 4, 1000}])).

-endif.
