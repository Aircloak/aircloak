%% @doc Can be used to detect an unexpected increase of some observation.
%%      (e.g. RAM or disk usage).
%%
%%      This structure relies on a simplistic cumsum (Cumulative sum) approach.
%%      Client code continuously adds absolute values (e.g. RAM usage). The
%%      structure then maintains the current value. At any point, we can check
%%      if some threshold value has been exceeded. However, on each add,
%%      a small amount (called correction) is deduced. This compensates for
%%      short bursts of increase. Let's say that memory quickly increases to 100 MB,
%%      and then remains there. If correction factor is > 0, the accumulated
%%      value will over time reduce to zero.
%%
%%      Therefore, threshold corresponds to change and not absolute value.
%%      For example, when measuring RAM usage, a threshold value of 100 (MB)
%%      doesn't mean we use 100 MB, but that RAM usage has recently increased by
%%      roughly 100 MB.
-module(trend_detector).

%% API
-export([
  new/2,
  new/3,
  clear/1,
  add/2,
  threshold_reached/1
]).

-include("cloak.hrl").

-export_type([
  trend_detector/0
]).

-record(trend_detector, {
  last_value :: number(),
  trend = 0 :: number(),
  correction :: number(),
  threshold :: number()
}).

-opaque trend_detector() :: #trend_detector{}.


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Creates the new instance.
-spec new(number(), number()) -> trend_detector().
new(Threshold, Correction) ->
  new(0, Threshold, Correction).

%% @doc Creates the new instance.
-spec new(number(), number(), number()) -> trend_detector().
new(Start, Threshold, Correction) ->
  #trend_detector{last_value=Start, threshold=Threshold, correction=Correction}.

%% @doc Clears accumulated trend, without clearing the last value.
%%      This allows us to avoid over reporting after the threshold has
%%      been reached.
-spec clear(trend_detector()) -> trend_detector().
clear(#trend_detector{} = TrendDetector) ->
  TrendDetector#trend_detector{trend=0}.

%% @doc Recomputes the new absolute, using the current value and the previously recorded one.
%%      Correction amount is deduced from the new absolute.
-spec add(number(), trend_detector()) -> trend_detector().
add(Value, #trend_detector{} = TrendDetector) ->
  Change = Value - TrendDetector#trend_detector.last_value,
  TrendDetector#trend_detector{
    last_value = Value,
    trend = max(0, TrendDetector#trend_detector.trend + Change - TrendDetector#trend_detector.correction)
  }.

%% @doc Determines if threshold has been reached.
-spec threshold_reached(trend_detector()) -> boolean().
threshold_reached(#trend_detector{trend=Trend, threshold=Threshold}) -> Trend >= Threshold.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

add_values(Values, Trend) ->
  lists:foldl(fun add/2, Trend, Values).

aggregate_test_() ->
  [
    ?_assertEqual(false, threshold_reached(add_values([1, 1, 1], new(10, 1)))),
    ?_assertEqual(false, threshold_reached(add_values([6, 11], new(10, 1)))),
    ?_assertEqual(true, threshold_reached(add_values([6, 12], new(10, 1)))),
    ?_assertEqual(false, threshold_reached(add_values([6, 12, 12], new(10, 1)))),
    ?_assertEqual(false, threshold_reached(clear(add_values([6, 12], new(10, 1))))),
    ?_assertEqual(false, threshold_reached(add_values([22], clear(add_values([6, 12], new(10, 1)))))),
    ?_assertEqual(true, threshold_reached(add_values([23], clear(add_values([6, 12], new(10, 1))))))
  ].

-endif.
