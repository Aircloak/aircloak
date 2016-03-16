%% @doc Responsible for starting the cloak metrics server, and doing anonymization
%%      and bucketization of metrics.
%%
%%      Note: metrics anonymization is disabled in the development mode.
-module(cloak_metrics_adapter).

%% API
-export([
  start_metrics_server/0
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec start_metrics_server() -> {ok, pid()} | {error, {already_started, pid()}}.
%% @doc Starts the singleton metrics server instance.
start_metrics_server() ->
  cloak_metrics:start_server([
        {noise_fun, fun noise_fun/1},
        {bucketizer, fun bucketize/1},
        {path_prefix, [cloak_core]},
        {reporters, [
          cloak_metrics:graphite_pickle_reporter(
                cloak_conf:get_val(metrics, graphite_server),
                cloak_conf:get_val(metrics, graphite_port)
              )
        ]}
      ]).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

noise_fun(N) ->
  case cloak_conf:in_development() of
    true ->
      (cloak_metrics:no_noise_fun())(N);
    _ ->
      SigmaSoftLowerBound = cloak_conf:get_val(noise, sigma_soft_lower_bound),
      cloak_distributions:gauss(SigmaSoftLowerBound, N)
  end.

bucketize(MetricsHistograms) ->
  case cloak_conf:in_development() of
    true ->
      (cloak_metrics:identity_bucketizer())(MetricsHistograms);
    _ ->
      HistogramAsRanges = metrics_to_ranges(MetricsHistograms),
      AnonymizedRanges = anonymize_ranges(HistogramAsRanges),
      to_bucket_sets(AnonymizedRanges)
  end.

metrics_to_ranges([]) -> [];
metrics_to_ranges([{MetricName, Datapoints}|RemainingMetrics]) ->
  [{MetricName, range_generation:generate(Datapoints, fun noise_fun/1, 5)} | metrics_to_ranges(RemainingMetrics)].

anonymize_ranges([]) -> [];
anonymize_ranges([{MetricName, Ranges}|RemainingMetrics]) ->
  AsBucketReports = ranges_to_report(Ranges),
  AnonymizedReports = anonymizer:anonymize(AsBucketReports),
  [{MetricName, AnonymizedReports} | anonymize_ranges(RemainingMetrics)].

ranges_to_report([]) -> [];
ranges_to_report([{From, To, Count} | Ranges]) ->
  Report = #bucket_report{
    label = #bucket_label{
      label = cloak_util:binarify(From),
      value = cloak_util:binarify(To)
    },
    count = Count,
    noisy_count = Count,
    users_hash = crypto:hash(md4, term_to_binary(["anon", "metrics"])),
    noise_sd = 0
  },
  [Report | ranges_to_report(Ranges)].

to_bucket_sets([]) -> [];
to_bucket_sets([{MetricName, Reports}|RemainingReports]) ->
  Buckets = [{range_from_string(From), range_from_string(To), NoisyCount}
      || #bucket_report{label=#bucket_label{label=From, value=To}, noisy_count=NoisyCount} <- Reports],
  BucketSet = {MetricName, Buckets},
  [BucketSet | to_bucket_sets(RemainingReports)].

range_from_string(<<"neg_infinite">>) -> neg_infinite;
range_from_string(<<"infinite">>) -> infinite;
range_from_string(Num) -> binary_to_integer(Num).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bucketize_test() ->
  meck:new(range_generation),
  meck:expect(range_generation, generate, fun(_, _, _) -> [{0,1,1}, {2, 10, 100}] end),
  Input = [{<<"TaskExecutionTime">>, [{1, 2}, {3, 4}]}],
  ?assertMatch([{<<"TaskExecutionTime">>, [{2,10, _}]}], bucketize(Input)),
  ?assert(meck:validate(range_generation)),
  meck:unload().

range_to_string_conversions_test_() ->
  [
    ?_assertEqual(1, range_from_string(cloak_util:binarify(1))),
    ?_assertEqual(infinite, range_from_string(cloak_util:binarify(infinite))),
    ?_assertEqual(neg_infinite, range_from_string(cloak_util:binarify(neg_infinite)))
  ].

-endif.
