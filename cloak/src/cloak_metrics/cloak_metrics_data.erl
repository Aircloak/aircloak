%% @doc Implements data structure for all collected metrics (counters and histograms).
%%      Receives individual datapoints and produces aggregated statictics.

-module(cloak_metrics_data).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
  new/1, reset/1, inc_counter/3, add_histogram/3, aggregate/2
]).

%% Types
-export_type([metrics/0]).

-type aggregated_metric() :: {binary(), {Timestamp::pos_integer(), Value::number()}}.

-type options() :: [
    {bucketizer, cloak_metrics:bucketizer()} |
    {noise_fun, cloak_metrics:noise_fun()} |
    {path_prefix, [cloak_metrics:metric_name()]}
  ].

%% Internal
-record(metrics, {
  counters = dict:new() :: dict:dict(binary(), pos_integer()),
  histograms = dict:new() :: dict:dict(binary(), cloak_metrics_datapoints:datapoints()),
  bucketizer,
  noise_fun,
  path_prefix,
  sanitize_regex
}).

-opaque metrics() :: #metrics{}.


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec new(options()) -> metrics().
%% @doc Creates new metrics instance.
new(Options) ->
  lists:foreach(
    fun(RequiredProperty) ->
      case proplists:get_value(RequiredProperty, Options) of
        undefined -> erlang:error({not_provided, RequiredProperty});
        _ -> ok
      end
    end,
    [bucketizer, noise_fun]
  ),

  % only alphanumerics, underscore, dash (-), and period characters are allowed
  {ok, SanitizeRegex} = re:compile("[^a-zA-Z0-9_\\-\\.]"),

  #metrics{
    bucketizer=proplists:get_value(bucketizer, Options),
    noise_fun=proplists:get_value(noise_fun, Options),
    path_prefix=[server_name() | proplists:get_value(path_prefix, Options, [])],
    sanitize_regex=SanitizeRegex
  }.

-spec reset(metrics()) -> metrics().
%% @doc Resets the metrics by erasing counters and histograms.
reset(#metrics{} = Metrics) -> Metrics#metrics{counters=dict:new(), histograms=dict:new()}.

-spec inc_counter(cloak_metrics:metric_name(), pos_integer(), metrics()) -> metrics().
%% @doc Increments a counter value.
inc_counter(Name, Value, #metrics{} = Metrics) when is_integer(Value) andalso Value > 0 ->
  NewCounters = dict:update_counter(
    normalized_name(Name),
    Value,
    Metrics#metrics.counters
  ),
  Metrics#metrics{counters=NewCounters}.


-spec add_histogram(cloak_metrics:metric_name(), integer(), metrics()) -> metrics().
%% @doc Adds a histogram value to collected statistics.
add_histogram(Name, Value, #metrics{} = Metrics) when is_integer(Value) ->
  Metrics#metrics{histograms=add_datapoint(normalized_name(Name), Value, Metrics#metrics.histograms)}.

-spec aggregate(metrics(), number()) -> [aggregated_metric()].
%% @doc Produces aggregated data from the collected metrics.
aggregate(#metrics{} = Metrics, Timespan) ->
  Timestamp = time_util:unix_timestamp(),
  lists:foldl(
    fun(MetricType, Aggregated) ->
      aggregate(MetricType, Timestamp, Timespan, Aggregated, Metrics)
    end,
    [], [counters, histograms]
  ).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

-ifdef(TEST).
server_name() -> localhost.
-else.
server_name() ->
  re:replace(net_adm:localhost(), "\\.", "_", [global, {return, binary}]).
-endif.

anonymization_error(FullMetricName, Timestamp, Original, Anonymized) ->
  {
    << <<"anonymization_error.">>/binary, FullMetricName/binary >>,
    {Timestamp, relative_anonymization_error(Original, Anonymized)}
  }.

relative_anonymization_error(Original, Anonymized) when Original == 0 andalso Anonymized == 0 -> 0;
relative_anonymization_error(Original, Anonymized) when Original == 0 ->
  round(100 * Anonymized / (abs(Anonymized)));
relative_anonymization_error(Original, Anonymized) -> round(100 * ((Anonymized - Original) / Original)).

add_datapoint(Name, Value, Storage) ->
  dict:update(
    Name,
    fun(Datapoints) -> cloak_metrics_datapoints:add(Value, Datapoints) end,
    cloak_metrics_datapoints:add(Value, cloak_metrics_datapoints:new()),
    Storage
  ).

normalized_name(Name) when is_atom(Name) -> atom_to_binary(Name, latin1);
normalized_name(Name) when is_list(Name) -> list_to_binary(Name);
normalized_name(Name) when is_binary(Name) -> Name.

aggregate(counters, Timestamp, Timespan, Aggregated, Metrics) ->
  dict:fold(
    fun(Name, Value, AggregatedAcc) ->
      FullMetricName = full_metric_name(Metrics, [Name, rate]),
      RatePerSec = round(Value / Timespan),
      % noised value is kept >= 0 since this is a counter
      AnonymizedRatePerSec = lists:max([0, apply_noise(RatePerSec, Metrics)]),
      AnonymizedEntry = {FullMetricName, {Timestamp, AnonymizedRatePerSec}},
      AnonymizationError = anonymization_error(
        FullMetricName, Timestamp, RatePerSec, AnonymizedRatePerSec
      ),
      [AnonymizedEntry | [AnonymizationError | AggregatedAcc]]
    end, Aggregated, Metrics#metrics.counters
  );
aggregate(histograms, Timestamp, _, Aggregated, Metrics) ->
  % Collect all histograms
  AllHistograms = lists:map(
    fun({Name, Datapoints}) ->
      {Name, cloak_metrics_datapoints:occurences(Datapoints)}
    end,
    dict:to_list(Metrics#metrics.histograms)
  ),
  % Make dict of original (non anonymized) stats
  OriginalValues = lists:foldl(
    fun(BucketSet, Acc) ->
      lists:foldl(
        fun({Name, {_, Value}}, InnerAcc) ->
          dict:store(Name, Value, InnerAcc)
        end,
        Acc,
        aggregate_bucket_set(Timestamp, Metrics, BucketSet)
      )
    end,
    dict:new(),
    (cloak_metrics:identity_bucketizer())(AllHistograms)
  ),
  % Emit anonymized stats and relative anonymization error
  lists:foldl(
    fun(BucketSet, Acc) ->
      lists:foldl(
        fun({Name, {_, AnonymizedValue}} = AnonymizedEntry, AccInner) ->
          OriginalValue = dict:fetch(Name, OriginalValues),
          AnonymizationError = anonymization_error(
            Name, Timestamp, OriginalValue, AnonymizedValue
          ),
          [AnonymizationError | [AnonymizedEntry | AccInner]]
        end,
        Acc,
        aggregate_bucket_set(Timestamp, Metrics, BucketSet)
      )
    end,
    Aggregated,
    (Metrics#metrics.bucketizer)(AllHistograms)
  ).

apply_noise(Value, #metrics{noise_fun=NoiseFun}) ->
  NoiseFun(Value).

aggregate_bucket_set(Timestamp, Metrics, {Name, Buckets}) ->
  lists:map(
    fun({MetricType, Value}) ->
      {
        full_metric_name(Metrics, [Name, MetricType]),
        {Timestamp, Value}
      }
    end,
    cloak_metrics_aggregation:aggregate(Buckets)
  ).

full_metric_name(#metrics{path_prefix=PathPrefix, sanitize_regex=SanitizeRegex}, NameParts) ->
  FullName = full_metric_name(PathPrefix ++ NameParts),
  re:replace(FullName, SanitizeRegex, "", [global, {return, binary}]).

full_metric_name([Head | []]) -> normalized_name(Head);
full_metric_name([Head | Tail]) ->
  << (normalized_name(Head))/binary, <<".">>/binary, (full_metric_name(Tail))/binary >>.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

new_data() ->
  new([
    {bucketizer, cloak_metrics:identity_bucketizer()},
    {noise_fun, cloak_metrics:no_noise_fun()}
  ]).

counters(Data) ->
  lists:foldl(
    fun(Value, Acc) -> inc_counter(<<"counter">>, Value, Acc) end,
    Data,
    lists:seq(10, 30, 10)
  ).

histograms(Data) ->
  lists:foldl(
    fun(Value, Acc) -> add_histogram(<<"histogram">>, Value, Acc) end,
    Data,
    lists:seq(1, 100, 1)
  ).

empty_test() ->
  ?assertEqual([], aggregate(new_data(), 10)).

required_params_test() ->
  ?assertError({not_provided,bucketizer}, new([])).

reset_test() ->
  Reset = reset(histograms(counters(new_data()))),
  ?assertEqual(dict:new(), Reset#metrics.counters),
  ?assertEqual(dict:new(), Reset#metrics.histograms).

sanitize_test() ->
  ?assertMatch(
    [
      {<<"localhost.abc.ABC123-_.rate">>, _},
      {<<"anonymization_error.localhost.abc.ABC123-_.rate">>, _}
    ],
    aggregate(inc_counter(<<"abc.ABC123-_?&()\\ ">>, 10, new_data()), 10)
  ).

path_prefix_test() ->
  Data = new([
    {bucketizer, cloak_metrics:identity_bucketizer()},
    {noise_fun, cloak_metrics:no_noise_fun()},
    {path_prefix, [p1, "p2", <<"p3">>]}
  ]),
  ?assertMatch(
    [
      {<<"localhost.p1.p2.p3.my_counter.rate">>, _},
      {<<"anonymization_error.localhost.p1.p2.p3.my_counter.rate">>, _}
    ],
    aggregate(inc_counter(my_counter, 10, Data), 10)
  ).

counter_test() ->
  ?assertMatch(
    [
      {<<"localhost.counter.rate">>, {_, 6}},
      {<<"anonymization_error.localhost.counter.rate">>, {_, 0}}
    ],
    aggregate(counters(new_data()), 10)
  ).

neg_counter_test() ->
  Data = new([
    {bucketizer, cloak_metrics:identity_bucketizer()},
    {noise_fun, fun(_) -> -1 end}
  ]),
  ?assertMatch([
    {<<"localhost.counter.rate">>, {_, 0}},
    {<<"anonymization_error.localhost.counter.rate">>, {_, -100}}
  ], aggregate(counters(Data), 10)).

histogram_test() ->
  ?assertMatch([
      {<<"anonymization_error.localhost.histogram.average">>, {_, 0}},
      {<<"localhost.histogram.average">>, {_, 50.5}},
      {<<"anonymization_error.localhost.histogram.median">>, {_, 0}},
      {<<"localhost.histogram.median">>, {_, 50.0}},
      {<<"anonymization_error.localhost.histogram.upper_75">>, {_, 0}},
      {<<"localhost.histogram.upper_75">>, {_, 75.0}},
      {<<"anonymization_error.localhost.histogram.upper_90">>, {_, 0}},
      {<<"localhost.histogram.upper_90">>, {_, 90.0}},
      {<<"anonymization_error.localhost.histogram.upper_99">>, {_, 0}},
      {<<"localhost.histogram.upper_99">>, {_, 99.0}}
    ],
    aggregate(histograms(new_data()), 10)
  ).

complete_test() ->
  ?assertMatch([
      {<<"anonymization_error.localhost.histogram.average">>, _},
      {<<"localhost.histogram.average">>, _},
      {<<"anonymization_error.localhost.histogram.median">>, _},
      {<<"localhost.histogram.median">>, _},
      {<<"anonymization_error.localhost.histogram.upper_75">>, _},
      {<<"localhost.histogram.upper_75">>, _},
      {<<"anonymization_error.localhost.histogram.upper_90">>, _},
      {<<"localhost.histogram.upper_90">>, _},
      {<<"anonymization_error.localhost.histogram.upper_99">>, _},
      {<<"localhost.histogram.upper_99">>, _},
      {<<"localhost.counter.rate">>, _},
      {<<"anonymization_error.localhost.counter.rate">>, _}
    ],
    aggregate(histograms(counters(new_data())), 10)
  ).

non_anonymized_counter_test() ->
  Data = new([
    {bucketizer, cloak_metrics:identity_bucketizer()},
    {noise_fun, fun(X) -> X * 2 end}
  ]),
  ?assertMatch([
      {<<"localhost.counter.rate">>, {_, 12}},
      {<<"anonymization_error.localhost.counter.rate">>, {_, 100}}
    ],
    aggregate(counters(Data), 10)
  ).

non_anonymized_histogram_test() ->
  Bucketizer = fun(Histograms) ->
    lists:map(fun({Name, _}) -> {Name, [{0, 11, 10}]} end, Histograms)
  end,
  Data = new([
    {bucketizer, Bucketizer},
    {noise_fun, cloak_metrics:no_noise_fun()}
  ]),
  ?assertMatch([
      {<<"anonymization_error.localhost.histogram.average">>, {_, -90}},
      {<<"localhost.histogram.average">>, {_, 5.0}},
      {<<"anonymization_error.localhost.histogram.median">>, {_, -90}},
      {<<"localhost.histogram.median">>, {_, 5.0}},
      {<<"anonymization_error.localhost.histogram.upper_75">>, {_, -90}},
      {<<"localhost.histogram.upper_75">>, {_, 7.5}},
      {<<"anonymization_error.localhost.histogram.upper_90">>, {_, -90}},
      {<<"localhost.histogram.upper_90">>, {_, 9.0}},
      {<<"anonymization_error.localhost.histogram.upper_99">>, {_, -90}},
      {<<"localhost.histogram.upper_99">>, {_, 9.9}}
    ],
    aggregate(histograms(Data), 10)
  ).

-endif.
