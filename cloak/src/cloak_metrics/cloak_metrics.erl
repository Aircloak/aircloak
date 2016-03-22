%% @doc This module serve as an interface point for publishing various metrics,
%%      such as counters and histograms. It is a singleton `gen_server' that
%%      accepts requests, and periodically flushes the aggregated statistics.
-module(cloak_metrics).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
  start_link/1,
  start_server/1,
  graphite_pickle_reporter/2,
  count/1,
  count/2,
  histogram/2,
  measure/2,
  measure/3,
  identity_bucketizer/0,
  no_noise_fun/0
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("cloak.hrl").

%% Types
-export_type([
  options/0,
  bucketizer/0,
  reporter_def/0,
  metric_name/0,
  bucket/0,
  noise_fun/0
]).

-type options() :: [
  {flush_interval, pos_integer()} |
  {reporters, [reporter_def()]} |
  {bucketizer, bucketizer()} |
  {noise_fun, noise_fun()} |
  {path_prefix, [metric_name()]}
].

-type noise_fun() :: fun((number()) -> number()).

-type bucketizer() :: fun(([histogram()]) -> [bucket_set()]).
-type histogram() :: {Name::metric_name(), [{Value::number(), Count::pos_integer()}]}.
-type bucket_set() :: {Name::metric_name(), [bucket()]}.
-type bucket() :: {
  From :: neg_infinite | integer(),
  To :: infinite | integer(),
  NoisyCount :: pos_integer()
}.

-type reporter_def() :: {encoder(), transport(), transport_args(), ReconnectInterval::pos_integer()}.
-type encoder() :: Module::atom() | fun((InputData::any()) -> OutputData::any).
-type transport() :: Module::atom() | [reporter_funs()].
-type reporter_funs() ::
  {init, fun((transport_args()) -> reporter_response())} |
  {send, fun((reporter_state()) -> reporter_response())} |
  {reconnect, fun((transport_args(), reporter_state()) -> reporter_response())}.
-type transport_args() :: any().
-type reporter_state() :: any().
-type reporter_response() :: ok | {ok, reporter_state()} | {error, Reason::any()}.

-type metric_name() :: binary() | string() | atom().

%% Internal
-record(state, {
  aggregator,
  dispatchers,
  collected,
  last_flush_time=os:timestamp(),
  flush_interval
}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
%% @hidden
start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec start_server(options()) -> {ok, pid()} | {error, {already_started, pid()}}.
%% @doc Starts the server with provided options. Returns `{error, already_started}'
%%      if the server is already running. The server is started under a registered
%%      local alias and resides in this application supervision tree.
%%
%%      Note: you must pass some kind of noise and bucketizer function.
%%      If you don't need noise or bucketization, you can pass
%%      `{noise_fun, cloak_metrics:no_noise_fun()}' and
%%      `{bucketizer, cloak_metrics:identity_bucketizer()}'.
start_server(Options) ->
  cloak_metrics_sup:start_metrics_server(Options).

-spec graphite_pickle_reporter(string(), pos_integer()) -> reporter_def().
%% @doc Returns the tuple describing graphite pickle reporter on a given host and port.
%%      Can be used for simplified starting with {@link start_server/1}.
graphite_pickle_reporter(Host, Port) ->
  {
    cloak_metrics_carbon_pickle,
    cloak_metrics_tcp_transport,
    [{host, Host}, {port, Port}],
    timer:seconds(60)
  }.

-spec count(metric_name()) -> ok.
%% @doc Increments a counter value for the given key.
count(Key) -> count(Key, 1).

-spec count(metric_name(), pos_integer()) -> ok.
%% @doc Increments a counter value for the given key for a given value.
count(Key, Value) when is_integer(Value) andalso Value > 0 ->
  gen_server:cast(?MODULE, {push, inc_counter, Key, Value}).

-spec histogram(metric_name(), integer()) -> ok.
%% @doc Adds a histogram value to the statistics.
histogram(Key, Value) when is_integer(Value) ->
  gen_server:cast(?MODULE, {push, add_histogram, Key, Value}).

-spec measure(metric_name(), fun(() -> any())) -> any().
%% @doc Same as {@link measure/3} using milliseconds as reported time unit.
measure(Key, Fun) -> measure(Key, ms, Fun).

-spec measure(metric_name(), s | ms | us, fun(() -> any())) -> any().
%% @doc Executes function, collects its execution time (in given time unit) to the
%%      corresponding histogram. Returns result of the executed function.
measure(Key, Unit, Fun) ->
  {Microseconds, Result} = timer:tc(Fun),
  histogram(Key, convert_time(Microseconds, Unit)),
  Result.

%% @doc Returns the identity bucketizer function that creates single bucket
%%      for each value.
-spec identity_bucketizer() -> bucketizer().
identity_bucketizer() -> fun identity_buckets/1.

%% @doc Returns the identity function that doesn't append any noise to the value.
-spec no_noise_fun() -> noise_fun().
no_noise_fun() ->
  fun(Value) -> Value end.


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

%% @hidden
init(Options) ->
  {ok, AggregatorPid} = cloak_metrics_aggregator:start_link(),
  FlushInterval = proplists:get_value(flush_interval, Options, timer:seconds(10)),
  timer:send_interval(FlushInterval, flush_stats),
  {ok, #state{
    aggregator = AggregatorPid,
    flush_interval = FlushInterval,
    dispatchers = start_dispatchers(proplists:get_value(reporters, Options, [])),
    collected = cloak_metrics_data:new(Options)
  }}.

start_dispatchers(ReportersDef) ->
  lists:map(fun(ReporterDef) ->
        {ok, ReporterPid} = cloak_metrics_dispatcher:start_link(ReporterDef),
        ReporterPid
      end, ReportersDef).


%% @hidden
handle_call(Unknown, _, State) ->
  ?ERROR("Unknown call in ~p: ~p", [{?MODULE, Unknown}]),
  {reply, error, State}.

%% @hidden
handle_cast({push, Operation, Key, Value}, State) ->
  NewCollected = cloak_metrics_data:Operation(Key, Value, State#state.collected),
  {noreply, State#state{collected=NewCollected}};
handle_cast(Unknown, State) ->
  ?ERROR("Unknown cast in ~p: ~p", [{?MODULE, Unknown}]),
  {noreply, State}.


%% @hidden
handle_info(flush_stats, State) ->
  Now = os:timestamp(),
  Timespan = timer:now_diff(Now, State#state.last_flush_time) / 1000000,
  cloak_metrics_aggregator:aggregate_and_dispatch(State#state.aggregator,
      State#state.collected, State#state.dispatchers, Timespan),
  {noreply, State#state{last_flush_time=Now, collected=cloak_metrics_data:reset(State#state.collected)}};
handle_info(_, State) ->
  {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

identity_buckets(Histograms) ->
  IdentityBuckets = fun(DataPoints) ->
    Counts = lists:foldl(
          fun({Value, Count}, CountsAcc) -> dict:update_counter(Value, Count, CountsAcc) end,
          dict:new(), DataPoints
        ),
    dict:fold(
          fun(Value, Count, Acc) -> [{Value, Value + 1, Count} | Acc] end,
          [],
          Counts
        )
  end,
  lists:map(
        fun({Name, DataPoints}) -> {Name, IdentityBuckets(DataPoints)} end,
        Histograms
      ).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

convert_time(Microseconds, s) -> round(Microseconds/1000000);
convert_time(Microseconds, ms) -> round(Microseconds/1000);
convert_time(Microseconds, us) -> Microseconds.

%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include("eunit_helpers.hrl").

metrics_test_() ->
  {foreach,
    fun setup/0, fun cleanup/1,
    [fun test_start_server/0, fun test_init/0, fun test_send/0, fun test_measure/0]
  }.

setup() ->
  {ok, Supervisor} = cloak_metrics_sup:start_link(),
  Supervisor.

cleanup(Supervisor) -> exit(Supervisor, normal), timer:sleep(50).

test_start_server() ->
  ?assertEqual(undefined, whereis(cloak_metrics)),
  start_test_server(),
  Pid = whereis(cloak_metrics),
  ?assert(is_pid(Pid)),
  ?assertEqual({error, {already_started, Pid}}, start_server([])),
  ?assertEqual(Pid, whereis(cloak_metrics)).

test_init() ->
  start_test_server(),
  ?assertReceived(initalized, 100),
  ?assertNotReceived({sent_data, _}, 100).

test_send() ->
  start_test_server(),
  [count(<<"counter">>) || _ <- lists:duplicate(100, 1)],
  [histogram(<<"histogram">>, 20) || _ <- lists:duplicate(100, 1)],
  ?assertReceived(
      {sent_data,
        [
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
        ]
      }, 100).

test_measure() ->
  start_test_server(),
  ?assertEqual(result, measure(<<"measured">>, fun() -> result end)),
  [
    spawn(fun() ->
          measure(<<"measured">>, fun() -> timer:sleep(Duration) end)
        end) ||
    Duration <- lists:seq(1, 20)
  ],
  ?assertReceived(
      {sent_data,
        [
          {<<"anonymization_error.localhost.measured.average">>, _},
          {<<"localhost.measured.average">>, _},
          {<<"anonymization_error.localhost.measured.median">>, _},
          {<<"localhost.measured.median">>, _},
          {<<"anonymization_error.localhost.measured.upper_75">>, _},
          {<<"localhost.measured.upper_75">>, _},
          {<<"anonymization_error.localhost.measured.upper_90">>, _},
          {<<"localhost.measured.upper_90">>, _},
          {<<"anonymization_error.localhost.measured.upper_99">>, _},
          {<<"localhost.measured.upper_99">>, _}
        ]
      }, 100).

start_test_server() ->
  start_server([
        {bucketizer, identity_bucketizer()},
        {noise_fun, no_noise_fun()},
        {flush_interval, 50},
        {reporters, [make_test_reporter()]}
      ]).

make_test_reporter() ->
  {
    fun(X) -> X end,
    [{init, fun test_init/1}, {send, fun test_send/2}],
    {transport_args, self()},
    50
  }.

test_init({transport_args, TestPid}) -> TestPid ! initalized, {ok, TestPid}.

test_send(Data, Pid) -> Pid ! {sent_data, Data}, {ok, Pid}.

identity_test_() ->
  {inorder,
    [fun test_no_noise_fun/0, fun test_identity_buckets/0]
  }.

test_no_noise_fun() ->
  ?assertEqual(10, (no_noise_fun())(10)).

test_identity_buckets() ->
  Histograms = [
    {metric1, [{1, 1}, {2, 2}]},
    {metric2, [{1, 1}, {2, 2}, {1, 3}]}
  ],
  ?assertEqual(
        [
          {metric1, [{2, 3, 2}, {1, 2, 1}]},
          {metric2, [{2, 3, 2}, {1, 2, 4}]}
        ],
        (identity_bucketizer())(Histograms)
      ).

-endif.
