%% @doc A storage for mapping bucket properties to user ids.
%%
%%      This module is used to collect all users which are removed due to low
%%      count filter (LCF). While running tasks, we'll add another property,
%%      so called <b>LCF tail</b>, which represents the anonymized count of
%%      LCF-ed users.
%%
%%      To use this module, you need to create the storage via {@link new/0}, and
%%      add new entries using {@link add_bucket_users/3}. It is recommended to
%%      add entries after the aggregation, only if the aggregated count is less
%%      than ?LCF_CERTAINTY_THRESHOLD (defined in cloak.hrl).
%%
%%      Finally, you can pass the storage to {@link anonymizer:anonymize/2}, which
%%      will use it to construct the additional LCF tail property.
%%
%%      The storage is internally powered by multiple ETS tables. On each node we
%%      create an ETS table which will hold LCF candidates for that node. This
%%      allows us to collect and keep user ids on the node they are reported on.
%%
%%      ETS tables are created in a temporary process which will die when the
%%      master process (the one which invoked `lcf_users:new/0') terminates.
-module(lcf_users).

%% API
-export([
  new/0,
  delete/1,
  add_bucket_users/3,
  lcf_tail_report/2
]).

%% Internal API
-export([
  create_local_lcf_table/1,
  local_lcf_tail_report/2
]).

-include("cloak.hrl").

-type lcf_users() :: dict:dict(node(), ets:tab()).

-export_type([
  lcf_users/0
]).

%% Number of processes used to aggregate the LCF-ed users. We want some
%% amount of concurrency here, but not too much, to avoid choking the entire
%% system. See lcf_tail_report/1 implementation for more details.
-define(AGGREGATE_PROCESSORS, 10).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Creates the new lcf users storage.
-spec new() -> lcf_users().
new() ->
  % Create local tables on all nodes, ignore bad nodes.
  {EtsTables, _BadNodes} = rpc:multicall(
    [node()],
    ?MODULE, create_local_lcf_table, [self()], timer:seconds(10)
  ),
  dict:from_list(EtsTables).

%% @doc Deletes the storage.
-spec delete(lcf_users()) -> ok.
delete(LcfStorage) ->
  [TableOwner ! stop || {_, {_, TableOwner}} <- dict:to_list(LcfStorage)],
  ok.

%% @doc Adds `property() -> [user_id()]' mapping to the storage.
-spec add_bucket_users(lcf_users(), property(), [user_id()]) -> lcf_users().
add_bucket_users(LcfStorage, Property, UserIds) ->
  case dict:find(node(), LcfStorage) of
    error ->
      % Not probable, but might happen during netsplit, so we'll just ignore.
      LcfStorage;
    {ok, {LocalEtsTable, _TableOwner}} ->
      ets:insert(LocalEtsTable, {Property, UserIds}),
      LcfStorage
  end.

%% @doc Generates the lcf_tail report based on the data in the storage.
%%      The generated property will be correctly connected to the sorted list of
%%      unique users which are lcf-ed. This function is not meant to be invoked
%%      directly. It will be called by the anonymizer after the reported properties
%%      have been lcf-ed.
%%
%%      For efficiency reasons, this function aggregates the property on each involved
%%      node. Then it performs final aggregation on this node. This greatly reduces the
%%      amount of data transferred. We keep list of LCF-ed users on the node where they
%%      were originally reported. Local aggregation will reduce this to a md4 of
%%      sorted users, so we're only sending a small amount of data cross-node, regardless
%%      of the amount of users in the lcf tail.
%%
%%      This works correctly as long as each user is guaranteed to be processed on the same
%%      node (for a single task).
-spec lcf_tail_report(lcf_users(), [property()]) -> undefined | #bucket{}.
lcf_tail_report(_, []) -> undefined;
lcf_tail_report(LcfStorage, Properties) ->
  Aggregator = aggregator:new(),
  try
    LocalReporters = start_local_reporters(LcfStorage, Properties, self()),
    add_reports_to_aggregator(LocalReporters, Aggregator),
    case aggregator:buckets(Aggregator) of
      [] -> undefined;
      [LcfReport] -> LcfReport
    end
  after
    aggregator:delete(Aggregator)
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

%% @hidden
create_local_lcf_table(MasterPid) ->
  Me = self(),
  %% Spawn the table owner
  TableOwner = spawn(fun() ->
    % Using duplicate_bag to improve performance. Since each user is handled in a dedicated partition,
    % overlapping entries shouldn't happen. Moreover, aggregation is used at the output to deduplicate entries,
    % so we can safely use a more performant version here to speed up insertions.
    EtsTable = ets:new(?MODULE, [duplicate_bag, public, {write_concurrency, true}, {read_concurrency, true}]),
    Me ! {ets_table, EtsTable},
    % Monitor the master so we can terminate if it goes down
    MRef = monitor(process, MasterPid),
    receive
      stop -> ok;
      {'DOWN', MRef, process, _, _} ->
        ?INFO("Master process down, stopping the local lcf_users table owner"),
        ok
    end
  end),
  receive
    {ets_table, EtsTable} -> {node(), {EtsTable, TableOwner}}
    after 5000 ->
      % Shouldn't happen, but just in case
      erlang:error(timeout)
  end.

start_local_reporters(LcfStorage, Properties, MasterPid) ->
  [
    begin
      % Spawn a process which fetches the report from the given node
      % Instead of linking, we monitor the process. This ensures the master process won't die
      % if the parallel process dies. We consider LCF tail as a "nice-to-have" feature, so if
      % something dies somewhere, we don't want to crash the entire task report.
      spawn_monitor(
        fun() ->
          MasterPid ! {
            result,
            % The timeout of 30 secs is chosen to accommodate cases when we have long LCF tails with
            % bunch of buckets and users.
            rpc:call(Node, ?MODULE, local_lcf_tail_report, [Table, Properties], timer:seconds(30))
          }
        end
      )
    end || {Node, {Table, _TableOwner}} <- dict:to_list(LcfStorage)
  ].

add_reports_to_aggregator(LocalReporters, Aggregator) ->
  [aggregator:add_buckets([Report], Aggregator) ||
    {_Pid, MRef} <- LocalReporters,
    Response <- [
      % Note: no timeout is needed here, because we enforce it in the spawned reporter.
      % The process will thus either send the result or die (which we catch with a monitor).
      receive
        {result, Result} -> Result;
        {'DOWN', MRef, process, _, _} -> down
      end
    ],
    % Filter only successful responses
    {report, Report} <- [Response]
  ],
  ok.

%% @hidden
local_lcf_tail_report(Table, Properties) ->
  Aggregator = aggregator:new(),
  try
    Processors = start_aggregate_processors(Table, Aggregator),
    dispatch_aggregations(Properties, Processors),
    stop_aggregate_processors(Processors),
    case aggregator:buckets(Aggregator) of
      [] -> undefined;
      [LcfReport] -> {report, LcfReport}
    end
  after
    aggregator:delete(Aggregator)
  end.

start_aggregate_processors(Table, Aggregator) ->
  [spawn_link(fun() -> aggregate_processor_loop(Table, Aggregator) end)
    || _ <- lists:seq(0, ?AGGREGATE_PROCESSORS - 1)].

dispatch_aggregations(Properties, Processors) ->
  dispatch_aggregations(Properties, [], Processors).

% We're dispatching each bucket to one processor in a round-robin fashion.
dispatch_aggregations([], _, _) -> ok;
dispatch_aggregations(Properties, [], AllProcessors) ->
  dispatch_aggregations(Properties, AllProcessors, AllProcessors);
dispatch_aggregations([Property | RestProperties], [ProcessorPid | RestProcessors], AllProcessors) ->
  ProcessorPid ! {aggregate, Property},
  dispatch_aggregations(RestProperties, RestProcessors, AllProcessors).

stop_aggregate_processors(Processors) ->
  MRefs = [
    begin
      ProcessorPid ! stop,
      monitor(process, ProcessorPid)
    end || ProcessorPid <- Processors
  ],
  % Wait for processes to finish, and then collect the final report.
  [receive {'DOWN', MRef, process, _, _} -> ok end || MRef <- MRefs],
  ok.

aggregate_processor_loop(Table, Aggregator) ->
  receive
    {aggregate, Property} ->
      [
        aggregator:add_property(
          [?AIRCLOAK_LABEL, ?LCF_TAIL_VALUE],
          UserId,
          Aggregator
        ) ||
        Matches <- ets:match(Table, {Property, '$1'}),
        UserIds <- Matches,
        UserId <- UserIds
      ],
      aggregate_processor_loop(Table, Aggregator);
    stop -> ok
  end.
