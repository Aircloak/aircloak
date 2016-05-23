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
-module(lcf_users).

%% API
-export([
  new/0,
  delete/1,
  add_bucket_users/3,
  lcf_tail_report/3
]).

-include("cloak.hrl").

-type lcf_users() :: {ets:tab(), pid()}.

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
  Me = self(),
  %% Spawn the table owner
  TableOwner = spawn(fun() ->
    % Using duplicate_bag to improve performance. Since each user is handled in a dedicated partition,
    % overlapping entries shouldn't happen. Moreover, aggregation is used at the output to deduplicate entries,
    % so we can safely use a more performant version here to speed up insertions.
    EtsTable = ets:new(?MODULE, [duplicate_bag, public, {write_concurrency, true}, {read_concurrency, true}]),
    Me ! {ets_table, EtsTable},
    % Monitor the master so we can terminate if it goes down
    MRef = monitor(process, Me),
    receive
      stop -> ok;
      {'DOWN', MRef, process, _, _} ->
        ?INFO("Master process down, stopping the local lcf_users table owner"),
        ok
    end
  end),
  receive
    {ets_table, EtsTable} -> {EtsTable, TableOwner}
    after 5000 ->
      % Shouldn't happen, but just in case
      erlang:error(timeout)
  end.

%% @doc Deletes the storage.
-spec delete(lcf_users()) -> ok.
delete({_EtsTable, TableOwner}) ->
  TableOwner ! stop,
  ok.

%% @doc Adds `property() -> [user_id()]' mapping to the storage.
-spec add_bucket_users(lcf_users(), property(), [user_id()]) -> lcf_users().
add_bucket_users({EtsTable, _TableOwner} = LcfStorage, Property, UserIds) ->
  ets:insert(EtsTable, {Property, UserIds}),
  LcfStorage.

%% @doc Generates the lcf_tail report based on the data in the storage.
%%      The generated property will be correctly connected to the sorted list of
%%      unique users which are lcf-ed. This function is not meant to be invoked
%%      directly. It will be called by the anonymizer after the reported properties
%%      have been lcf-ed.
-spec lcf_tail_report(lcf_users(), [property()], pos_integer()) -> undefined | #bucket{}.
lcf_tail_report(_, [], _) -> undefined;
lcf_tail_report({EtsTable, _TableOwner}, Properties, ColumnsCount) ->
  Aggregator = aggregator:new(),
  try
    Processors = start_aggregate_processors(EtsTable, ColumnsCount, Aggregator),
    dispatch_aggregations(Properties, Processors),
    stop_aggregate_processors(Processors),
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

start_aggregate_processors(Table, ColumnsCount, Aggregator) ->
  [spawn_link(fun() -> aggregate_processor_loop(Table, ColumnsCount, Aggregator) end)
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

aggregate_processor_loop(Table, ColumnsCount, Aggregator) ->
  receive
    {aggregate, Property} ->
      [
        aggregator:add_property(
          lists:duplicate(ColumnsCount, ?LCF_TAIL_PROPERTY),
          UserId,
          Aggregator
        ) ||
        Matches <- ets:match(Table, {Property, '$1'}),
        UserIds <- Matches,
        UserId <- UserIds
      ],
      aggregate_processor_loop(Table, ColumnsCount, Aggregator);
    stop -> ok
  end.
