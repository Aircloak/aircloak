%% @doc Aggregates a set of job responses into buckets.
%%      Here, we collect all job responses, group them together, and
%%      remove duplicates for individual users (multiple responses per distinct
%%      property-user combination).
%%
%%      Each bucket also contains a hash (md4) of a distinct list of users
%%      that reported the particular bucket, which is necessary for later
%%      anonymization steps.
%%
%%      Internally, aggregator relies on an ETS table. It has been experimentally
%%      established that this approach performs much better than the purely
%%      functional approach relying on dicts and sets.
-module(aggregator).

%% API
-export([
  new/0,
  new/1,
  delete/1,
  add_buckets/2,
  add_property/3,
  buckets/1
]).

-include("cloak.hrl").

-record(aggregator, {
  table :: ets:tab(),
  lcf_users :: undefined | 'Elixir.Cloak.LowCountFilter':t()
}).

-opaque aggregator() :: #aggregator{}.

-export_type([
  aggregator/0
]).

%% Number of processes used to aggregate buckets. We want some
%% amount of concurrency here, but not too much, to avoid choking the entire
%% system. See buckets/1 implementation for more details.
-define(BUCKETS_PROCESSORS, 10).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Just like {@link new/1} with `undefined' lcf users collector.
-spec new() -> aggregator().
new() ->
  new(undefined).

%% @doc Creates the new aggregator instance. If {@link 'Elixir.Cloak.LowCountFilter'} is provided, it
%%      will be used to collect candidates for low count filter (lcf) tail. These
%%      candidates are collected when {@link bucket/1} function is called.
-spec new(undefined | 'Elixir.Cloak.LowCountFilter':t()) -> aggregator().
new(LcfUsers) ->
  #aggregator{
    % The aggregator is internally an ETS set table, with keys containing
    % `{Property, UserId}`. Furthermore, the table is public, and concurrency
    % options are set.
    % This allows us to quickly insert/deduplicate when aggregator is used by
    % job runners. In these cases, we want to quickly insert data and thus allow
    % job runners to process subsequent requests as soon as possible. In
    % particular, we get following properties:
    %
    %   1. Insertion of job responses is very fast.
    %   2. Different job runners can insert job responses in parallel.
    %   3. Job responses are immediately deduplicated during insertion.
    %   4. There is no aggregator process, which means no single process bottleneck,
    %      no large mailbox queues, and no scheduler switching overhead.
    %
    % The final aggregation takes place when buckets are requested.
    % To improve the performance when processing a huge number of buckets, we
    % produce final buckets in parallel. See `buckets/1` for more details.
    table = ets:new(aggregator, [set, public, {write_concurrency, true}, {read_concurrency, true}]),
    lcf_users = LcfUsers
  }.

%% @doc Releases the memory occupied by the aggregator.
-spec delete(aggregator()) -> ok.
delete(#aggregator{table=Table}) ->
  ets:delete(Table),
  ok.

%% @doc Adds a property for the given user to the aggregator.
-spec add_property(property(), user_id(), aggregator()) -> aggregator().
add_property(Property, UserId, Aggregator) ->
  add(Property, UserId, 1, Aggregator),
  Aggregator.

%% @doc Adds buckets to the aggregator. User hash of the bucket
%%      will be used as a user identifier. Refer to implementation
%%      of `task_coordinator:handle_message/2' for detailed explanation.
-spec add_buckets([#bucket{}], aggregator()) -> aggregator().
add_buckets(Buckets, Aggregator) ->
  [
    add(Property, UsersHash, Count, Aggregator)
    || #bucket{property = Property, users_hash = UsersHash, count = Count} <- Buckets
  ],
  Aggregator.

%% @doc Returns aggregated buckets.
-spec buckets(aggregator()) -> [#bucket{}].
buckets(#aggregator{table = Table} = Aggregator) ->
  % Here we build final buckets, joining all reported buckets under the same key.
  % Since this can be an intensive task for a large number of reported buckets,
  % we do a parallel split. We start a couple of workers, each responsible for
  % some partition of reported buckets. Then in this "master" process, we
  % iterate through the table and forward items to corresponding workers.
  % All buckets with the same properties are handled by the same worker, so
  % aggregation will work correctly. This allows us to do some work in parallel,
  % and also puts less strain on the garbage collector.
  % Finally, the master process collects the results from all workers.
  % Experiments demonstrated an order of magnitude performance improvement for a
  % very large sets (~ 2 millions of initial buckets, which are reduced to
  % ~ 100k buckets).
  Reporters = start_reporters(Aggregator),
  process_reports(Table, Reporters, ets:first(Table)).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

add(Property, UsersKey, Count, #aggregator{table=Table} = Aggregator) ->
  ets:insert(Table, {{Property, UsersKey}, Count}),
  Aggregator.

start_reporters(Aggregator) ->
  list_to_tuple([start_reporter(Aggregator) || _ <- lists:seq(0, ?BUCKETS_PROCESSORS - 1)]).

start_reporter(Aggregator) ->
  spawn_link(fun() -> report_loop(Aggregator, dict:new()) end).

report_loop(#aggregator{table=Table, lcf_users=LcfUsers} = Aggregator, Dict) ->
  receive
    {add, {Property, UsersKey} = Key} ->
      [{_, Count}] = ets:lookup(Table, Key),
      Dict1 = dict:update(
        Property,
        fun(UsersKeys) -> gb_trees:enter(UsersKey, Count, UsersKeys) end,
        gb_trees:from_orddict([{UsersKey, Count}]),
        Dict
      ),
      report_loop(Aggregator, Dict1);
    {get, Caller} ->
      Reports = dict:fold(
        fun(Property, UsersKeys, ReportsAcc) ->
          SortedUsersKeys = gb_trees:keys(UsersKeys),
          RawCount = lists:sum(gb_trees:values(UsersKeys)),
          case LcfUsers of
            undefined -> ok;
            _ ->
              case RawCount > ?LCF_CERTAINTY_THRESHOLD of
                true -> ok;
                false ->
                  'Elixir.Cloak.LowCountFilter':add_bucket_users(LcfUsers, Property, SortedUsersKeys)
              end
          end,
          [
            #bucket{
              property = Property,
              count = RawCount,
              noisy_count = RawCount,
              noise_sd = 0,
              users_hash = crypto:hash(md4, term_to_binary(SortedUsersKeys))
            } | ReportsAcc
          ]
        end,
        [],
        Dict
      ),
      Caller ! {reports, Reports}
  end.

process_reports(_, Reporters, '$end_of_table') ->
  get_reports(tuple_to_list(Reporters));
process_reports(Table, Reporters, {Property, _} = Key) ->
  Processor = element(erlang:phash2(Property, ?BUCKETS_PROCESSORS) + 1, Reporters),
  Processor ! {add, Key},
  process_reports(Table, Reporters, ets:next(Table, Key)).

get_reports(ReportersList) ->
  [Reporter ! {get, self()} || Reporter <- ReportersList],
  lists:foldl(
    fun(_, ReportsAcc) ->
      receive
        {reports, Reports} -> Reports ++ ReportsAcc
      end
    end,
    [],
    ReportersList
  ).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_buckets(Entries) ->
  Aggregator = new(),
  [
    add_property(Property, User, Aggregator)
    || {Property, User} <- Entries
  ],
  buckets(Aggregator).

bucket_counts(Buckets) ->
  SortedBuckets = lists:sort(
    fun(#bucket{count = Count1}, #bucket{count = Count2}) -> Count1 =< Count2 end,
    Buckets
  ),
  [{Bucket#bucket.property, Bucket#bucket.count} || Bucket <- SortedBuckets].

aggregator_test_() ->
  {setup,
    fun() -> gproc:start_link() end,
    fun(_) -> ok end,
    [
      ?_assertEqual([{b2, 1}, {b1, 2}], bucket_counts(make_buckets([{b1, u1}, {b1, u2}, {b2, u1}]))),
      ?_assertEqual(
        lists:sort(make_buckets([{b1, u1}, {b1, u2}, {b2, u1}])),
        lists:sort(make_buckets([{b2, u1}, {b1, u1}, {b1, u2}]))
      ),
      ?_assertNotEqual(
        lists:sort(make_buckets([{b1, u1}, {b1, u2}, {b2, u1}])),
        lists:sort(make_buckets([{b1, u1}, {b1, u2}, {b2, u3}]))
      ),
      ?_assertNotEqual(
        lists:sort(make_buckets([{b1, u1}, {b1, u2}, {b2, u1}])),
        lists:sort(make_buckets([{b1, u1}, {b1, u2}, {b2, u1}, {b2, u2}]))
      ),
      ?_assertNotEqual(
        lists:sort(make_buckets([{b1, u1}, {b1, u2}, {b2, u1}])),
        lists:sort(make_buckets([{b1, u1}, {b1, u2}, {b2, u1}, {b3, u2}]))
      ),
      fun() ->
        Aggregator = new(),
        EntriesList = [
          [{b1, u1}, {b1, u2}, {b2, u1}],
          [{b1, u3}, {b2, u4}]
        ],
        [add_buckets(make_buckets(Entries), Aggregator) || Entries <- EntriesList],
        ?assertEqual([{b2, 2}, {b1, 3}], bucket_counts(buckets(Aggregator)))
      end
    ]
  }.

-endif.
