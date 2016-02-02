%% @doc Calculates user table statistics
%%      This calculation is implemented as a globally registered gen_server
%%      with key being `{Analyst, TableId}'. This prevents multiple parallel
%%      stats calculations for the same table.
-module(table_stats_calculator).
-behaviour(gen_server).

%% Internal API
-export([
  run/4
]).

%% Internal API
-export([
  start_link/5
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

-include("air.hrl").
-include_lib("airpub/include/types.hrl").

-record(state, {
  analyst :: integer(),
  table_id :: integer(),
  cloak_url :: binary(),
  task_spec :: binary(),
  runner_mref :: undefined | reference()
}).

-define(PROGRESS_NOTIFICATION_INTERVAL, timer:seconds(10)).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the calculation concurrently.
-spec run(integer(), integer(), binary(), binary()) -> ok | already_running.
run(Analyst, TableId, CloakUrl, TaskSpec) ->
  Pid = global_service:get_or_create(
        {?MODULE, Analyst, TableId},
        {?MODULE, start_link, [Analyst, TableId, CloakUrl, TaskSpec]}
      ),
  gen_server:call(Pid, run).


%% -------------------------------------------------------------------
%% Internal API
%% -------------------------------------------------------------------

%% @hidden
start_link(GlobalServiceKey, Analyst, TableId, CloakUrl, TaskSpec) ->
  gen_server:start_link(
        {via, global_service, GlobalServiceKey},
        ?MODULE,
        #state{analyst=Analyst, table_id=TableId, cloak_url=CloakUrl, task_spec=TaskSpec},
        []
      ).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(Params) ->
  {ok, Params}.

%% @hidden
handle_call(run, _From, #state{runner_mref=undefined} = State) ->
  {_Pid, MRef} = erlang:spawn_monitor(
        fun() ->
          compute_stats(
              State#state.analyst,
              State#state.table_id,
              State#state.cloak_url,
              State#state.task_spec
            )
        end
      ),
  publish_to_airpub(State#state.analyst, State#state.table_id, [{type, <<"calculating">>}]),
  {reply, ok, State#state{runner_mref=MRef}, ?PROGRESS_NOTIFICATION_INTERVAL};
handle_call(run, _From, State) ->
  {reply, already_running, State, ?PROGRESS_NOTIFICATION_INTERVAL};
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info({'DOWN', MRef, process, _Pid, Reason}, #state{runner_mref=MRef} = State) ->
  case Reason of
    normal -> ok;
    _Other ->
      publish_to_airpub(State#state.analyst, State#state.table_id, [{type, <<"error">>}]),
      air_db:call(
            fun(Conn) ->
              sql_conn:extended_query(
                    [
                      "UPDATE user_table_stats SET success=false ",
                      "WHERE id = (SELECT max(id) FROM user_table_stats WHERE user_table_id=$1)"
                    ],
                    [State#state.table_id],
                    Conn
                  )
            end
          )
  end,
  {stop, Reason, State};
handle_info(timeout, State) ->
  publish_to_airpub(State#state.analyst, State#state.table_id, [{type, <<"calculating">>}]),
  {noreply, State, ?PROGRESS_NOTIFICATION_INTERVAL};
handle_info(_, State) -> {noreply, State, ?PROGRESS_NOTIFICATION_INTERVAL}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

compute_stats(Analyst, TableId, CloakUrl, TaskSpec) ->
  StartedAt = os:timestamp(),
  {ok, TaskResponse} = run_task(Analyst, CloakUrl, TaskSpec),
  {{Year, Month, Day}, {Hour, Minute, _Sec}} = cloak_util:timestamp_to_datetime(StartedAt),
  TableStats = [
    {created_at, iolist_to_binary(io_lib:format(
          "~B/~2.10.0B/~2.10.0B ~2.10.0B:~2.10.0B",
          [Year, Month, Day, Hour, Minute]
        ))} |
    table_stats(TaskResponse)
  ],
  publish_to_airpub(Analyst, TableId, [{type, <<"table_stats">>}, {data, TableStats}]),
  air_db:call(fun(Conn) ->
        {{insert,0,1}, [{RowId}]} = sql_conn:extended_query(
              [
                "INSERT INTO user_table_stats(user_table_id, num_users, num_rows, success, created_at, updated_at)",
                " VALUES($1, $2, $3, true, $4, $4) RETURNING id"
              ],
              [
                TableId,
                proplists:get_value(num_users, TableStats, 0),
                proplists:get_value(num_rows, TableStats, 0),
                cloak_util:timestamp_to_datetime(StartedAt)
              ],
              Conn
            ),
        {{delete, _}, []} = sql_conn:extended_query(
              "DELETE FROM user_table_stats WHERE user_table_id=$1 and id < $2",
              [TableId, RowId],
              Conn
            )
      end).

run_task(Analyst, CloakUrl, TaskSpec) ->
  Res = hackney:request(
        post,
        iolist_to_binary([CloakUrl, "/task/run"]),
        [
          {"async_query", "false"},
          {"analyst", integer_to_binary(Analyst)}
        ],
        mochijson2:encode(TaskSpec),
        [
          {connect_timeout, timer:seconds(10)},
          {recv_timeout, timer:minutes(10)}
        ]
      ),
  case Res of
    {ok, 200, _Headers, ClientRef} ->
      {ok, Body} = hackney:body(ClientRef),
      {ok, mochijson2:decode(Body)};
    Error ->
      ?ERROR("Error fetching data from cloak ~p", [Error]),
      error
  end.

table_stats(TaskResponse) ->
  Buckets = ej:get({"buckets", {select, {"label", "num_rows"}}}, TaskResponse),
  SortedRowCounts = lists:sort([
    {
      binary_to_integer(proplists:get_value(<<"value">>, Bucket)),
      proplists:get_value(<<"count">>, Bucket)
    } || {struct, Bucket} <- Buckets
  ]),
  [
    {num_users, num_users(SortedRowCounts)},
    {num_rows, estimated_num_rows(SortedRowCounts)}
  ].

num_users([]) ->
  % No buckets -> user count is 0 (or small enough)
  0;
num_users(RowCounts) ->
  lists:max([Count || {_Value, Count} <- RowCounts]).

estimated_num_rows(SortedRowCounts) ->
  % Sum weighted counts to get the total row count. We must keep in mind
  % that buckets are in CDF style, so we need to account the difference
  % of counts between two adjacent columns to get the correct result.
  %
  % For example, let's say we have:
  %   20 users with at least 1 row
  %   15 users with at least 2 rows
  %    7 users with at least 3 rows
  %
  % Then, the total count is computed as:
  %     7 * 3 + (15 - 7) * 2 + (20 - 15) * 1
  {EstimatedRowCount, _} = lists:foldr(
        fun({Count, NumRows}, {TotalAcc, PreviousBucketCount}) ->
          {
            TotalAcc + max(NumRows - PreviousBucketCount, 0) * Count,
            NumRows
          }
        end,
        {0, 0},
        SortedRowCounts
      ),
  EstimatedRowCount.

publish_to_airpub(Analyst, TableId, Content) ->
  % We need to push on the node where the airpub leader is running
  case airpub_leader:leader() of
    undefined ->
      ?ERROR("No airpub leader, can't push the message");
    Pid ->
      rpc:cast(
            node(Pid),
            router,
            publish,
            [#article{
              path=binary_to_list(iolist_to_binary(io_lib:format("/table_stats/~p/~p", [Analyst, TableId]))),
              content=iolist_to_binary(mochijson2:encode(Content)),
              published_at=os:timestamp()
            }]
          )
  end.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

num_users_test_() ->
  [
    ?_assertEqual(0, num_users([])),
    ?_assertEqual(10, num_users([{1, 10}])),
    ?_assertEqual(10, num_users([{1, 10}, {2, 5}])),
    ?_assertEqual(15, num_users([{1, 10}, {2, 15}]))
  ].

estimated_num_rows_test_() ->
  [
    ?_assertEqual(0, estimated_num_rows([])),
    ?_assertEqual(10, estimated_num_rows([{1, 10}])),
    % 2*4 + 1*(10 - 4)
    ?_assertEqual(14, estimated_num_rows([{1, 10}, {2, 4}])),
    % All 1-rows users are also 2-rows users
    ?_assertEqual(20, estimated_num_rows([{1, 10}, {2, 10}])),
    % There are more 2-rows users than 1-row users (possible because of anonymization)
    ?_assertEqual(20, estimated_num_rows([{1, 5}, {2, 10}])),
    % 4*1 + 3*(5-1) + 2*(5-5) + 1*(10-5)
    ?_assertEqual(21, estimated_num_rows([{1, 10}, {2, 5}, {3, 5}, {4, 1}]))
  ].

-endif.