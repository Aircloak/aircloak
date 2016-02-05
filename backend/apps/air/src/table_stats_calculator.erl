%% @doc Calculates user table statistics
%%      This calculation is implemented as a globally registered gen_server
%%      with key being `{Analyst, TableId}'. This prevents multiple parallel
%%      stats calculations for the same table.
%%
%%      The statistics are calculated by running an async task on the cloak.
%%      This process registers on airpub and waits for the task results. Then
%%      it shapes the final result and pushes it via airpub to connected clients.
-module(table_stats_calculator).
-behaviour(gen_server).

%% Internal API
-export([
  run/2
]).

%% Internal API
-export([
  start_link/2
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
  request_headers :: [{string(), string()}],
  started_at = os:timestamp() :: erlang:timestamp()
}).

-define(PROGRESS_NOTIFICATION_INTERVAL, timer:seconds(10)).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the calculation concurrently.
-spec run({struct, [{binary(), any()}]}, any()) -> ok.
run(Arguments, RequestHeaders) ->
  Analyst = ej:get({"analyst"}, Arguments),
  TableId = ej:get({"table_id"}, Arguments),
  global_service:get_or_create(
        {?MODULE, Analyst, TableId},
        {?MODULE, start_link, [{Analyst, TableId, RequestHeaders}]}
      ),
  ok.


%% -------------------------------------------------------------------
%% Internal API
%% -------------------------------------------------------------------

%% @hidden
start_link(GlobalServiceKey, Arg) ->
  gen_server:start_link(
        {via, global_service, GlobalServiceKey},
        ?MODULE,
        Arg,
        []
      ).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init({Analyst, TableId, RequestHeaders}) ->
  % Start the task later, so we don't block the supervisor
  self() ! start_task,
  {ok, #state{analyst=Analyst, table_id=TableId, request_headers=RequestHeaders}}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(start_task, State) ->
  notify_calculating(State),
  router:add_subscriber(airpub_path("/table_stats/backend/~p/~p", [State#state.analyst, State#state.table_id])),
  case start_task(State) of
    ok ->
      % Task execution timeout is set to 30 minutes. Hopefully this should be long enough to fetch statistics
      % even for larger tables.
      erlang:send_after(timer:minutes(30), self(), task_timeout),
      {noreply, State, ?PROGRESS_NOTIFICATION_INTERVAL};
    error ->
      notify_and_store_error(State),
      {stop, normal, State}
  end;
handle_info({notify, Article}, State) ->
  handle_task_result(mochijson2:decode(post_processor:unpack_content(Article)), State),
  {stop, normal, State};
handle_info(task_timeout, State) ->
  % Task timed out
  notify_and_store_error(State),
  {stop, normal, State};
handle_info(timeout, State) ->
  % Regular heartbeat messages so we can notify clients we're processing
  notify_calculating(State),
  {noreply, State, ?PROGRESS_NOTIFICATION_INTERVAL};
handle_info(_, State) ->
  {noreply, State, ?PROGRESS_NOTIFICATION_INTERVAL}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

airpub_path(Format, Args) ->
  binary_to_list(iolist_to_binary(io_lib:format(Format, Args))).

notify_calculating(State) ->
  publish_to_airpub(State#state.analyst, State#state.table_id, [{type, <<"calculating">>}]).

publish_to_airpub(Analyst, TableId, Content) ->
  airpub_leader:publish(#article{
        path=airpub_path("/table_stats/~p/~p", [Analyst, TableId]),
        content=iolist_to_binary(mochijson2:encode(Content)),
        published_at=os:timestamp()
      }).

start_task(State) ->
  case request_proxy:frontend_request(
        post,
        iolist_to_binary(io_lib:format("user_tables/~p/run_stats_task", [State#state.table_id])),
        State#state.request_headers,
        [],
        [
          {connect_timeout, timer:seconds(10)},
          {recv_timeout, timer:seconds(30)}
        ]
      ) of
    {ok, 200, _Headers, _ClientRef} -> ok;
    Error ->
      ?ERROR("Error running stats task: ~p", [Error]),
      error
  end.

handle_task_result(Data, State) ->
  case ej:get({"success"}, Data) of
    true ->
      notify_and_store_success(Data, State);
    false ->
      ?ERROR("Error computing table stats: ~p", [ej:get({"exceptions"}, Data)]),
      notify_and_store_error(State)
  end.

notify_and_store_success(Data, State) ->
  {{Year, Month, Day}, {Hour, Minute, _Sec}} = cloak_util:timestamp_to_datetime(State#state.started_at),
  CreatedAtString = iolist_to_binary(io_lib:format("~B/~2.10.0B/~2.10.0B ~2.10.0B:~2.10.0B",
      [Year, Month, Day, Hour, Minute])),
  publish_to_airpub(State#state.analyst, State#state.table_id, [
        {type, <<"table_stats">>},
        {data, [
          {created_at, CreatedAtString},
          {num_users, ej:get({"num_users"}, Data)},
          {num_rows, ej:get({"num_rows"}, Data)}
        ]}
      ]),
  air_db:call(fun(Conn) ->
        {{insert,0,1}, [{RowId}]} = sql_conn:extended_query(
              [
                "INSERT INTO ",
                "user_table_stats(user_table_id, num_users, num_rows, success, created_at, updated_at) ",
                "VALUES($1, $2, $3, true, $4, $4) RETURNING id"
              ],
              [
                State#state.table_id,
                ej:get({"num_users"}, Data),
                ej:get({"num_rows"}, Data),
                cloak_util:timestamp_to_datetime(State#state.started_at)
              ],
              Conn
            ),
        {{delete, _}, []} = sql_conn:extended_query(
              "DELETE FROM user_table_stats WHERE user_table_id=$1 and id <> $2",
              [State#state.table_id, RowId],
              Conn
            )
      end).

notify_and_store_error(State) ->
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
      ).
