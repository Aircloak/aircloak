%% @doc Global (cluster-wide) service which purges deleted tasks.
-module(task_purger).
-behaviour(gen_server).

%% Internal API
-export([
  setup_cron/0
]).

%% Internal API
-export([
  start_link/2,
  run/1
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


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Sets up the cluster cron job.
-spec setup_cron() -> ok.
setup_cron() ->
  cluster_cron:install(air_conf:get_val(tasks, purge_cron_spec), ?MODULE, ?MODULE).


%% -------------------------------------------------------------------
%% Internal API
%% -------------------------------------------------------------------

%% @hidden
start_link(GlobalServiceKey, ?MODULE) ->
  gen_server:start_link({via, global_service, GlobalServiceKey}, ?MODULE, undefined, []).

%% @hidden
run(Pid) ->
  gen_server:cast(Pid, run).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  {ok, undefined}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
handle_cast(run, State) ->
  purge_tasks(),
  {noreply, State};
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

purge_tasks() ->
  air_db:call({fun(Connection) ->
        {{select, _}, TaskIds} = pgsql_connection:simple_query(
              "SELECT id FROM tasks WHERE deleted=true AND purged=false AND updated_at <= now() - INTERVAL '7 days'",
              Connection
            ),
        [purge_task(TaskId, Connection) || {TaskId} <- TaskIds],
        ok
      end, timer:minutes(1)}).

purge_task(TaskId, Connection) ->
  {{delete, _}, _} = pgsql_connection:extended_query(
        "DELETE FROM exception_results USING results "
        "WHERE exception_results.result_id=results.id AND results.task_id=$1",
        [TaskId],
        Connection
      ),
  {{delete, _}, _} = pgsql_connection:extended_query(
        "DELETE FROM results WHERE task_id=$1",
        [TaskId],
        Connection
      ),
  {{delete, _}, _} = pgsql_connection:extended_query(
        "DELETE FROM pending_results WHERE task_id=$1",
        [TaskId],
        Connection
      ),
  {{update, 1}, _} = pgsql_connection:extended_query(
        "UPDATE tasks SET purged=true WHERE id=$1",
        [TaskId],
        Connection
      ).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

add_task(TaskNum, Deleted, Purged, UpdatedAt) ->
  db_test_helpers:insert_rows("tasks",
        ["name", "token", "deleted", "purged", "updated_at"],
        [["task" ++ [$0 + TaskNum], "token" ++ [$0 + TaskNum], Deleted, Purged, UpdatedAt]]
      ),
  db_test_helpers:insert_rows("results", ["task_id"], [[db_test_helpers:last_id("tasks")]]),
  db_test_helpers:insert_rows("exception_results", ["result_id"], [[db_test_helpers:last_id("results")]]),
  db_test_helpers:insert_rows("pending_results", ["task_id"], [[db_test_helpers:last_id("tasks")]]).

days_ago(Days) ->
  TargetEpoch = cloak_util:timestamp_to_epoch(os:timestamp()) - Days*24*60*60 - 10, % 10 seconds buffer
  cloak_util:timestamp_to_sql(cloak_util:epoch_to_timestamp(TargetEpoch)).

?test_suite(purger_test_,
      setup,
      [
        ?load_conf,
        ?with_db
      ],
      [
        fun() ->
          db_test_helpers:simple_query("TRUNCATE TABLE tasks"),
          db_test_helpers:simple_query("TRUNCATE TABLE results"),
          db_test_helpers:simple_query("TRUNCATE TABLE exception_results"),
          db_test_helpers:simple_query("TRUNCATE TABLE pending_results"),
          add_task(1, false, false, days_ago(7)),
          add_task(2, true, false, days_ago(0)),
          add_task(3, true, false, days_ago(6)),
          add_task(4, true, false, days_ago(7)),
          add_task(5, true, true, days_ago(7)),
          purge_tasks(),
          ?assertEqual(
                {{select, 2}, [{<<"task2">>}, {<<"task3">>}]},
                db_test_helpers:simple_query(
                      "SELECT name FROM tasks WHERE deleted=true and purged=false "
                      "ORDER BY name"
                    )
              ),
          ?assertEqual(
                {{select, 4}, [{<<"task1">>}, {<<"task2">>}, {<<"task3">>}, {<<"task5">>}]},
                db_test_helpers:simple_query(
                      "SELECT DISTINCT tasks.name FROM results INNER JOIN tasks on task_id=tasks.id "
                      "ORDER BY tasks.name"
                    )
              ),
          ?assertEqual(
                {{select, 4}, [{<<"task1">>}, {<<"task2">>}, {<<"task3">>}, {<<"task5">>}]},
                db_test_helpers:simple_query(
                      "SELECT DISTINCT tasks.name "
                      "FROM exception_results INNER JOIN results on result_id=results.id "
                      "INNER JOIN tasks on task_id=tasks.id "
                      "ORDER BY tasks.name"
                    )
              ),
          ?assertEqual(
                {{select, 4}, [{<<"task1">>}, {<<"task2">>}, {<<"task3">>}, {<<"task5">>}]},
                db_test_helpers:simple_query(
                      "SELECT DISTINCT tasks.name FROM pending_results INNER JOIN tasks on task_id=tasks.id "
                      "ORDER BY tasks.name"
                    )
              )
        end
      ]
    ).

-endif.
