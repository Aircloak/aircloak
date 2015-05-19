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
  purge_deleted(),
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

purge_deleted() ->
  {{delete, _}, []} = air_db:call(fun(Connection) ->
        pgsql_connection:simple_query("DELETE FROM tasks WHERE deleted=true and purged=true", Connection)
      end).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

?test_suite(purger_test_,
      setup,
      [
        ?load_conf,
        ?with_db
      ],
      [
        fun() ->
          ?db_simple_query("TRUNCATE TABLE tasks"),
          ?db_insert_rows("tasks",
                ["name", "token", "deleted", "purged"],
                [
                  ["task1", "token1", false, false],
                  ["task2", "token2", false, true],
                  ["task3", "token3", true, false],
                  ["task4", "token4", true, true]
                ]
              ),
          ?assertMatch({{delete, 1}, _}, purge_deleted()),
          ?assertEqual(
                {{select, 3}, [{<<"task1">>}, {<<"task2">>}, {<<"task3">>}]},
                ?db_simple_query("SELECT name FROM tasks")
              )
        end
      ]
    ).

-endif.
