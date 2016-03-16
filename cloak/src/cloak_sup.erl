%% @doc Main supervisor for the cloak-core erlang application.
%%      Starts the full supervision hierarchy and the required
%%      top level servers.
-module(cloak_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

-define(NAME_MASTER(Name), list_to_atom(atom_to_list(Name) ++ "_master")).
-define(SUP(Name), ?SUP(Name, Name, [])).
-define(SUP(SupId, SupModule, StartArgs),
    {SupId, {SupModule, start_link, StartArgs}, permanent, infinity, supervisor, [SupModule]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(QUEUED_WORKER(I, Workers),
    {I, {queued_worker, start_link, [I, I, Workers]}, permanent, 5000, worker, [I]}).
-define(EVENT(Name), {Name, {gen_event, start_link, [{local, Name}]}, permanent, 5000, worker, [Name]}).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init(_Args) ->
  Processes = [
    ?CHILD(db_job, worker),
    ?CHILD(progress_handler, worker),
    ?SUP(global_service_sup),
    ?SUP(cloak_db_pool_sup),
    ?CHILD(cloak_db_def, worker),
    ?SUP(result_sender_sup),
    ?SUP(job_runner_sup),
    ?CHILD(resource_monitor, worker),
    ?CHILD(cron_manager, worker),
    ?QUEUED_WORKER(task_coordinator, cloak_conf:get_val(queries, concurrent_executions)),
    web_config()
  ],
  {ok, {{one_for_one, 1000000, 1}, Processes}}.

web_config() ->
  WebConfig = [
    {ip, cloak_conf:get_val(api, address)},
    {port, cloak_conf:get_val(api, port)},
    {dispatch, []}
  ],
  {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, [mochiweb_socket_server]
  }.
