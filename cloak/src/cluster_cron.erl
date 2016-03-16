%% @doc Helper for running cluster-wide jobs which have to run periodically on
%%      one machine in the cluster.
%%
%% Internally, this module installs the cron (powered by `erlcron') on every machine.
%% Then, when the cron is executing, the {@link global_service} module is used
%%  to ensure that the job is running only on a single node.
-module(cluster_cron).

%% API
-export([
  init/0,
  install/3,
  install/4,
  cancel/2,
  cancel_all_for_type/1
]).

%% Internal API
-export([
  install_local/4,
  cancel_local/2,
  run_local/3,
  cancel_all_for_type_local/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Initializes the global ETS table where job references are installed.
%%      Call this function once from the application controller or top-level
%%      supervisor.
init() ->
  ets:new(?MODULE, [set, public, named_table, {read_concurrency, true}]),
  ok.

%% @doc Works like {@link install/4}, with the empty arguments list.
-spec install(erlcron:run_when(), module(), term()) -> ok.
install(Spec, Module, Id) ->
  install(Spec, Module, Id, []).

%% @doc Installs the cron job in the cluster.
%%
%%      A job is uniquely identified with `{Module, Id}'. If there already is an
%%      installed job with these parameters, it will be canceled before the
%%      new job is installed.
%%
%%      `Spec' corresponds to the `erlcron' period specification.
%%
%%      `Module' identifies the module which implements the {@link global_service}
%%      compliant `gen_server'. Before the job is about to be executed, the
%%      function `Module:start_link(GlobalServiceKey, CronId, ...)' will be
%%      invoked, where `...' will be remaining arguments specified via `Args'.
%%      Then the function `Module:run(pid)' will be invoked to run the job.
%%
%%      After the job is finished, you may stop the server process that was running
%%      the job. However, to minimize problems that might be caused due to system
%%      time mismatch on different machines, you're advised to keep the process
%%      running. Doing so will reduce chances for irregular job executions.
-spec install(erlcron:run_when(), module(), term(), [term()]) -> ok.
install(Spec, Module, Id, Args) ->
  rpc:multicall(cloak_util:all_visible_nodes(), ?MODULE, install_local, [Spec, Module, Id, Args]),
  ok.

%% @doc Installs the cron job on the local node. Otherwise works just like
%%      {@link install/4}
-spec install_local(erlcron:run_when(), module(), term(), [term()]) -> ok.
install_local(Spec, Module, Id, Args) ->
  cancel_local(Module, Id),
  ?DEV_INFO("Installing cluster cron ~p", [{Spec, Module, Id}]),
  NewJobRef = erlcron:cron({Spec, {?MODULE, run_local, [Module, Id, Args]}}),
  ets:insert(?MODULE, {{Module, Id}, NewJobRef}),
  ok.

%% @doc Cancels the cron job in the entire cluster.
-spec cancel(module(), term()) -> ok.
cancel(Module, Id) ->
  rpc:multicall(cloak_util:all_visible_nodes(), ?MODULE, cancel_local, [Module, Id]),
  ok.

%% @doc Cancels all cron jobs connected to the given module in the entire cluster.
-spec cancel_all_for_type(module()) -> ok.
cancel_all_for_type(Module) ->
  rpc:multicall(cloak_util:all_visible_nodes(), ?MODULE, cancel_all_for_type_local, [Module]),
  ok.

%% @doc Cancels all cron jobs connected to the given module on the local node.
-spec cancel_all_for_type_local(module()) -> ok.
cancel_all_for_type_local(Module) ->
  [do_cancel_job(JobEntry) || {{M, _}, _} = JobEntry <- ets:tab2list(?MODULE), M =:= Module],
  ok.

%% -------------------------------------------------------------------
%% Internal API functions
%% -------------------------------------------------------------------

%% @hidden
run_local(Module, Id, Args) ->
  Pid = global_service:get_or_create({dcron, Module, Id}, {Module, start_link, [Id | Args]}),
  if
    node(Pid) =:= node() -> Module:run(Pid);
    true -> ok
  end.

%% @hidden
cancel_local(Module, Id) ->
  case ets:lookup(?MODULE, {Module, Id}) of
    [JobEntry] -> do_cancel_job(JobEntry);
    _ -> ok
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

do_cancel_job({{Module, Id}, JobRef}) ->
  ?DEV_INFO("Canceling cluster cron ~p", [{Module, Id}]),
  erlcron:cancel(JobRef),
  ets:delete(?MODULE, {Module, Id}),
  %% Stop the cron process in case it's still running.
  global_service:stop({dcron, Module, Id}).
