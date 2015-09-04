%% @doc gen server for orchestrating periodically scheduled integration tests
-module(integration_tests).

-behaviour(gen_server).

%% API
-export([
  setup_cron/0,
  start_link/2,
  run/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  test :: {ModuleName :: atom(), FunctionName :: atom()}
}).

-compile([{parse_transform, lager_transform}]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Sets up the cluster cron job.
-spec setup_cron() -> ok.
setup_cron() ->
  lager:info("Setting up regularly scheduled integration tests"),
  % Full fledged cluster integration test
  Id = {cluster_integration_test, run},
  Conf = air_conf:get_val(tasks, full_cluster_test),
  cluster_cron:install(Conf, ?MODULE, Id).

start_link(GlobalServiceKey, Test) ->
  gen_server:start_link({via, global_service, GlobalServiceKey}, ?MODULE, Test, []).

run(Pid) ->
  gen_server:cast(Pid, run).


%% -------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------

init(Test) ->
  lager:info("Starting integration test: ~p", [Test]),
  {ok, #state{test=Test}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(run, #state{test={Module,Function}}=State) ->
  try
    Module:Function()
  catch
    ExceptionType:ExceptionReason ->
      lager:error("Test ~p:~p failed with ~p:~p", [Module, Function, ExceptionType, ExceptionReason])
  end,
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{test=Test}) ->
  lager:info("Integration test ended: ~p", [Test]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
