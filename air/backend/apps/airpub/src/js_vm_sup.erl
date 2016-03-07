%% @doc Supervisor for the JavaScript virtual machines pool.
-module(js_vm_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  call/2
]).

%% Supervisor callbacks
-export([
  init/1
]).

-define(POOL_NAME, js_vm_pool).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Executes the specified function with the supplied arguments on one of the
%%      virtual machines in the pool and returns the result.
-spec call(binary() | string(), [term()]) -> {ok, term()} | {error, term()}.
call(Function, Arguments) ->
  Worker = poolboy:checkout(?POOL_NAME, true, infinity),
  try
    gen_server:call(Worker, {call, Function, Arguments}, infinity)
  after
    poolboy:checkin(?POOL_NAME, Worker)
  end.


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

init([]) ->
  PoolSize = case erlang:system_info(logical_processors_available) of
    unknown -> 2; % if we can't determine CPU count, assume 2
    Count -> Count
  end,
  {ok, JSFolder} = application:get_env(airpub, js_folder),
  JSModules = filelib:wildcard(JSFolder ++ "/src/*.js"),
  lager:info("Starting JS VM pool with size ~p and code modules ~p.~n", [PoolSize, JSModules]),
  PoolArgs = [{name, {local, ?POOL_NAME}}, {worker_module, js_vm_worker}, {size, PoolSize}, {max_overflow, 0}],
  Pool = poolboy:child_spec(?POOL_NAME, PoolArgs, JSModules),
  {ok, {{one_for_one, 5, 10}, [Pool]}}.
