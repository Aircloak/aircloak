%% @doc This module implements a generic supervisor that can be used to start and
%%      supervise services started via {@link local_service} module. There can
%%      be only one instance of the supervisor for the single service type,
%%      which is determined by the module that powers the service.
-module(local_service_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/1,
  start_service/3,
  stop_service/2,
  pid/1
]).

%% Callbacks
-export([
  init/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the supervisor
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(ServiceModule) ->
  supervisor:start_link(?MODULE, ServiceModule).

%% @doc Starts the accumulator process
-spec start_service(atom(), local_service:key(), [any()]) -> {ok, pid()} | {error, term()}.
start_service(ServiceModule, ServiceKey, ServiceArgs) ->
  supervisor:start_child(pid(ServiceModule), [ServiceKey | ServiceArgs]).

%% @doc Stops the accumulator process
-spec stop_service(atom(), pid()) -> ok | {error, any()}.
stop_service(ServiceModule, Pid) ->
  supervisor:terminate_child(pid(ServiceModule), Pid).

%% @doc Returns the pid of the supervisor responsible for the service.
-spec pid(atom()) -> pid() | undefined.
pid(ServiceModule) ->
  gproc:where(name(ServiceModule)).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

name(ServiceModule) ->
  {n, l, {local_service_supervisor, ServiceModule}}.


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% @hidden
init (ServiceModule) ->
  % Normally, we can pass `{via, gproc, name(...)}` to `supervisor:start_link`. However, in R16 this is
  % not supported in a supervisor typespec, so dialyzer complains. Thus, we register the supervisor manually
  % in the initialization.
  gproc:reg(name(ServiceModule)),
  {ok, {{simple_one_for_one, 10, 10}, [
    {undefined, {ServiceModule, start_link, []},
    temporary, 5000, worker, [ServiceModule]}
  ]}}.
