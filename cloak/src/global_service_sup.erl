%% @doc This supervisor is internally used by {@link global_service} to start
%%      and stop global services. All services are treated as temporary, which
%%      means they won't be restarted. If a service terminates, it is expected
%%      that it will be recreated on next retrieval through {@link global_service}.
-module(global_service_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_service/2,
  stop_service/1
]).

%% Supervisor callbacks
-export([
  init/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Starts the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts the service
-spec start_service(any(), {module(), atom(), [any()]}) -> {ok, pid()} | {error, term()}.
start_service(ServiceKey, {Module, Function, Args}) ->
  supervisor:start_child(
        ?MODULE,
        {
          ServiceKey,
          {Module, Function, [ServiceKey | Args]},
          temporary,
          5000,
          worker,
          [Module]
        }
      ).

%% @doc Stops the service
-spec stop_service(any()) -> ok.
stop_service(ServiceKey) ->
  supervisor:terminate_child(?MODULE, ServiceKey),
  ok.


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% @hidden
init ([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.