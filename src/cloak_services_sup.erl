%% @doc Services from the cloak application
-module(cloak_services_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

%% Supervisor callbacks
-export([
  init/1
]).

-include("air.hrl").

%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    ?SUP(global_service_sup),
    ?SUP(job_runner_sup),
    ?CHILD(cron_manager, worker)
  ]}}.
