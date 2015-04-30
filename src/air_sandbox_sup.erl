%% @doc Supervisor for air-sandbox
-module(air_sandbox_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

-include("air.hrl").

%% Supervisor callbacks
-export([
  init/1
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
    {ok, Pid} ->
      air_sandbox_web:setup_routes(),
      {ok, Pid};
    Other -> Other
  end.


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) ->
  air_sandbox_state:init(),
  {ok, {{one_for_one, 5, 10}, [
    air_sandbox_web:child_spec()
  ]}}.
