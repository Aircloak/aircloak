%% @doc Top-level supervisor
-module(air_sandbox_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0
]).

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
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% Helper macro for declaring children of supervisor
-define(SUP(Name), {Name, {Name, start_link, []}, permanent, infinity, supervisor, [Name]}).

%% @hidden
init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    ?SUP(job_runner_sup),
    air_sandbox_web:child_spec()
  ]}}.
