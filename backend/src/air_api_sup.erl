%% @doc Supervisor for air-sandbox
-module(air_api_sup).
-behaviour(supervisor).

%% API
-export([
  start_link/0,
  setup_routes/0
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
      setup_routes(),
      {ok, Pid};
    Other -> Other
  end.

%% @doc Sets up routes for the server.
-spec setup_routes() -> ok.
setup_routes() ->
  Routes = [
    % Internal endpoints not accessible to the web
    {["task", action], air_sandbox_task_resource, []},

    % All endpoints accessible to external users need to
    % be authenticated. They are kept separate from internal
    % APIs by all being prefixed with _ in their path
    {["_", "tasks", task_token, "results.csv"], csv_resource, []}
  ],
  [webmachine_router:add_route(Route) || Route <- Routes],
  ok.


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) ->
  air_sandbox_state:init(),
  WebConfig = [{dispatch, []} | air_conf:get_section(web_server)],
  WebChildSpec = {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, [mochiweb_socket_server]
  },
  {ok, {{one_for_one, 5, 10}, [WebChildSpec]}}.
