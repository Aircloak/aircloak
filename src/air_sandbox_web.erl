%% @doc Helper for starting the web interface.
-module(air_sandbox_web).

%% API
-export([
  child_spec/0,
  setup_routes/0
]).

-include("air.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Makes the child specification which can be inserted into the supervisor tree.
-spec child_spec() -> supervisor:child_spec().
child_spec() ->
  {ok, WebServerConfig} = application:get_env(air, web_server),
  WebConfig = [{dispatch, []} | WebServerConfig],
  {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, [mochiweb_socket_server]
  }.

%% @doc Sets up routes for the server.
-spec setup_routes() -> ok.
setup_routes() ->
  Routes = [
    {["task", action], air_sandbox_task_resource, []}
  ],
  [webmachine_router:add_route(Route) || Route <- Routes],
  ok.
