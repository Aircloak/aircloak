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

    % End-point allowing monitoring to perform checks from
    % the perspective of the erlang backend
    {["monitoring", action], monitoring_resource, []},

    % Generic API handler that forwards all backend requests to
    % the rails app for authentication, and to decide what
    % the erlang application should in fact do with them
    {["backend", '*'], rails_rpc_resource, []}
  ],
  [webmachine_router:add_route(Route) || Route <- Routes],
  ok.


%% -------------------------------------------------------------------
%% Supervisor callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) ->
  air_sandbox_state:init(),
  HttpPort = binary_to_integer(air_etcd:get("/tcp_ports/air_backend/http")),
  ?INFO("Starting HTTP server on port ~p", [HttpPort]),
  WebConfig = [{dispatch, []}, {port, HttpPort} | air_conf:get_section(web_server)],
  WebChildSpec = {webmachine_mochiweb,
    {webmachine_mochiweb, start, [WebConfig]},
    permanent, 5000, worker, [mochiweb_socket_server]
  },
  %% We start web and registrator under rest_for_one. That way, if web crashes,
  %% the registrator will remove the registration and crash as well.
  {ok, {{rest_for_one, 5, 10}, [
    WebChildSpec,
    ?CHILD(gen_air_service_registration, worker, [http_server_registration_data()])
  ]}}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

http_server_registration_data() ->
  HttpHost = air_utils:env("HTTP_HOST_IP", "127.0.0.1"),
  HttpPort = binary_to_integer(air_etcd:get("/tcp_ports/air_backend/http")),
  HttpEndPoint = iolist_to_binary(io_lib:format("~s:~p", [HttpHost, HttpPort])),
  {
    iolist_to_binary(io_lib:format("/service_instances/backends/~s_~p", [HttpHost, HttpPort])),
    iolist_to_binary(mochijson2:encode([{http_endpoint, HttpEndPoint}]))
  }.
