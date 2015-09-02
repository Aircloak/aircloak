-module(airpub_app).
-behaviour(application).

-include_lib("cloak/src/cloak.hrl").

%% Application callbacks
-export([
  start/2,
  stop/1
]).


%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  {ok, Sup} = airpub_sup:start_link(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/test", cowboy_static, {file, io_lib:format("~s/test.html", [code:priv_dir(airpub)])}},
      {"/subscribe/ws", websocket_handler, []},
      {"/publish/[...]", publish_handler, []}
    ]}
  ]),
  HttpPort = binary_to_integer(air_etcd:get("/tcp_ports/airpub/http")),
  ResultsUrl = lists:flatten(io_lib:format("http://infrastructure-api.air-local:~s/results",
      [air_etcd:get("/tcp_ports/router/http")])),
  {ok, ForwardRoutes} = application:get_env(airpub, publishing_forward_routes),
  ok = application:set_env(airpub, publishing_forward_routes,
      [{"/results/", ResultsUrl, ["QueryAuthToken"]} | ForwardRoutes]),
  {ok, HttpIp} = application:get_env(airpub, http_ip),
  {ok, _} = cowboy:start_http(http, 100, [{port, HttpPort}, {ip, HttpIp}], [{env, [{dispatch, Dispatch}]}]),
  ?INFO("Airpub listening on port ~p", [HttpPort]),
  {ok, Sup}.

stop(_State) ->
  ok.
