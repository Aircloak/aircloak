%% @doc Airpub leader election.
%%      This process is started on every air node, and it is used to resolve
%%      the airpub leader. The process which is elected as the leader will
%%      start the corresponding HTTP server and register the URL in etcd.
%%      Others will just sit passively and act if the leader steps down
%%      (for example if air service is stopped on the leader node, or if the
%%      leader process crashes).
-module(airpub_leader).
-behaviour(gen_etcd_leader).

%% API functions
-export([
  start_link/0
]).

%% gen_etcd_leader callbacks
-export([
  init/1,
  handle_leader/1,
  handle_follower/1,
  handle_info/2,
  terminate/2
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the airpub leader candidate
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_etcd_leader:start_link(?MODULE, "airpub_leader", undefined).


%% -------------------------------------------------------------------
%% gen_etcd_leader callbacks
%% -------------------------------------------------------------------

init(_) ->
  {ok, undefined}.

handle_leader(State) ->
  start_airpub_server(),
  {ok, State}.

handle_follower(State) ->
  stop_airpub_server(),
  {ok, State}.

handle_info(_Message, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  stop_airpub_server().


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

start_airpub_server() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/test", cowboy_static, {file, io_lib:format("~s/test.html", [code:priv_dir(airpub)])}},
      {"/subscribe/ws", websocket_handler, []},
      {"/publish/[...]", publish_handler, []}
    ]}
  ]),
  HttpPort = binary_to_integer(air_etcd:get("/tcp_ports/airpub/http")),
  {ok, HttpIp} = application:get_env(airpub, http_ip),
  {ok, _} = cowboy:start_http(airpub_http_server, 100, [{port, HttpPort}, {ip, HttpIp}], [{env, [{dispatch, Dispatch}]}]),
  ?INFO("Airpub listening on port ~p", [HttpPort]),
  register_server().

register_server() ->
  {ok, #set{}} = air_etcd:set("/service/airpub_leader", registration_value()).

stop_airpub_server() ->
  ?INFO("Stopping airpub server"),
  cowboy:stop_listener(airpub_http_server),
  air_etcd:compare_and_delete("/service/airpub_leader", registration_value()).

registration_value() ->
  HttpHost = air_utils:env("HTTP_HOST_IP", "127.0.0.1"),
  HttpPort = binary_to_integer(air_etcd:get("/tcp_ports/airpub/http")),
  iolist_to_binary(io_lib:format("~s:~p", [HttpHost, HttpPort])).
