-module(airpub_app).
-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).


%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  {ok, SharedSecretFile} = application:get_env(airpub, shared_secret_file),
  {ok, SharedSecret} = file:read_file(SharedSecretFile),
  ok = application:set_env(airpub, shared_secret, binary_to_list(SharedSecret)),
  {ok, Sup} = airpub_sup:start_link(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {file, "index.html"}},
      {"/websocket", websocket_handler, []},
      {"/publish/[...]", publish_handler, []}
    ]}
  ]),
  {ok, HttpPort} = application:get_env(airpub, http_port),
  {ok, HttpIp} = application:get_env(airpub, http_ip),
  {ok, _} = cowboy:start_http(http, 100, [{port, HttpPort}, {ip, HttpIp}], [{env, [{dispatch, Dispatch}]}]),
  {ok, Sup}.

stop(_State) ->
  ok.
