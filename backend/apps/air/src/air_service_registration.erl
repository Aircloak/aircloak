%% @doc In charge of renewing the service registration in etcd
-module(air_service_registration).
-behaviour(gen_server).

%% Internal API
-export([
  start_link/0
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").

-define(RENEW_INTERVAL, timer:seconds(10)).
-define(REGISTRATION_EXPIRY_SEC, 30).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, undefined, []).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  process_flag(trap_exit, true),
  Node = atom_to_list(node()),
  HttpHost = case os:getenv("HTTP_HOST_IP") of
    false -> "127.0.0.1";
    Value -> Value
  end,
  HttpPort = binary_to_integer(air_etcd:get("/tcp_ports/air_backend/http")),
  HttpEndPoint = iolist_to_binary(io_lib:format("~s:~p", [HttpHost, HttpPort])),
  Data = iolist_to_binary(mochijson2:encode([
        {http_endpoint, HttpEndPoint},
        {erlang_node, iolist_to_binary(Node)}
      ])),
  Key = iolist_to_binary(io_lib:format("/service_instances/backends/~s_~p", [HttpHost, HttpPort])),
  State = {Key, Data},
  renew_registration(State),
  {ok, State, ?RENEW_INTERVAL}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(timeout, State) ->
  renew_registration(State),
  {noreply, State, ?RENEW_INTERVAL};
handle_info(_, State) -> {noreply, State, ?RENEW_INTERVAL}.

%% @hidden
terminate(_Reason, {Key, _Data}) ->
  air_etcd:delete(Key),
  ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

renew_registration({Key, Data}) ->
  case air_etcd:set(Key, Data, ?REGISTRATION_EXPIRY_SEC) of
    {ok, #set{}} -> ok;
    Error -> ?ERROR("Error registering the service ~p", [Error])
  end.
