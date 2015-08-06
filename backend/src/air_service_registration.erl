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
  % Need to replace @ because it's not allowed in etcd key name
  NodeNameStr = iolist_to_binary(re:replace(atom_to_list(node()), "@", "__at__", [global])),
  renew_registration(NodeNameStr),
  {ok, NodeNameStr, ?RENEW_INTERVAL}.

%% @hidden
-spec handle_call(any(), any(), any()) -> no_return().
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(timeout, NodeNameStr) ->
  renew_registration(NodeNameStr),
  {noreply, NodeNameStr, ?RENEW_INTERVAL};
handle_info(_, State) -> {noreply, State, ?RENEW_INTERVAL}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

renew_registration(NodeNameStr) ->
  Key = iolist_to_binary(io_lib:format("/services/backends/~s", [NodeNameStr])),
  {ok, #set{}} = air_etcd:set(Key, NodeNameStr, ?REGISTRATION_EXPIRY_SEC).
