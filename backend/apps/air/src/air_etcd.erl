%% @doc Helper for working air etcd KV store.
-module(air_etcd).
-compile({no_auto_import,[get/1]}).

%% API
-export([
  get/1,
  set/2,
  set/3,
  delete/1,
  ls/1
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").

-define(ETCD_TIMEOUT, 5000).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Retrieves the value under given key. Raises an error if the value under
%%      given key is not present.
-spec get(string() | binary()) -> binary().
get(Key) ->
  {ok, #get{value=Value}} = etcd:get(etcd_url(), Key, ?ETCD_TIMEOUT),
  Value.

%% @doc Just like {@link set/3} but the item never expires.
-spec set(string() | binary(), string() | binary()) -> result().
set(Key, Value) ->
  etcd:set(etcd_url(), Key, Value, ?ETCD_TIMEOUT).

%% @doc Sets the value under a given key, with the given ttl (in seconds).
-spec set(string() | binary(), string() | binary(), pos_integer()) -> result().
set(Key, Value, Ttl) ->
  etcd:set(etcd_url(), Key, Value, Ttl, ?ETCD_TIMEOUT).

%% @doc Sets the value under a given key, with the given ttl (in seconds).
-spec delete(string() | binary()) -> result().
delete(Key) ->
  etcd:delete(etcd_url(), Key, ?ETCD_TIMEOUT).

%% @doc Retrieves all key-value pairs which reside immediately under the given key.
-spec ls(string() | binary()) -> [{binary(), binary()}].
ls(Key) ->
  case etcd:get(etcd_url(), Key, ?ETCD_TIMEOUT) of
    {ok, #get{nodes=Nodes}} ->
      [{SubKey, Value} || #node{key=SubKey, value=Value} <- Nodes];
    _ -> []
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

etcd_url() ->
  case application:get_env(air, etcd_url) of
    {ok, EtcdUrl} -> EtcdUrl;
    undefined ->
      % Note: etcd port is always provided through OS env. This is an implementation
      % detail that allows us to reuse the code from `config/config.sh` without
      % needing to run bash script from Erlang.
      EtcdUrl = lists:flatten(io_lib:format("http://127.0.0.1:~p",
          [list_to_integer(env("ETCD_CLIENT_PORT", undefined))])),
      application:set_env(air, etcd_url, EtcdUrl, [{persistent, true}]),
      EtcdUrl
  end.

env(VarName, Default) ->
  case os:getenv(VarName) of
    false -> Default;
    Value -> Value
  end.
