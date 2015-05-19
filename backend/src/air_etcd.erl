%% @doc Helper for working air etcd KV store.
-module(air_etcd).
-compile({no_auto_import,[get/1]}).

%% API
-export([
  get/1
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Retrieves the value under given key. Raises an error if the value under
%%      given key is not present.
-spec get(string() | binary()) -> binary().
get(Key) ->
  {ok, #get{value=Value}} = etcd:get(etcd_url(), Key, 5000),
  Value.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

-ifdef(TEST).
  etcd_url() -> "http://127.0.0.1:4003".
-else.
  etcd_url() ->
    lists:flatten(io_lib:format("http://~s:~s", [env("ETCD_HOST", "127.0.0.1"), env("ETCD_PORT", "4002")])).

  env(VarName, Default) ->
    case os:getenv(VarName) of
      false -> Default;
      Value -> Value
    end.
-endif.
