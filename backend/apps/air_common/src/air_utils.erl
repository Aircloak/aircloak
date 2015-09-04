%% @doc Helper for working air etcd KV store.
-module(air_utils).

%% API
-export([
  env/2
]).

-include("air.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

env(VarName, Default) ->
  case os:getenv(VarName) of
    false -> Default;
    Value -> Value
  end.