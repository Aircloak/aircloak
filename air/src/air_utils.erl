%% @doc Common helpers for air backend
-module(air_utils).

%% API
-export([
  env/2,
  url_params/1
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Returns OS environment variable
-spec env(string(), string()) -> string().
env(VarName, Default) ->
  case os:getenv(VarName) of
    false -> Default;
    Value -> Value
  end.

%% @doc Encodes params for url. If the input list is non-empty, the result is prefixed with ? character.
-spec url_params([{atom() | string() | binary(), term()}]) -> binary().
url_params([]) -> [];
url_params(Params) ->
  List = [
    http_uri:encode(to_string(Key)) ++ "=" ++ http_uri:encode(to_string(Value))
      || {Key, Value} <- Params
  ],
  binary:list_to_bin([$? | string:join(List, "&")]).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

to_string(Value) when is_integer(Value) -> integer_to_list(Value);
to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(Value) when is_list(Value) -> Value.
