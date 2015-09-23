%% @doc Helper for working air etcd KV store.
-module(air_etcd).
-compile({no_auto_import,[get/1]}).

%% API
-export([
  get/1,
  set/2,
  set/3,
  delete/1,
  rmdir/1,
  ls/1,
  create_new/2,
  create_new/3,
  get_or_create/2,
  get_or_create/3
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").

-define(ETCD_TIMEOUT, 5000).

% Taken from https://github.com/coreos/etcd/blob/master/Documentation/errorcode.md
-define(ETCD_KEY_ALREADY_EXISTS, 105).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Retrieves the value under given key. Raises an error if the value under
%%      given key is not present.
-spec get(key()) -> binary().
get(Key) ->
  {ok, #get{value=Value}} = etcd:get(etcd_url(), Key, ?ETCD_TIMEOUT),
  Value.

%% @doc Just like {@link set/3} but the item never expires.
-spec set(key(), value()) -> result().
set(Key, Value) ->
  etcd:set(etcd_url(), Key, Value, ?ETCD_TIMEOUT).

%% @doc Sets the value under a given key, with the given ttl (in seconds).
-spec set(key(), value(), pos_integer()) -> result().
set(Key, Value, Ttl) ->
  etcd:set(etcd_url(), Key, Value, Ttl, ?ETCD_TIMEOUT).

%% @doc Sets the value under a given key, with the given ttl (in seconds).
-spec delete(key()) -> result().
delete(Key) ->
  etcd:delete(etcd_url(), Key, ?ETCD_TIMEOUT).

%% @doc Removes a directory folder.
-spec rmdir(key()) -> result().
rmdir(Key) ->
  etcd:delete(etcd_url(), [Key, "?recursive=true"], ?ETCD_TIMEOUT).

%% @doc Retrieves all key-value pairs which reside immediately under the given key.
-spec ls(key()) -> [{binary(), binary()}].
ls(Key) ->
  case etcd:get(etcd_url(), Key, ?ETCD_TIMEOUT) of
    {ok, #get{nodes=Nodes}} ->
      [{SubKey, Value} || #node{key=SubKey, value=Value} <- Nodes];
    _ -> []
  end.

%% @doc Same as {@link create_new/3} without TTL.
-spec create_new(key(), value()) -> ok | {error, already_exists}.
create_new(Key, Value) ->
  do_create_new(Key, Value, []).

%% @doc Creates a key, but only if it doesn't already exist.
%%      This operation is atomic. If multiple clients invoke it at the same time,
%%      only one of them will succeed.
-spec create_new(key(), value(), pos_integer()) -> ok | {error, already_exists}.
create_new(Key, Value, Ttl) ->
  do_create_new(Key, Value, [{ttl, Ttl}]).

%% @doc Same as {@link get_or_create/3} without TTL.
-spec get_or_create(key(), value()) -> binary().
get_or_create(Key, Value) ->
  handle_create_new(create_new(Key, Value), Key, Value).

%% @doc Retrieves the existing value of the key, or sets a given value if the
%%      key doesn't exist. Returns the etcd value of the key.
%%      This operation is atomic. If multiple clients invoke it at the same time,
%%      only one of them will set the value, while others will obtain that value.
-spec get_or_create(key(), value(), pos_integer()) -> binary().
get_or_create(Key, Value, Ttl) ->
  handle_create_new(create_new(Key, Value, Ttl), Key, Value).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

%% This is used for direct etcd request, which are not supported by the etcd
%% library.
%% Since there is no more mature library currently available, we're left with two
%% options: either maintain our own etcd library, or perform direct request to etcd,
%% thus bypassing etcd library. We currently choose the latter since etcd library
%% mostly works for our purposes.
etcd_req(Method, Path) -> etcd_req(Method, Path, "").

etcd_req(Method, Path, Body) ->
  Res = hackney:request(
        Method,
        iolist_to_binary([etcd_url(), "/v2/keys", Path]),
        [],
        Body,
        [
          {connect_timeout, ?ETCD_TIMEOUT},
          {recv_timeout, ?ETCD_TIMEOUT}
        ]
      ),
  decode_response(Res).

decode_response({error, _} = Error) -> Error;
decode_response({ok, _Status, _Headers, ClientRef}) ->
  {ok, ResponseBody} = hackney:body(ClientRef),
  {struct, Response} = mochijson2:decode(ResponseBody),
  {ok, Response}.

do_create_new(Key, Value, AdditionalParams) ->
  AllParams = [{prevExist, false}, {value, Value} | AdditionalParams],
  {ok, Response} = etcd_req(put, [Key, url_params(AllParams)]),
  case proplists:get_value(<<"errorCode">>, Response) of
    undefined -> ok;
    ?ETCD_KEY_ALREADY_EXISTS -> {error, already_exists}
  end.

url_params([]) -> [];
url_params(Params) ->
  List = [
    http_uri:encode(to_string(Key)) ++ "=" ++ http_uri:encode(to_string(Value))
      || {Key, Value} <- Params
  ],
  binary:list_to_bin([$? | string:join(List, "&")]).

to_string(Value) when is_integer(Value) -> integer_to_list(Value);
to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(Value) when is_list(Value) -> Value.

handle_create_new(ok, _Key, Value) -> cloak_util:binarify(Value);
handle_create_new({error, already_exists}, Key, _Value) -> get(Key).

etcd_url() ->
  case application:get_env(air_common, etcd_url) of
    {ok, EtcdUrl} -> EtcdUrl;
    undefined ->
      % Note: etcd port is always provided through OS env. This is an implementation
      % detail that allows us to reuse the code from `config/config.sh` without
      % needing to run bash script from Erlang.
      EtcdUrl = lists:flatten(io_lib:format("http://127.0.0.1:~p",
          [list_to_integer(air_utils:env("ETCD_CLIENT_PORT", undefined))])),
      application:set_env(air_common, etcd_url, EtcdUrl, [{persistent, true}]),
      EtcdUrl
  end.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

-define(assertExpiry(Key, ExpectedExpiryTime),
  (
    (fun
      ThisFun (RemainingTime) when RemainingTime > 0 ->
        case catch get(Key) of
          {'EXIT', _} -> ok;
          _ ->
            timer:sleep(100),
            ThisFun(RemainingTime - 100)
        end;

      ThisFun (_) -> ?assert(false)
    end)(ExpectedExpiryTime + timer:seconds(3))
  )).

?test_suite(standard_test_,
      setup,
      [
        ?with_applications([hackney, etcd]),
        {
          fun() -> rmdir("/tests") end,
          fun(_) -> ok end
        }
      ],
      [
        {"get non-existent", ?_assertError(_, get("/tests/non-existent key"))},
        {"set, get, delete", fun() ->
          ?assertMatch({ok, #set{key= <<"/tests/key1">>, value= <<"value1">>}}, set("/tests/key1", "value1")),
          ?assertMatch(<<"value1">>, get("/tests/key1")),
          ?assertMatch({ok,#delete{key= <<"/tests/key1">>, prevValue= <<"value1">>}}, delete("/tests/key1")),
          ?assertError(_, get("/tests/key1"))
        end},
        {"set timeout", fun() ->
          set("/tests/key2", "value2", 1),
          ?assertMatch(<<"value2">>, get("/tests/key2")),
          ?assertExpiry("/tests/key2", timer:seconds(1))
        end},
        {"ls", fun() ->
          [set("/tests/folder/" ++ Is, Is) || I <- lists:seq(1, 3), Is <- [integer_to_list(I)]],
          ?assertMatch(
                [
                  {<<"/tests/folder/1">>, <<"1">>},
                  {<<"/tests/folder/2">>, <<"2">>},
                  {<<"/tests/folder/3">>, <<"3">>}
                ],
                lists:sort(ls("/tests/folder"))
              )
        end},
        {"rmdir", fun() ->
          [set("/tests/folder2/" ++ Is, Is) || I <- lists:seq(1, 3), Is <- [integer_to_list(I)]],
          ?assertMatch({ok, #delete{key= <<"/tests/folder2">>}}, rmdir("/tests/folder2")),
          ?assertMatch([], ls("/tests/folder2"))
        end},
        {"create_new", fun() ->
          ?assertEqual(ok, create_new(<<"/tests/key3">>, <<"value3">>)),
          ?assertEqual(<<"value3">>, get("/tests/key3"))
        end},
        {"create_new timeout", fun() ->
          ?assertEqual(ok, create_new(<<"/tests/key4">>, <<"value4">>, 1)),
          ?assertExpiry("/tests/key4", timer:seconds(1))
        end},
        {"get_or_create", fun() ->
          ?assertEqual(<<"value5">>, get_or_create(<<"/tests/key5">>, <<"value5">>)),
          ?assertEqual(<<"value5">>, get("/tests/key5")),
          ?assertEqual(<<"value5">>, get_or_create(<<"/tests/key5">>, <<"value5">>)),
          ?assertEqual(<<"value5">>, get("/tests/key5"))
        end},
        {"get_or_create timeout", fun() ->
          ?assertEqual(<<"value6">>, get_or_create(<<"/tests/key6">>, <<"value6">>, 1)),
          ?assertExpiry("/tests/key6", timer:seconds(1))
        end}
      ]
    ).

-endif.
