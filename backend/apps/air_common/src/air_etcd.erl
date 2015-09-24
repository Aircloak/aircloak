%% @doc Helper for working air etcd KV store.
-module(air_etcd).
-compile({no_auto_import,[get/1]}).

%% API
-export([
  url/0,
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

-export_type([
  key/0
]).

-define(ETCD_TIMEOUT, 5000).

% Taken from https://github.com/coreos/etcd/blob/master/Documentation/errorcode.md
-define(ETCD_KEY_ALREADY_EXISTS, 105).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Returns the url of the etcd endpoint
-spec url() -> string().
url() ->
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

%% @doc Retrieves the value under given key. Raises an error if the value under
%%      given key is not present.
-spec get(key()) -> binary().
get(Key) ->
  {ok, #get{value=Value}} = etcd:get(url(), Key, ?ETCD_TIMEOUT),
  Value.

%% @doc Just like {@link set/3} but the item never expires.
-spec set(key(), value()) -> result().
set(Key, Value) ->
  etcd:set(url(), Key, Value, ?ETCD_TIMEOUT).

%% @doc Sets the value under a given key, with the given ttl (in seconds).
-spec set(key(), value(), pos_integer()) -> result().
set(Key, Value, Ttl) ->
  etcd:set(url(), Key, Value, Ttl, ?ETCD_TIMEOUT).

%% @doc Sets the value under a given key, with the given ttl (in seconds).
-spec delete(key()) -> result().
delete(Key) ->
  etcd:delete(url(), Key, ?ETCD_TIMEOUT).

%% @doc Removes a directory folder.
-spec rmdir(key()) -> result().
rmdir(Key) ->
  etcd:delete(url(), [Key, "?recursive=true"], ?ETCD_TIMEOUT).

%% @doc Retrieves all key-value pairs which reside immediately under the given key.
-spec ls(key()) -> [{binary(), binary()}].
ls(Key) ->
  case etcd:get(url(), Key, ?ETCD_TIMEOUT) of
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
        iolist_to_binary([url(), "/v2/keys", Path]),
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
  {ok, Response} = etcd_req(put, [Key, air_utils:url_params(AllParams)]),
  case proplists:get_value(<<"errorCode">>, Response) of
    undefined -> ok;
    ?ETCD_KEY_ALREADY_EXISTS -> {error, already_exists}
  end.

handle_create_new(ok, _Key, Value) -> cloak_util:binarify(Value);
handle_create_new({error, already_exists}, Key, _Value) -> get(Key).


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

-define(keyTest(Fun), fun() -> Fun(etcd_test_helper:unique_key()) end).

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
        {"set, get, delete", ?keyTest(fun(Key) ->
          ?assertMatch({ok, #set{key=Key, value= <<"value">>}}, set(Key, "value")),
          ?assertMatch(<<"value">>, get(Key)),
          ?assertMatch({ok,#delete{key= Key, prevValue= <<"value">>}}, delete(Key)),
          ?assertError(_, get(Key))
        end)},
        {"set timeout", ?keyTest(fun(Key) ->
          set(Key, "value", 1),
          ?assertMatch(<<"value">>, get(Key)),
          ?assertExpiry(Key, timer:seconds(1))
        end)},
        {"ls", ?keyTest(fun(Folder) ->
          FolderSize = byte_size(Folder),
          [set(binary_to_list(Folder) ++ "/" ++ Is, Is) || I <- lists:seq(1, 3), Is <- [integer_to_list(I)]],
          ?assertMatch(
                [
                  {<<Folder:FolderSize/binary, "/1" >>, <<"1">>},
                  {<<Folder:FolderSize/binary, "/2" >>, <<"2">>},
                  {<<Folder:FolderSize/binary, "/3" >>, <<"3">>}
                ],
                lists:sort(ls(Folder))
              )
        end)},
        {"rmdir", ?keyTest(fun(Folder) ->
          [set(binary_to_list(Folder) ++ "/" ++ Is, Is) || I <- lists:seq(1, 3), Is <- [integer_to_list(I)]],
          ?assertMatch({ok, #delete{key=Folder}}, rmdir(Folder)),
          ?assertMatch([], ls(Folder))
        end)},
        {"create_new", ?keyTest(fun(Key) ->
          ?assertEqual(ok, create_new(Key, <<"value">>)),
          ?assertEqual(<<"value">>, get(Key))
        end)},
        {"create_new timeout", ?keyTest(fun(Key) ->
          ?assertEqual(ok, create_new(Key, <<"value">>, 1)),
          ?assertExpiry(Key, timer:seconds(1))
        end)},
        {"get_or_create", ?keyTest(fun(Key) ->
          ?assertEqual(<<"value">>, get_or_create(Key, <<"value">>)),
          ?assertEqual(<<"value">>, get(Key)),
          ?assertEqual(<<"value">>, get_or_create(Key, <<"value">>)),
          ?assertEqual(<<"value">>, get(Key))
        end)},
        {"get_or_create timeout", ?keyTest(fun(Key) ->
          ?assertEqual(<<"value">>, get_or_create(Key, <<"value">>, 1)),
          ?assertExpiry(Key, timer:seconds(1))
        end)}
      ]
    ).

-endif.
