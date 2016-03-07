%% @doc Helper for working air etcd KV store.
-module(air_etcd).
-compile({no_auto_import,[get/1]}).

%% API
-export([
  url/0,
  fetch/1,
  get/1,
  set/2,
  set/3,
  delete/1,
  rmdir/1,
  ls/1,
  create_new/2,
  create_new/3,
  compare_and_swap/3,
  compare_and_swap/4,
  compare_and_delete/2
]).

-include("air.hrl").
-include_lib("etcd/include/etcd_types.hrl").

-export_type([
  key/0
]).

-define(ETCD_TIMEOUT, 5000).

% Taken from https://github.com/coreos/etcd/blob/master/Documentation/errorcode.md
-define(ETCD_KEY_NOT_FOUND, 100).
-define(ETCD_COMPARE_FAILED, 101).
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

%% @doc Looks up the value under given key.
-spec fetch(key()) -> {ok, binary()} | error.
fetch(Key) ->
  case etcd:get(url(), Key, ?ETCD_TIMEOUT) of
    {ok, #get{value=Value}} -> {ok, Value};
    {ok, #error{errorCode=?ETCD_KEY_NOT_FOUND}} -> error
  end.

%% @doc Retrieves the value under given key. Raises an error if the value under
%%      given key is not present.
-spec get(key()) -> binary().
get(Key) ->
  {ok, Value} = fetch(Key),
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
-spec create_new(key(), value()) -> ok | error.
create_new(Key, Value) ->
  etcd_put_request(Key, Value, [{prevExist, false}]).

%% @doc Creates a key, but only if it doesn't already exist.
%%      This operation is atomic. If multiple clients invoke it at the same time,
%%      only one of them will succeed.
-spec create_new(key(), value(), pos_integer()) -> ok | error.
create_new(Key, Value, Ttl) ->
  etcd_put_request(Key, Value, [{prevExist, false}, {ttl, Ttl}]).

%% @doc Same as {@link compare_and_swap/3} without TTL.
-spec compare_and_swap(key(), value(), value()) -> ok | error.
compare_and_swap(Key, Value, PrevValue) ->
  etcd_put_request(Key, Value, [{prevValue, PrevValue}]).

%% @doc Updates a key, but only if its value matches the given previous value.
-spec compare_and_swap(key(), value(), value(), pos_integer()) -> ok | error.
compare_and_swap(Key, Value, PrevValue, Ttl) ->
  etcd_put_request(Key, Value, [{prevValue, PrevValue}, {ttl, Ttl}]).

%% @doc Deletes the key if it has the given value.
-spec compare_and_delete(key(), value()) -> ok | error.
compare_and_delete(Key, PrevValue) ->
  etcd_delete_request(Key, [{prevValue, PrevValue}]).

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

etcd_put_request(Key, Value, Params) ->
  AllParams = [{value, Value} | Params],
  {ok, Response} = etcd_req(put, [Key, air_utils:url_params(AllParams)]),
  handle_etcd_response(Response).

etcd_delete_request(Key, Params) ->
  {ok, Response} = etcd_req(delete, [Key, air_utils:url_params(Params)]),
  handle_etcd_response(Response).

handle_etcd_response(Response) ->
  case proplists:get_value(<<"errorCode">>, Response) of
    undefined -> ok;
    ?ETCD_COMPARE_FAILED -> error;
    ?ETCD_KEY_ALREADY_EXISTS -> error
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
        case fetch(Key) of
          error -> ok;
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
        {"set, fetch, get, delete", ?keyTest(fun(Key) ->
          ?assertMatch({ok, #set{key=Key, value= <<"value">>}}, set(Key, "value")),
          ?assertEqual({ok, <<"value">>}, fetch(Key)),
          ?assertEqual(<<"value">>, get(Key)),
          ?assertMatch({ok,#delete{key= Key, prevValue= <<"value">>}}, delete(Key)),
          ?assertEqual(error, fetch(Key)),
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
        {"compare and swap", ?keyTest(fun(Key) ->
          ?assertEqual(ok, create_new(Key, <<"value">>)),
          ?assertEqual(ok, compare_and_swap(Key, <<"another_value">>, <<"value">>)),
          ?assertEqual(error, compare_and_swap(Key, <<"another_value">>, <<"value">>)),
          ?assertEqual(<<"another_value">>, get(Key))
        end)},
        {"compare and swap timeout", ?keyTest(fun(Key) ->
          ?assertEqual(ok, create_new(Key, <<"value">>)),
          ?assertEqual(ok, compare_and_swap(Key, <<"another_value">>, <<"value">>, 1)),
          ?assertExpiry(Key, timer:seconds(1))
        end)},
        {"compare and delete", ?keyTest(fun(Key) ->
          ?assertEqual(ok, create_new(Key, <<"value">>)),
          ?assertEqual(error, compare_and_delete(Key, <<"another_value">>)),
          ?assertEqual(ok, compare_and_delete(Key, <<"value">>)),
          ?assertEqual(error, fetch(Key))
        end)}
      ]
    ).

-endif.
