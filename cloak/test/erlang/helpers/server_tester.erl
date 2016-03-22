%% @doc Helpers for testing resource modules.
%%
%%      Example:
%%
%%      ```
%%      resource_test_() ->
%%        {setup,
%%          fun() ->
%%            server_tester:start()
%%          end,
%%          fun(Pid) ->
%%            server_tester:stop(Pid)
%%          end,
%%          [
%%            ?_assertMatch(
%%                  {_, {{_, 200, _}, _, <<"expected response">>}},
%%                  server_tester:get("some/resource")
%%                )
%%            end
%%          ]
%%        }.
%%      '''
-module(server_tester).
-compile({no_auto_import,[put/2]}).

-ifdef(TEST).

-export([
  get/1,
  get/2,
  get/3,
  post/1,
  post/2,
  post/3,
  post/4,
  post/5,
  put/1,
  put/2,
  put/3,
  put/4,
  put/5,
  delete/1,
  delete/2,
  delete/3,
  delete/4,
  delete/5
]).

-define(TEST_SERVER_PORT, 18098).
-define(TEST_SERVER_IP, "127.0.0.1").

get(Path) -> get(Path, []).
get(Path, Headers) -> get(Path, Headers, []).
get(Path, Headers, HttpOpts) ->
  sync_http_request(get, Path, Headers, <<>>, "", HttpOpts).

post(Path) -> post(Path, []).
post(Path, Headers) -> post(Path, Headers, <<>>).
post(Path, Headers, Body) -> post(Path, Headers, Body, "").
post(Path, Headers, Body, ContentType) -> post(Path, Headers, Body, ContentType, []).
post(Path, Headers, Body, ContentType, HttpOpts) ->
  sync_http_request(post, Path, Headers, Body, ContentType, HttpOpts).

put(Path) -> put(Path, []).
put(Path, Headers) -> put(Path, Headers, <<>>).
put(Path, Headers, Body) -> put(Path, Headers, Body, "").
put(Path, Headers, Body, ContentType) -> put(Path, Headers, Body, ContentType, []).
put(Path, Headers, Body, ContentType, HttpOpts) ->
  sync_http_request(put, Path, Headers, Body, ContentType, HttpOpts).

delete(Path) -> delete(Path, []).
delete(Path, Headers) -> delete(Path, Headers, <<>>).
delete(Path, Headers, Body) -> delete(Path, Headers, Body, "").
delete(Path, Headers, Body, ContentType) -> delete(Path, Headers, Body, ContentType, []).
delete(Path, Headers, Body, ContentType, HttpOpts) ->
  sync_http_request(delete, Path, Headers, Body, ContentType, HttpOpts).

sync_http_request(Verb, Path, Headers, Body, ContentType, HttpOpts) ->
  httpc:request(Verb, req(Verb, Path, Headers, Body, ContentType), HttpOpts, [{sync, true}, {body_format, binary}]).

req(get, Path, Headers, _Body, _ContentType) -> {url(Path), Headers};
req(_, Path, Headers, Body, ContentType) -> {url(Path), Headers, ContentType, iolist_to_binary(Body)}.

url(Path) ->
  lists:concat(["http://", cloak_conf:get_val(api, address), ":",
      integer_to_list(cloak_conf:get_val(api, port)), "/", Path]).

-endif.
