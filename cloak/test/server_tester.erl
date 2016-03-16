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
  start/0,
  stop/1,
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

start() ->
  with_tty_off(fun() -> {ok, _} = application:ensure_all_started(webmachine) end),
  WebConfig = [
    {name, test_server},
    {ip, "127.0.0.1"},
    {port, ?TEST_SERVER_PORT},
    {dispatch, []}
  ],
  {ok, Pid} = webmachine_mochiweb:start(WebConfig),
  cloak_app:add_webmachine_routes(),
  Pid.

stop(Pid) ->
  unlink(Pid),
  exit(Pid, kill).

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
  lists:concat(["http://", ?TEST_SERVER_IP, ":", integer_to_list(?TEST_SERVER_PORT), "/", Path]).

with_tty_off(Fun) ->
  Tty = case [Handler || Handler <- gen_event:which_handlers(error_logger), Handler =:= error_logger_tty_h] of
    [] -> false;
    _ -> true
  end,
  error_logger:tty(false),
  try
    Fun()
  after
    error_logger:tty(Tty)
  end.

-endif.
