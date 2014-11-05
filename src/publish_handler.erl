%% @doc Handler for http publish requests coming through cowboy.
%%      The format of the request should be that of a POST to http://airpub/publish/article_path,
%%      where the content is the body of the request and the content_type is provided in the http headers.
%% @end

-module(publish_handler).

%% Cowboy callbacks.
-export([
    init/2
  ]).

-include("types.hrl").


%% -------------------------------------------------------------------
%% Cowboy callbacks
%% -------------------------------------------------------------------

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req),
  ContentType = header_to_string(cowboy_req:header(<<"content-type">>, Req)),
  Path = extract_path(binary_to_list(cowboy_req:path(Req))),
  Req2 = handle_publish_request(Method, Path, HasBody, ContentType, Req),
  {ok, Req2, Opts}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Converts the http header value to a string.
-spec header_to_string(undefined | binary()) -> string().
header_to_string(undefined) ->
  "undefined";
header_to_string(Value) when is_binary(Value) ->
  binary_to_list(Value).

% Extracts the article path from the path of the http request.
-spec extract_path(string()) -> string().
extract_path("/publish" ++ Path) ->
  case Path of
    "" -> "";
    "/" -> "";
    _ -> Path
  end.

% Validates the publish request and sends it to the router.
-spec handle_publish_request(binary(), string(), boolean(), string(), term()) -> term().
handle_publish_request(<<"POST">>, "", true, _ContentType, Req) ->
  cowboy_req:reply(400, [], <<"Invalid path provided.">>, Req); % Bad request.
handle_publish_request(<<"POST">>, Path, true, ContentType, Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Article = #article{path = Path, content_type = ContentType, content = Body},
  router:publish(Article),
  cowboy_req:reply(200, [], <<>>, Req2);
handle_publish_request(<<"POST">>, _Path, false, _ContentType, Req) ->
  cowboy_req:reply(400, [], <<"Missing body.">>, Req); % Bad request.
handle_publish_request(_Method, _Path, _HasBody, _ContentType, Req) ->
  cowboy_req:reply(405, Req). % Method not allowed