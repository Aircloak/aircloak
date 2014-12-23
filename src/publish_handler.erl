%% @doc Handler for http publish requests coming through cowboy.
%%      The format of the request should be that of a POST to http://airpub/publish/article_path,
%%      where the content is the body of the request and the content_type is provided in the http headers.
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
  case get_forward_info(Path) of
    undefined ->
      cowboy_req:reply(200, [], <<>>, Req2);
    {ForwardUrl, ForwardHeadersNames} ->
      lager:notice("Forwarding article published on ~s to ~s", [Path, ForwardUrl]),
      ForwardHeaders = get_forward_headers(Path, ForwardHeadersNames, Req2),
      StatusCode = forward_article(ForwardUrl, ForwardHeaders, ContentType, Body),
      cowboy_req:reply(StatusCode, [], <<>>, Req2)
  end;
handle_publish_request(<<"POST">>, _Path, false, _ContentType, Req) ->
  cowboy_req:reply(400, [], <<"Missing body.">>, Req); % Bad request.
handle_publish_request(_Method, _Path, _HasBody, _ContentType, Req) ->
  cowboy_req:reply(405, Req). % Method not allowed

% Returns the forward destination for the longest matching route.
-spec get_forward_info(string()) -> undefined | {string(), [string()]}.
get_forward_info(Path) ->
  {ok, Routes} = application:get_env(airpub, publishing_forward_routes),
  {_Length, Match} = lists:foldl(fun({SubPath, Url, Headers}, {MatchLength, Match}) ->
      case lists:prefix(SubPath, Path) of
        true ->
          {string:len(SubPath), {Url, Headers}};
        false ->
          {MatchLength, Match}
      end
    end, {0, undefined}, Routes),
  Match.

% Computes the header list to forward with the article content.
-spec get_forward_headers(string(), [string()], term()) -> [tuple(string(), string())].
get_forward_headers(Path, Headers, Req) ->
  [{"Path", base64:encode_to_string(Path)} | lists:map(fun(Header) ->
        header_to_string(cowboy_req:header(list_to_binary(string:to_lower(Header)), Req))
      end, Headers)].

% Forwards the published article to another endpoint for further processing.
-spec forward_article(string(), [tuple(string(), string())], string(), binary()) -> pos_integer().
forward_article(Url, Headers, ContentType, Body) ->
  Request = {Url, Headers, ContentType, Body},
  try
    case httpc:request(post, Request, [], []) of
      {ok, {{_Version, Status, _StatusPhrase}, _Headers, _Reply}} ->
        Status;
      {error, Reason} ->
        lager:error("Failed to forward article to ~s. Reason: ~p", [Url, Reason]),
        500  % internal server error
    end
  catch
    error:ErrorReason ->
      lager:error("Failed to forward article to ~s, Reason: ~p:~p.",
        [Url, ErrorReason, erlang:get_stacktrace()]),
      500 % internal server error
  end.
