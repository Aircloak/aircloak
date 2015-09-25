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
  ContentEncoding = header_to_string(cowboy_req:header(<<"content-encoding">>, Req)),
  Path = extract_path(binary_to_list(cowboy_req:path(Req))),
  Req2 = handle_publish_request(Method, Path, HasBody, ContentType, ContentEncoding, Req),
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
-spec handle_publish_request(binary(), string(), boolean(), string(), string(), term()) -> term().
handle_publish_request(<<"POST">>, "", true, _ContentType, _ContentEcoding, Req) ->
  cowboy_req:reply(400, [], <<"Invalid path provided.">>, Req); % Bad request.
handle_publish_request(<<"POST">>, Path, true, ContentType, ContentEncoding, Req) ->
  {ok, MaxArticleSize} = application:get_env(airpub, max_article_size),
  {ok, Body, Req2} = cowboy_req:body(Req, [{length, MaxArticleSize}]),
  Article = #article{path = Path, content_type = ContentType, content_encoding = ContentEncoding, content = Body},
  router:publish(Article),
  case get_forward_info(Path) of
    undefined ->
      cowboy_req:reply(200, [], <<>>, Req2);
    {ForwardUrl, ForwardHeadersNames} ->
      lager:notice("Forwarding article published on ~s to ~s", [Path, ForwardUrl]),
      % convert published timestamp to miliseconds and forward it to the final endpoint
      {MegaSecs, Secs, MicroSecs} = Article#article.published_at,
      PublishedAtMillis = (MegaSecs * 1000 * 1000 + Secs) * 1000 + MicroSecs div 1000,
      MetadataHeaders = [{"Path", base64:encode_to_string(Path)}, {"PublishedAt", integer_to_list(PublishedAtMillis)}],
      ForwardHeaders = get_forward_headers(["Content-Encoding" | ForwardHeadersNames], Req2),
      StatusCode = forward_article(ForwardUrl, MetadataHeaders ++ ForwardHeaders, ContentType, Body),
      cowboy_req:reply(StatusCode, [], <<>>, Req2)
  end;
handle_publish_request(<<"POST">>, _Path, false, _ContentType, _ContentEncoding, Req) ->
  cowboy_req:reply(400, [], <<"Missing body.">>, Req); % Bad request.
handle_publish_request(_Method, _Path, _HasBody, _ContentType, _ContentEncoding, Req) ->
  cowboy_req:reply(405, Req). % Method not allowed

% Returns the forward destination for the longest matching route.
-spec get_forward_info(string()) -> undefined | {string(), [string()]}.
get_forward_info(Path) ->
  Routes = [
    {"/results/", infrastructure_url("results"), ["QueryAuthToken"]},
    {"/audit_log/", infrastructure_url("audit_logs"), []}
  ],
  {_Length, Match} = lists:foldl(fun({SubPath, Url, Headers}, {MatchLength, Match}) ->
      case lists:prefix(SubPath, Path) andalso string:len(SubPath) > MatchLength of
        true ->
          {string:len(SubPath), {Url, Headers}};
        false ->
          {MatchLength, Match}
      end
    end, {0, undefined}, Routes),
  Match.

infrastructure_url(Path) ->
  lists:flatten(io_lib:format("~s/~s", [
        air_etcd:get("/service/infrastructure_api_local"),
        Path
      ])).

% Builds the headers list, from the original request, to forward along with the article content.
-spec get_forward_headers([string()], term()) -> [tuple(string(), string())].
get_forward_headers(Headers, Req) ->
  GetHeaderValue = fun(HeaderName) ->
    PreppedHeaderName = list_to_binary(string:to_lower(HeaderName)),
    header_to_string(cowboy_req:header(PreppedHeaderName, Req))
  end,
  [{HeaderName, GetHeaderValue(HeaderName)} || HeaderName <- Headers].

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
