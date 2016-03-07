-module(request_proxy).

-include("air.hrl").

-export([
  forward_request/1,
  frontend_request/5,
  forward_headers/1
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

forward_request(IncomingRequest) ->
  case frontend_request(
        method(wrq:method(IncomingRequest)),
        wrq:disp_path(IncomingRequest) ++ query_string(IncomingRequest),
        forward_headers(IncomingRequest),
        [],
        [
          {connect_timeout, timer:seconds(10)},
          {recv_timeout, infinity}
        ]
      ) of
    {ok, _Status, _Headers, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, Body} ->
          ?DEBUG("Received proxy response: ~p", [Body]),
          try {ok, mochijson2:decode(Body)} catch error:_Reason -> {error, decoding_error} end;
        _ ->
          {error, decoding_error}
      end;
    error ->
      {error, failed_connect}
  end.

frontend_request(Method, Path, Headers, Body, Options) ->
  Url = iolist_to_binary([air_etcd:get("/service/frontend_local"), $/, Path]),
  ?INFO("Requesting from frontend: ~s", [Url]),
  Res = hackney:request(Method, Url, Headers, Body, Options),
  case Res of
    {error, Reason} ->
      ?ERROR("Error proxying to ~s: ~p", [Url, Reason]),
      error;
    _Success -> Res
  end.

forward_headers(IncomingRequest) ->
  ExistingHeaders = mochiweb_headers:to_list(wrq:req_headers(IncomingRequest)),
  StringifiedExisitingHeaders = lists:map(fun({Key, Value}) ->
        {cloak_util:stringify(Key), cloak_util:stringify(Value)}
      end, ExistingHeaders),
  % These headers may have been set by the client but we don't want them in the proxied request.
  % For example, if the Host is not dropped, we won't access the desired site.
  FilteredHeaders = [{Key, Value} ||
    {Key, Value} <- StringifiedExisitingHeaders,
    not lists:member(Key, ["Accept-Encoding", "Host", "X-Forwarded-For", "X-Forwarded-Proto", "User-Agent"])],
  [
    {"request-endpoint", "backend"} |
    FilteredHeaders
  ].

method('GET') -> get;
method('POST') -> post.

query_string(Request) ->
  "?" ++ mochiweb_util:urlencode(wrq:req_qs(Request)).
