-module(request_proxy).

-include("air.hrl").

-export([
  forward_request/1
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

forward_request(IncomingRequest) ->
  RequestPath = wrq:disp_path(IncomingRequest),
  Url = binary_to_list(air_etcd:get("/service/frontend_local")) ++ "/" ++
      RequestPath ++ query_string(IncomingRequest),
  ExistingHeaders = mochiweb_headers:to_list(wrq:req_headers(IncomingRequest)),
  StringifiedExisitingHeaders = lists:map(fun({Key, Value}) ->
        {cloak_util:stringify(Key), cloak_util:stringify(Value)}
      end, ExistingHeaders),
  % These headers may have been set by the client but we don't want them in the proxied request.
  % For example, if the Host is not dropped, we won't access the desired site.
  FilteredHeaders = [{Key, Value} ||
    {Key, Value} <- StringifiedExisitingHeaders,
    not lists:member(Key, ["Accept-Encoding", "Host", "X-Forwarded-For", "X-Forwarded-Proto", "User-Agent"])],
  Headers = [
    {"request-endpoint", "backend"} |
    FilteredHeaders
  ],
  ProxyRequest = {Url, Headers},
  ?INFO("Proxying requst: ~p", [Url]),
  case httpc:request(get, ProxyRequest, [], []) of
    {ok, {_, _ResponseHeaders, undefined}} ->
      {error, decoding_error};
    {ok, {_, _ResponseHeaders, DecodedBody}} ->
      ?DEBUG("Received proxy response: ~p", [DecodedBody]),
      try {ok, mochijson2:decode(DecodedBody)} catch error:_Reason -> {error, decoding_error} end;
    {error, Reason} ->
      ?ERROR("Backend proxy failed with reason: ~p", [Reason]),
      {error, failed_connect}
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

query_string(Request) ->
  "?" ++ mochiweb_util:urlencode(wrq:req_qs(Request)).
