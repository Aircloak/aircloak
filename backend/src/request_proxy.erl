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
  Headers = [
    {"request-endpoint", "backend"} |
    StringifiedExisitingHeaders
  ],
  ProxyRequest = {Url, Headers},
  ?INFO("Proxying requst: ~p", [Url]),
  case httpc:request(get, ProxyRequest, [], []) of
    {ok, {_, ResponseHeaders, RawBody}} ->
      DecodedBody = case proplists:get_value("content-encoding", ResponseHeaders) of
        "gzip" -> zlib:gunzip(RawBody);
        undefined -> RawBody;
        OtherEncoding ->
          ?ERROR("Unknown encoding ~p", [OtherEncoding]),
          undefined
      end,
      case DecodedBody of
        undefined -> {error, decoding_error};
        _ ->
          ?DEBUG("Received proxy response: ~p", [DecodedBody]),
          try {ok, mochijson2:decode(DecodedBody)} catch error:_Reason -> {error, decoding_error} end
      end;
    {error, Reason} ->
      ?ERROR("Backend proxy failed with reason: ~p", [Reason]),
      {error, failed_connect}
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

query_string(Request) ->
  "?" ++ mochiweb_util:urlencode(wrq:req_qs(Request)).
