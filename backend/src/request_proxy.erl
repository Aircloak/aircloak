-module(request_proxy).

-export([
  forward_request/1
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

forward_request(IncomingRequest) ->
  RequestPath = wrq:disp_path(IncomingRequest),
  Url = binary_to_list(air_etcd:get("/service/frontend/endpoint")) ++ "/" ++
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
  lager:info("Proxying requst: ~p", [Url]),
  case httpc:request(get, ProxyRequest, [], []) of
    {ok, {_, _Headers, RawBody}} ->
      lager:debug("Received proxy response: ~p", [RawBody]),
      try {ok, mochijson2:decode(RawBody)} catch error:_Reason -> {error, decoding_error} end;
    {error, Reason} ->
      lager:error("Backend proxy failed with reason: ~p", [Reason]),
      {error, failed_connect}
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

query_string(Request) ->
  "?" ++ mochiweb_util:urlencode(wrq:req_qs(Request)).
