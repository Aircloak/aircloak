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
  ProxyRequest = {Url, [{"Cookie", cookie_string(IncomingRequest)}]},
  case httpc:request(get, ProxyRequest, [], []) of
    {ok, {_, _Headers, RawBody}} ->
      try {ok, mochijson2:decode(RawBody)} catch error:_Reason -> {error, decoding_error} end;
    {error, _} -> {error, failed_connect}
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% The cookie string is supposed to look like:
% name=value; name2=value2; name3=value3
cookie_string(Request) ->
  CookieProps = wrq:req_cookie(Request),
  FoldlFun = fun({Name, Value}, Cookies) -> [Name ++ "=" ++ Value | Cookies] end,
  CookieList = lists:foldl(FoldlFun, [], CookieProps),
  lists:flatten(cloak_util:join(CookieList, "; ")).

query_string(Request) ->
  "?" ++ mochiweb_util:urlencode(wrq:req_qs(Request)).
