%% @doc Helper for checking whether a user is validated or not
-module(authentication).

-export([
  validate/1
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

validate(IncomingRequest) ->
  Url = binary_to_list(air_etcd:get("/service/frontend/endpoint")) ++ "/infrastructure-api/authenticated",
  AuthRequest = {Url, [{"Cookie", cookie_string(IncomingRequest)}]},
  case httpc:request(get, AuthRequest, [], []) of
    {ok, {_, _Headers, RawBody}} ->
      Body = mochijson2:decode(RawBody),
      case ej:get({"authenticated"}, Body) of
        false -> false;
        true ->
          UserId = ej:get({"user_id"}, Body),
          AnalystId = ej:get({"analyst_id"}, Body),
          {true, UserId, AnalystId}
      end;
    {error, _} ->
      {error, failed_connect}
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
