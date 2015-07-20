%% @doc Generic HTTP API endpoint that handles requests
%%      to the backend that benefit from Erlang ability
%%      to have long and concurrent requests.
%%
%%      It relies on the authentication and business logic
%%      present in rails to determine what action to perform.
%%
%%      The general flow is as follows:
%%
%%      client ---> rails_rpc_resourcec ---> rails
%%             (request)                (forwarded request)
%%      client <--- rails_rpc_resourcec <--- rails
%%             (response)               (rpc specification)
%%
%%      Any request sent to /backend/* will be forwarded to
%%      rails under /*.
%%
%%      The resource only handles GET requests at the moment,
%%      but will be extended to support POST, and DELETE requests
%%      as needed.
%%
%%      The RPC response from the rails backend should be a JSON blob:
%%
%%      {
%%        "rpc": <dispatch-name>,
%%        "arguments": [<argument1>, <argument2>, ...]
%%      }
%%
%%      This resource will in turn call:
%%
%%      rpc_dispatch:<dispatch-name>([list of arguments], Request, State).
%%
%%      The resource is responsible for producing a valid response
%%      as per the webmachine requirements.
-module(rails_rpc_resource).

%% webmachine callbacks
-export([
  init/1,
  allowed_methods/2,
  service_available/2,
  to_html/2
]).

-record(request, {
  rpc_payload
}).

-include_lib("webmachine/include/webmachine.hrl").


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) -> {ok, #request{}}.

%% @hidden
allowed_methods(Request, State) -> {['GET'], Request, State}.

% We have to validate here already since the validation
% uses the external service, which we don't know whether
% is available or not
service_available(Request, State) ->
  case request_proxy:forward_request(Request) of
    {ok, RPCPayload} ->
      {true, Request, State#request{rpc_payload=RPCPayload}};
    {error, failed_connect} ->
      {false, resource_common:respond_error(backend_failure_description(), Request), State};
    {error, decoding_error} ->
      {false, resource_common:respond_error(decoding_failure_description(), Request), State}
  end.

to_html(Request, #request{rpc_payload=RPCPayload}=State) ->
  case ej:get({"rpc"}, RPCPayload) of
    undefined ->
      {{halt, 500}, resource_common:respond_error(invalid_rpc_failure_description(), Request), State};
    BinaryDispatchName ->
      DispatchName = binary_to_atom(BinaryDispatchName, utf8),
      Arguments = case ej:get({"arguments"}, RPCPayload) of
        undefined -> [];
        Args -> Args
      end,
      try rpc_dispatch:DispatchName(Arguments, Request, State)
      catch
        Some:Problem ->
          io:format("Attempted requested RPC call: rpc_dispatch:~p/3" ++
              " with arguments ~p. Failed with ~p:~p~n",
              [DispatchName, Arguments, Some, Problem]),
          {{halt, 500}, resource_common:respond_error(invalid_rpc_failure_description(), Request), State}
      end
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

backend_failure_description() ->
  "Temporary problems due to internal system failure. Please try again later".

decoding_failure_description() ->
  "Unexpected response from backend system (1)".

invalid_rpc_failure_description() ->
  "Unexpected response from backend system (2)".


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

?test_suite(resource_test_,
      setup,
      [
        ?load_conf,
        ?with_applications([webmachine]),
        ?with_processes([air_api_sup])
      ],
      [
        {"Returns a 503 when receiving invalid response from rails", fun() ->
          ExpectedMessage = "{\"success\":false,\"error\":\"" ++ decoding_failure_description() ++ "\"}",
          mecked_auth({error, decoding_error}, fun() -> verifyHttp(503, ExpectedMessage, get_path("some/path")) end)
        end},

        {"Returns a 503 when cannot connect to rails", fun() ->
          ExpectedMessage = "{\"success\":false,\"error\":\"" ++ backend_failure_description() ++ "\"}",
          mecked_auth({error, failed_connect}, fun() -> verifyHttp(503, ExpectedMessage, get_path("some/path")) end)
        end},

        {"Returns a 500 when RPC module is missing from payload", fun() ->
          ExpectedResponse = "{\"success\":false,\"error\":\"" ++ invalid_rpc_failure_description() ++ "\"}",
          RPCPayload = {ok, mochijson2:decode("
                {
                  \"arguments\": [\"hello\", \"world\"]
                }
              ")},
          mecked_auth(RPCPayload, fun() -> verifyHttp(500, ExpectedResponse, get_path("some/path")) end)
        end},

        {"Returns a 500 when the RPC module doesn't exist", fun() ->
          ExpectedResponse = "{\"success\":false,\"error\":\"" ++ invalid_rpc_failure_description() ++ "\"}",
          RPCPayload = {ok, mochijson2:decode("
                {
                  \"rpc\": \"bogus_dispatch_name\",
                  \"arguments\": []
                }
              ")},
          mecked_auth(RPCPayload, fun() -> verifyHttp(500, ExpectedResponse, get_path("some/path")) end)
        end},

        {"Should have valid response for valid call", fun() ->
          ExpectedResponse = "hello, world",
          RPCPayload = {ok, mochijson2:decode("
                {
                  \"rpc\": \"test\",
                  \"arguments\": [\"hello\", \"world\"]
                }
              ")},
          mecked_auth(RPCPayload, fun() -> verifyHttp(200, ExpectedResponse, get_path("some/path")) end)
        end},

        {"Should not require arguments, and default to an empty list of args", fun() ->
          ExpectedResponse = "",
          RPCPayload = {ok, mochijson2:decode("
                {
                  \"rpc\": \"test\"
                }
              ")},
          mecked_auth(RPCPayload, fun() -> verifyHttp(200, ExpectedResponse, get_path("some/path")) end)
        end}
      ]
    ).

mecked_auth(Response, Fun) ->
  meck:new(request_proxy),
  meck:expect(request_proxy, forward_request, fun(_) -> Response end),
  Fun(),
  meck:unload(request_proxy).

verifyHttp(ExpectedStatusCode, ExpectedBody, HttpResponse) ->
  ?assertMatch({ok, {{_, _, _}, _, _}}, HttpResponse),
  {ok, {{_, StatusCode, _}, _, Body}} = HttpResponse,
  ?assertEqual(ExpectedStatusCode, StatusCode),
  ?assertEqual(ExpectedBody, Body).

get_path(Path) ->
  httpc:request(get, {"http://127.0.0.1:11000/backend/" ++ Path, []}, [], []).

-endif.
