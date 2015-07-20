-module(rpc_dispatch).

%% Dispatch functions
-export([
  csv_row_based/3,
  error/3
]).

%% Test dispatch functions
-export([
  test/3
]).


%% -------------------------------------------------------------------
%% Dispatch functions
%% -------------------------------------------------------------------

%% @doc Exports a task results for a set period of time
%%      as rows, where each reported property is its own row.
csv_row_based(Arguments, Request, State) ->
  csv_rpc:row_based_export(Arguments, Request, State).

%% @doc Re-reports a rails error message verbatim to the user
error([ErrorJson, StatusCode], Request, State) ->
  RequestWithHeader = wrq:set_resp_header("Content-Type", "application/json", Request),
  RequestWithBody = wrq:set_resp_body(ErrorJson, RequestWithHeader),
  {{halt, StatusCode}, RequestWithBody, State}.


%% -------------------------------------------------------------------
%% Test dispatch functions
%% -------------------------------------------------------------------

%% @doc Function used in testing dispatching from the rails rpc resource.
%%      We need an actual function here, as meck doesn't allow mocking
%%      functions that don't exist.
test(Arguments, Request, State) ->
  Response = cloak_util:join(Arguments, ", "),
  {Response, Request, State}.
