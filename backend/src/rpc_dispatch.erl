-module(rpc_dispatch).

%% Dispatch functions
-export([
  csv_row_based/3
]).

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


%% -------------------------------------------------------------------
%% Test dispatch functions
%% -------------------------------------------------------------------

%% @doc Function used in testing dispatching from the rails rpc resource.
%%      We need an actual function here, as meck doesn't allow mocking
%%      functions that don't exist.
test(Arguments, Request, State) ->
  Response = cloak_util:join(Arguments, ", "),
  {Response, Request, State}.
