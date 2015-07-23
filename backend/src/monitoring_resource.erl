%% @doc HTTP-endpoint for monitoring purposes that allows
%%      an external monitoring agent to validate different
%%      aspects of the correct behaviour of the erlang backend.
%%
%%      The response to the check is text/plain
%%      and always one of "SUCCESS: " or "FAILURE: " with the
%%      name of the particular check following the test status.
%%      For the database connectivity check, the result is one of:
%%
%%      <ul>
%%        <li>SUCCESS: db_connectivity</li>
%%        <li>FAILURE: db_connectivity</li>
%%      </ul>
-module(monitoring_resource).

%% webmachine callbacks
-export([
  init/1,
  allowed_methods/2,
  to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) -> {ok, nostate}.

%% @hidden
allowed_methods(Request, State) -> {['GET'], Request, State}.

to_html(Request, State) ->
  RequestWithHeader = wrq:set_resp_header("Content-Type", "text/plain", Request),
  CheckName = cloak_util:stringify(wrq:path_info(action, Request)),
  Success = case CheckName of
    "db_connectivity" -> db_check_output();
    _ -> unknown_check
  end,
  case Success of
    true -> {"SUCCESS: " ++ CheckName, RequestWithHeader, State};
    false -> {"FAILURE: " ++ CheckName, RequestWithHeader, State};
    unknown_check -> {"FAILURE: check '" ++ CheckName ++ "' does not exist",
        RequestWithHeader, State}
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

db_check_output() ->
  try air_db:call(fun(C) -> pgsql_connection:simple_query("SELECT true", C) end) of
    {{select, 1}, [{true}]} -> true;
    Response ->
      lager:error("DB check FAILED. Got unexpected response: ~p", [Response]),
      false
  catch
    ProblemType:Reason ->
      lager:critical("DB check FAILED critically. Problem:: ~p:~p", [ProblemType, Reason]),
      false
  end.
