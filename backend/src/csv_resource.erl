%% @doc HTTP API that returns task results encoded as CSV.
%%
%% This resource is somewhat inefficient as performs two
%% passes through the data to be exported.
%% The reason for this inefficiency is to avoid holding all
%% the data in memory at once.
%% The first pass constructs a set of all label-value pairs
%% that will exist in the final output, and the second pass
%% exports the data itself.
%%
%% Example (assuming nginx in front):
%%   curl http://localhost:5500/_/tasks/[TASK-ID]/results.csv
%%
-module(csv_resource).

%% webmachine callbacks
-export([
  init/1,
  allowed_methods/2,
  is_authorized/2,
  service_available/2,
  content_types_provided/2,
  to_csv/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(request, {
  user_id :: string(),
  analyst_id :: string(),
  is_authorized = false :: boolean()
}).


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) -> {ok, #request{}}.

%% @hidden
allowed_methods(Req, State) -> {['GET'], Req, State}.

% We have to validate here already since the validation
% uses the external service, which we don't know whether
% is available or not
service_available(Req, State) ->
  case authentication:validate(Req) of
    false ->
      {true, Req, State};
    {true, UserId, AnalystId} ->
      {true, Req, State#request{user_id = UserId, analyst_id = AnalystId, is_authorized = true}};
    {error, failed_connect} ->
      {false, resource_common:respond_error("Authentication service unavailable. Please try again later", Req), State}
  end.

is_authorized(Req, #request{is_authorized=false}=State) ->
  {"Not authorized", resource_common:respond_error("Not authenticated, please login", Req), State};
is_authorized(Req, #request{is_authorized=true}=State) ->
  case validate_user_can_access_task(Req, State) of
    true -> {true, Req, State};
    false -> {"Not authorized", resource_common:respond_error("Not permitted to access this task", Req), State}
  end.

content_types_provided(Req, State) ->
  {[{"text/csv", to_csv}], Req, State}.

to_csv(Req, State) ->
  {{halt, 200}, wrq:set_resp_body({stream, stream_csv(Req, State)}, Req), State}.


%% -------------------------------------------------------------------
%% Authentication
%% -------------------------------------------------------------------

%% Ensures that the analyst the user is attached to in the web,
%% also owns this particular task for which results are being downloaded.
%% @hidden
validate_user_can_access_task(Request, #request{analyst_id=AnalystId}) ->
  TaskToken = wrq:path_info(task_token, Request),
  ReturnedAnalystId = air_db:call(fun(Connection) ->
        Result = pgsql_connection:extended_query(
              "SELECT analyst_id FROM tasks WHERE token = $1",
              [TaskToken],
              Connection
            ),
        case Result of
          {{select, _}, [{TaskAnalystId}]} -> TaskAnalystId;
          _ -> none
        end
      end),
  AnalystId =:= ReturnedAnalystId.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

stream_csv(Request, _State) ->
  Self = self(),
  TaskToken = wrq:path_info(task_token, Request),
  spawn_link(fun() -> get_csv_results(Self, TaskToken, Request) end),
  receive_incoming_results().

receive_incoming_results() ->
  receive
    {data, Result} ->
      {Result, fun() -> receive_incoming_results() end};
    done ->
      {"", done}
  end.


%% -------------------------------------------------------------------
%% Processing CSV
%% -------------------------------------------------------------------

-record(task_params, {
  task_token,
  start_time,
  end_time,
  connection,
  tasks_with_error :: sets:set()
}).

get_csv_results(ReturnPid, TaskToken, Request) ->
  DbFun = fun(Connection) ->
    TaskParams = get_params(TaskToken, Request, Connection),
    Headers = get_headers(TaskParams),
    HeadersForShow = cloak_util:join([<<"time">>, <<"errors">>] ++ Headers, ","),
    ReturnPid ! {data, HeadersForShow},
    ReturnPid ! {data, "\n"},
    get_data(TaskParams, Headers, ReturnPid),
    ReturnPid ! done
  end,
  air_db:call({DbFun, infinity}).

get_params(TaskToken, Request, Connection) ->
  TaskParams = #task_params{
    task_token = TaskToken,
    start_time = start_time(TaskToken, Request, Connection),
    end_time = end_time(TaskToken, Request, Connection),
    connection = Connection
  },
  TaskParams#task_params{tasks_with_error = get_result_errors(TaskParams)}.

%% We find which results have errors in one call to avoid having
%% to make N additional calls when collecting the data for rendering.
get_result_errors(#task_params{task_token=Token, start_time=Start, end_time=End, connection=Connection}) ->
  SQL = ["
    SELECT results.id
    FROM results, tasks, exception_results
    WHERE
      tasks.token = $1 and
      results.task_id = tasks.id and
      exception_results.result_id = results.id and
      results.created_at >= $2 and
      results.created_at <= $3
    GROUP BY results.id, COUNT(exception_results)"
  ],
  {{select, _}, ErrorTasks} = pgsql_connection:extended_query(SQL, [Token, Start, End], Connection),
  lists:foldl(fun({TaskId}, AccSet) -> sets:add_element(TaskId, AccSet) end, sets:new(), ErrorTasks).

% If the start_time is provided, then we use it.
% If it isn't, then we looks for the time of the
% first result for the task. If there is no result
% yet, we choose the created_at time of the task itself.
start_time(TaskToken, Request, Connection) ->
  case wrq:get_qs_value("begin_date", Request) of
    undefined ->
      SQL = ["
        SELECT results.created_at
        FROM results, tasks
        WHERE results.task_id = tasks.id and
              tasks.token = $1
        ORDER BY results.created_at ASC
        LIMIT 1"
      ],
      Result = pgsql_connection:extended_query(SQL, [TaskToken], Connection),
      case Result of
        {{select, 0}, _} ->
          % There is no existing result...
          task_created_at_time(TaskToken, Connection);
        {{select, 1}, [{Time}]} -> Time
      end;
    Time -> Time
  end.

% If the end_date is provided, then we use it.
% If it isn't, then we choose the time of the
% very last result for the task. If there are
% no results for the task, we choose the current
% system time.
end_time(TaskToken, Request, Connection) ->
  case wrq:get_qs_value("end_date", Request) of
    undefined ->
      SQL = ["
        SELECT results.created_at
        FROM results, tasks
        WHERE results.task_id = tasks.id and
              tasks.token = $1
        ORDER BY results.created_at DESC
        LIMIT 1"
      ],
      Result = pgsql_connection:extended_query(SQL, [TaskToken], Connection),
      case Result of
        {{select, 0}, _} ->
          % There is no existing result...
          calendar:now_to_datetime(now());
        {{select, 1}, [{Time}]} -> Time
      end;
    Time -> Time
  end.

task_created_at_time(TaskToken, Connection) ->
  SQL = ["
    SELECT created_at
    FROM tasks
    WHERE tasks.token = $1"
  ],
  {{select, 1}, [{Time}]} = pgsql_connection:extended_query(SQL, [TaskToken], Connection),
  Time.

sql_for_task(#task_params{task_token=TaskToken, start_time=StartTime, end_time=EndTime}) ->
  SQL = ["
    SELECT results.id, results.created_at, results.buckets_json
    FROM results, tasks
    WHERE results.task_id = tasks.id and
          tasks.token = $1 and
          results.created_at >= $2 and
          results.created_at <= $3
    ORDER BY results.created_at ASC"
  ],
  {SQL, [TaskToken, StartTime, EndTime]}.

get_headers(#task_params{connection=Connection}=TaskParams) ->
  FoldFun = fun({_Id, _CreatedAt, Val}, Acc) ->
    Props = mochijson2:decode(Val),
    lists:foldl(fun(Struct, TitleAcc) -> sets:add_element(title_from_bucket(Struct), TitleAcc) end, Acc, Props)
  end,
  {SQL, Params} = sql_for_task(TaskParams),
  {ok, ResultSet} = pgsql_connection:fold(FoldFun, sets:new(), SQL, Params, Connection),
  lists:sort(sets:to_list(ResultSet)).

get_data(#task_params{tasks_with_error=ErrorSet, connection=Connection}=TaskParams, Headers, ReturnPid) ->
  EachFun = fun({Id, CreatedAt, Val}) ->
    Props = mochijson2:decode(Val),
    HasError = case sets:is_element(Id, ErrorSet) of
      true -> "true";
      false -> "false"
    end,
    Props = mochijson2:decode(Val),
    ValDict = lists:foldl(fun(Struct, AccDict) ->
          Title = title_from_bucket(Struct),
          Count = ej:get({"count"}, Struct),
          dict:store(Title, Count, AccDict)
        end, dict:new(), Props),
    Row = lists:map(fun(Header) ->
            case dict:find(Header, ValDict) of
              error -> "";
              {ok, Count} -> integer_to_list(Count)
            end
          end, Headers),
    ReturnableRow = cloak_util:join([format_time(CreatedAt), HasError] ++ Row, ","),
    ReturnPid ! {data, ReturnableRow},
    ReturnPid ! {data, "\n"}
  end,
  {SQL, Params} = sql_for_task(TaskParams),
  pgsql_connection:foreach(EachFun,  SQL, Params, Connection).

format_time({{Year, Month, Day}, {Hour, Minute, Second}}) when is_integer(Second) ->
  io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~p", [Year, Month, Day, Hour, Minute, Second]);
format_time({{Year, Month, Day}, {Hour, Minute, Second}}) when is_float(Second) ->
  io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~.2f", [Year, Month, Day, Hour, Minute, Second]).

title_from_bucket(Bucket) ->
  Label = ej:get({"label"}, Bucket),
  Value = ej:get({"value"}, Bucket),
  <<Label/binary, ": ", Value/binary>>.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

clear_tables() ->
  db_test_helpers:simple_query("TRUNCATE TABLE tasks"),
  db_test_helpers:simple_query("TRUNCATE TABLE results"),
  db_test_helpers:simple_query("TRUNCATE TABLE exception_results"),
  db_test_helpers:simple_query("TRUNCATE TABLE analysts").

add_task_and_analyst(Token) ->
  db_test_helpers:insert_rows("analysts", ["name"], [["Test analyst"]]),
  {{select, 1}, [{AnalystId}]} = db_test_helpers:simple_query("SELECT id FROM analysts"),
  Fields = ["token", "analyst_id", "created_at"],
  Values = [[Token, AnalystId, {{1985, 3, 11}, {5, 21, 0.0}}]],
  db_test_helpers:insert_rows("tasks", Fields, Values),
  Sql = "SELECT id FROM tasks WHERE token = $1",
  {{select, 1}, [{TaskId}]} = db_test_helpers:extended_query(Sql, [Token]),
  {AnalystId, TaskId}.

cells_json(Values) ->
  Items = create_cells_json(Values),
  "[" ++ cloak_util:join(Items, ",") ++ "]".

create_cells_json([]) -> [];
create_cells_json([Count|Rem]) ->
  CountStr = integer_to_list(Count),
  ["{\"label\":\"label" ++ CountStr ++ "\", \"value\":\"value" ++ CountStr ++ "\", \"count\":" ++ CountStr ++ "\}"
      | create_cells_json(Rem)].

?test_suite(resource_test_,
      setup,
      [
        ?load_conf,
        ?with_applications([webmachine]),
        ?with_db,
        ?with_processes([air_api_sup])
      ],
      [
        {"Validate 401 when not logged in", fun() ->
          ExpectedMessage = "{\"success\":false,\"error\":\"Not authenticated, please login\"}",
          mecked_auth(false, fun() -> verifyHttp(401, ExpectedMessage, get_result("TaskToken")) end)
        end},

        {"Validate 503 when cannot connect to auth server", fun() ->
          ExpectedMessage = "{\"success\":false,\"error\":\"Authentication service unavailable. "
              "Please try again later\"}",
          mecked_auth({error, failed_connect}, fun() -> verifyHttp(503, ExpectedMessage, get_result("TaskToken")) end)
        end},

        {"Validate 401 when task under different analyst", fun() ->
          clear_tables(),
          UserId = 1,
          Token = "Test-Token",
          {AnalystId, _TaskId} = add_task_and_analyst(Token),
          ExpectedMessage = "{\"success\":false,\"error\":\"Not permitted to access this task\"}",
          mecked_auth({true, UserId, AnalystId + 1}, fun() -> verifyHttp(401, ExpectedMessage, get_result(Token)) end)
        end},

        {"Should have valid CSV output with empty lines where needed and errors", fun() ->
          clear_tables(),
          UserId = 1,
          Token = "Test-Token",
          Time1 = {{1951, 12, 12}, {10, 10, 10}},
          Time2 = {{1958, 9, 28}, {10, 10, 10}},
          {AnalystId, TaskId} = add_task_and_analyst(Token),
          Results = [
            [TaskId, Time1, AnalystId, cells_json([1])],
            [TaskId, Time2, AnalystId, cells_json([2])]
          ],
          db_test_helpers:insert_rows("results", ["task_id", "created_at", "analyst_id", "buckets_json"], Results),
          % Insert an exception for one of the tasks to ensure that we
          % also correctly show which tasks have exceptions
          SqlException = ["
            SELECT id
            FROM results
            WHERE results.created_at = $1"
          ],
          {{select, 1}, [{ResultId}]} = db_test_helpers:extended_query(SqlException, [Time1]),
          db_test_helpers:insert_rows("exception_results", ["result_id"], [[ResultId]]),
          ExpectedResponse = lists:flatten([
            "time,errors,label1: value1,label2: value2\n"
            "" ++ format_time(Time1) ++ ",true,1,\n" ++
            "" ++ format_time(Time2) ++ ",false,,2\n"
          ]),
          mecked_auth({true, UserId, AnalystId}, fun() -> verifyHttp200(ExpectedResponse, get_result(Token)) end)
        end},

        {"Should return CSV in a time range", fun() ->
          clear_tables(),
          UserId = 1,
          Token = "Test-Token",
          Time1 = {{1951, 12, 12}, {10, 10, 10}},
          Time2 = {{1958, 9, 28}, {10, 10, 10}},
          Time3 = {{1985, 3, 11}, {10, 10, 10}},
          {AnalystId, TaskId} = add_task_and_analyst(Token),
          Results = [
            [TaskId, Time1, AnalystId, cells_json([1,2])],
            [TaskId, Time2, AnalystId, cells_json([2,3])],
            [TaskId, Time3, AnalystId, cells_json([3,4])]
          ],
          db_test_helpers:insert_rows("results", ["task_id", "created_at", "analyst_id", "buckets_json"], Results),
          ExpectedResponse = lists:flatten([
            "time,errors,label2: value2,label3: value3\n"
            "" ++ format_time(Time2) ++ ",false,2,3\n"
          ]),
          mecked_auth({true, UserId, AnalystId}, fun() ->
                verifyHttp200(ExpectedResponse, get_result(Token,
                    "begin_date=1957%2F01%2F01+12%3A00%3A00&end_date=1959%2F01%2F01+12%3A00%3A00"))
              end)
        end}
      ]
    ).

mecked_auth(AuthResponse, Fun) ->
  meck:new(authentication),
  meck:expect(authentication, validate, fun(_) -> AuthResponse end),
  Fun(),
  meck:unload().

verifyHttp(ExpectedStatusCode, ExpectedBody, HttpResponse) ->
  ?assertMatch({ok, {{_, _, _}, _, _}}, HttpResponse),
  {ok, {{_, StatusCode, _}, _, Body}} = HttpResponse,
  ?assertEqual(ExpectedStatusCode, StatusCode),
  ?assertEqual(ExpectedBody, Body).

verifyHttp200(ExpectedBody, HttpResponse) ->
  verifyHttp(200, ExpectedBody, HttpResponse).

get_result(TaskToken) ->
  get_result(TaskToken, "").

get_result(TaskToken, QueryString) ->
  httpc:request(get, {"http://127.0.0.1:11000/_/tasks/" ++ TaskToken ++ "/results.csv?" ++ QueryString, []}, [], []).

-endif.
