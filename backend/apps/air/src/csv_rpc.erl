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
%%   curl http://localhost:5500/_/tasks/[TASK-ID]/single_result.csv?result_id=[RESULT_ID]
%%
-module(csv_rpc).

%% Dispatch API
-export([
  row_based_export/3,
  single_export/3
]).

-include_lib("air.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Exports the requested time frame as CSV to the client.
row_based_export(Arguments, Request, State) ->
  CSVRequest = wrq:set_resp_header("Content-Type", "text/csv", Request),
  StreamingRequest = wrq:set_resp_body({stream, stream_csv(Arguments)}, CSVRequest),
  {{halt, 200}, StreamingRequest, State}.

%% @doc Exports the requested result as CSV to the client.
single_export(Arguments, Request, State) ->
  HeaderRequest = wrq:set_resp_header("Content-Type", "text/csv", Request),
  {TimeStamp, HasErrors, Buckets} = get_single_result(Arguments),
  % format buckets into CSV rows
  Rows = [[$\n, title_from_bucket(Bucket), $,, integer_to_list(ej:get({"count"}, Bucket))] || Bucket <- Buckets],
  % append buckets to result metadata
  Body = [<<"sep=,\nTime,">>, TimeStamp, <<"\nErrors,">>, format_errors(HasErrors), Rows],
  % write body
  BodyRequest = wrq:set_resp_body(Body, HeaderRequest),
  {{halt,200}, BodyRequest, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

stream_csv(Arguments) ->
  Self = self(),
  spawn_link(fun() -> get_csv_results(Self, Arguments) end),
  % European versions of Excel default to semicolon as a separator instead of comma
  % (because comma is used for numbers) so we need to specify the separator explicitly
  % if we want to import the resulting file into Excel without any changes
  % this is non-standard and could break some CSV processing tools,
  % but users using other tools should be smart enough to fix this by themselves.
  % NOTICE: We send the first bit of data immediately, rather than waiting for the
  % first line to complete. This the timeout in nginx is reset, and we have more
  % time to gather the complete set of headers.
  {"sep=,\n", fun() -> receive_metadata() end}.

receive_metadata() ->
  receive
    {metadata, Headers, ErrorSet} ->
      HeadersForShow = cloak_util:join([<<"Time">>, <<"Errors">>] ++ Headers, ","),
      {HeadersForShow ++ "\n", fun() -> receive_incoming_results(Headers, ErrorSet) end};
    {error, {Error, Reason}} ->
      ?ERROR("CSV export failed gathering headers and errors: ~p:~p", [Error, Reason]),
      {"Error, true, Aircloak: Due to an internal error we were unable to export your CSV. Sorry!", done}
  end.

receive_incoming_results(Headers, ErrorSet) ->
  receive
    {data, RawResult} ->
      Result = try
        process_raw_rows(Headers, ErrorSet, RawResult)
      catch
        Error:Reason ->
          ?ERROR("Processing raw result failed: ~p. Reason: ~p:~p", [RawResult, Error, Reason]),
          "Error, true, Aircloak: Error processing this row. Sorry!"
      end,
      {Result ++ "\n", fun() -> receive_incoming_results(Headers, ErrorSet) end};
    {error, {Error, Reason}} ->
      ?ERROR("CSV export failed while trying to collect data: ~p:~p", [Error, Reason]),
      {"Error, true, Aircloak: Due to an internal error we were unable to export your CSV. Sorry!", done};
    done ->
      {"", done}
  end.

get_single_result([TaskToken, ResultId]) ->
  DbFun = fun(Connection) ->
    SQL = ["
      SELECT results.created_at, EXISTS(SELECT TRUE FROM exception_results WHERE result_id = $2), results.buckets_json
      FROM results, tasks
      WHERE tasks.token = $1 AND results.task_id = tasks.id AND results.id = $2"
    ],
    {{select, _}, [Result]} = pgsql_connection:extended_query(SQL, [TaskToken, ResultId], Connection),
    Result
  end,
  {TimeStamp, HasErrors, BucketsJSON} = air_db:call({DbFun, infinity}),
  Buckets = case mochijson2:decode(BucketsJSON) of
    null -> [];
    Other -> Other
  end,
  {format_time(TimeStamp), HasErrors, Buckets}.


%% -------------------------------------------------------------------
%% Processing CSV
%% -------------------------------------------------------------------

-record(task_params, {
  task_token,
  start_time,
  end_time,
  connection
}).

get_csv_results(ReturnPid, Arguments) ->
  DbFun = fun(Connection) ->
    try
      TaskParams = get_params(Arguments, Connection),
      ReturnPid ! {metadata, get_headers(TaskParams), get_result_errors(TaskParams)},
      get_data(TaskParams, ReturnPid),
      ReturnPid ! done
    catch Error:Reason ->
      ReturnPid ! {error, {Error, Reason}}
    end
  end,
  air_db:call({DbFun, infinity}).

get_params([TaskToken, StartTime, EndTime], Connection) ->
  #task_params{
    task_token = TaskToken,
    start_time = binary_to_list(StartTime),
    end_time = binary_to_list(EndTime),
    connection = Connection
  }.

%% We find which results have errors in one call to avoid having
%% to make N additional calls when collecting the data for rendering.
get_result_errors(#task_params{task_token=Token, start_time=Start, end_time=End, connection=Connection}) ->
  SQL = ["
    SELECT results.id
    FROM results, tasks, exception_results
    WHERE
      tasks.token = $1 AND
      results.task_id = tasks.id AND
      exception_results.result_id = results.id AND
      results.created_at BETWEEN $2 AND $3
    GROUP BY results.id, COUNT(exception_results)"
  ],
  {{select, _}, ErrorTasks} = pgsql_connection:extended_query(SQL, [Token, Start, End], Connection),
  lists:foldl(fun({TaskId}, AccSet) -> sets:add_element(TaskId, AccSet) end, sets:new(), ErrorTasks).

sql_for_task(#task_params{task_token=TaskToken, start_time=StartTime, end_time=EndTime}) ->
  SQL = ["
    SELECT results.id, results.created_at, results.buckets_json
    FROM results, tasks
    WHERE results.task_id = tasks.id AND
          tasks.token = $1 AND
          results.created_at BETWEEN $2 AND $3
    ORDER BY results.created_at ASC"
  ],
  {SQL, [TaskToken, StartTime, EndTime]}.

get_headers(#task_params{connection=Connection}=TaskParams) ->
  FoldFun = fun({_Id, _CreatedAt, Val}, Acc) ->
    Props = case mochijson2:decode(Val) of
      null -> [];
      Other -> Other
    end,
    lists:foldl(fun(Struct, TitleAcc) -> sets:add_element(title_from_bucket(Struct), TitleAcc) end, Acc, Props)
  end,
  {SQL, Params} = sql_for_task(TaskParams),
  {ok, ResultSet} = pgsql_connection:fold(FoldFun, sets:new(), SQL, Params, Connection),
  lists:sort(sets:to_list(ResultSet)).

get_data(#task_params{connection=Connection}=TaskParams, ReturnPid) ->
  EachFun = fun(Row) -> ReturnPid ! {data, Row} end,
  {SQL, Params} = sql_for_task(TaskParams),
  pgsql_connection:foreach(EachFun,  SQL, Params, Connection).

process_raw_rows(Headers, ErrorSet, {Id, CreatedAt, Val}) ->
  Errors = format_errors(sets:is_element(Id, ErrorSet)),
  Props = case mochijson2:decode(Val) of
    null -> [];
    Other -> Other
  end,
  ValDict = lists:foldl(fun(Struct, AccDict) ->
        Title = title_from_bucket(Struct),
        Count = ej:get({"count"}, Struct),
        dict:store(Title, Count, AccDict)
      end, dict:new(), Props),
  Row = lists:map(fun(Header) ->
          case dict:find(Header, ValDict) of
            error -> "";
            {ok, Count} -> cloak_util:stringify(Count)
          end
        end, Headers),
  cloak_util:join([format_time(CreatedAt), Errors] ++ Row, ",").

format_time({{Year, Month, Day}, {Hour, Minute, Second}}) when is_integer(Second) ->
  io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second]);
format_time({{Year, Month, Day}, {Hour, Minute, Second}}) when is_float(Second) ->
  io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~5.2.0f", [Year, Month, Day, Hour, Minute, Second]).

title_from_bucket(Bucket) ->
  Label = ej:get({"label"}, Bucket),
  Value = ej:get({"value"}, Bucket),
  <<Label/binary, ": ", Value/binary>>.

format_errors(true) ->
  <<"present">>;
format_errors(false) ->
  <<"none">>.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

clear_tables() ->
  SQL = ["
        TRUNCATE TABLE tasks;
        TRUNCATE TABLE results;
        TRUNCATE TABLE exception_results;
        TRUNCATE TABLE analysts;
      "],
  db_test_helpers:simple_query(SQL).

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
        ?with_applications([webmachine, etcd]),
        ?with_db,
        ?with_processes([air_api_sup])
      ],
      [
        {timeout, 30, {"Should have valid CSV output with empty lines where needed and errors", fun() ->
          clear_tables(),
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
            "sep=,\nTime,Errors,label1: value1,label2: value2\n"
            "" ++ format_time(Time1) ++ ",present,1,\n" ++
            "" ++ format_time(Time2) ++ ",none,,2\n"
          ]),
          Args = [Token, "1900/01/01 12:00:00", "2050/01/01 12:00:00"],
          mecked_backend(Args, fun() -> verifyHttp(ExpectedResponse, get_result(Token)) end)
        end}},

        {timeout, 30, {"Should return CSV in a time range", fun() ->
          clear_tables(),
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
            "sep=,\nTime,Errors,label2: value2,label3: value3\n"
            "" ++ format_time(Time2) ++ ",none,2,3\n"
          ]),
          Args = [Token, "1957/01/01 12:00:00", "1959/01/01 12:00:00"],
          mecked_backend(Args, fun() ->
                verifyHttp(ExpectedResponse, get_result(Token,
                    "begin_date=1957%2F01%2F01+12%3A00%3A00&end_date=1959%2F01%2F01+12%3A00%3A00"))
              end)
        end}}
      ]
    ).

mecked_backend(Args, Fun) ->
  meck:new(request_proxy),
  RPCPayload = {ok, {struct, [
    {<<"rpc">>, <<"csv_row_based">>},
    {<<"arguments">>, lists:map(fun cloak_util:binarify/1, Args)}
  ]}},
  meck:expect(request_proxy, forward_request, fun(_) -> RPCPayload end),
  Fun(),
  meck:unload(request_proxy).

verifyHttp(ExpectedBody, HttpResponse) ->
  ?assertMatch({ok, {{_, 200, _}, _, _}}, HttpResponse),
  {ok, {_, _, Body}} = HttpResponse,
  ?assertEqual(ExpectedBody, Body).

get_result(TaskToken) ->
  get_result(TaskToken, "").

get_result(TaskToken, QueryString) ->
  httpc:request(get, {db_test_helpers:http_url("/backend/tasks/" ++ TaskToken ++ "/results.csv?" ++ QueryString), []}, [], []).

-endif.
