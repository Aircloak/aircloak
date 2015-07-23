%% @doc RPC handler for exporting results through the API.
%%      This rpc handler streams individual results to the client
%%      as they are fetched from the database. As a result,
%%      quite large result sets can be downloaded, without causing
%%      large memory spikes on the server.
%%
%%      Potential problem:
%%      - if the consumer downloading a large dataset is very slow,
%%        we might end up effectively keeping most of the data in
%%        memory at the same time. This is something that can
%%        be addressed if it turns out to be a problem in reality.
-module(task_results_rpc).

%% API
-export([
  as_json/3
]).


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

as_json([TaskId, PageNum, PerPage, BeginTime, EndTime], Request, State) ->
  JsonRequest = wrq:set_resp_header("Content-Type", "application/json", Request),
  PreppedArgs = [TaskId, PageNum, PerPage, cloak_util:stringify(BeginTime), cloak_util:stringify(EndTime)],
  StreamingRequest = wrq:set_resp_body({stream, stream_json(PreppedArgs)}, JsonRequest),
  {{halt, 200}, StreamingRequest, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

%% @doc Streams the results of a task individually to the user.
%%      We manually construct the resulting JSON, rather than
%%      exporting the result through a JSON encoder, in order
%%      to not have to rely on a complex SAX parser, or
%%      have all results in memory at once.
stream_json([_TaskId, PageCount, PerPage | _]=Arguments) ->
  Self = self(),
  %% Manually constructed JSON preamble
  JsonStart = "{
      \"success\": true,
      \"page\": " ++ integer_to_list(PageCount) ++ ",
      \"per_page\": " ++ integer_to_list(PerPage) ++ ",",
  spawn_link(fun() -> get_json_results(Self, Arguments) end),
  {JsonStart, fun() -> receive_results(awaiting_first_row) end}.

receive_results(RequestType) ->
  receive
    {count, Count} ->
      Json = " \"count\": " ++ integer_to_list(Count) ++ ", \"results\": [",
      {Json, fun() -> receive_results(RequestType) end};
    {data, RawResult, RawExceptions} ->
      Result = lists:flatten(process_raw_result(RawResult, RawExceptions)),
      case RequestType of
        awaiting_first_row ->
          {Result, fun() -> receive_results(subsequent_result) end};
        _ ->
          {", " ++ Result, fun() -> receive_results(RequestType) end}
      end;
    done ->
      {"]}", done}
  end.


%% -------------------------------------------------------------------
%% Processing JSON
%% -------------------------------------------------------------------

get_json_results(ReturnPid, Arguments) ->
  DbFun = fun(Connection) ->
    ResultsCount = get_results_count(Arguments, Connection),
    ReturnPid ! {count, ResultsCount},
    get_results(ReturnPid, Arguments, Connection),
    ReturnPid ! done
  end,
  air_db:call({DbFun, infinity}).

get_results_count([TaskId, _PageNum, _PerPage, BeginTime, EndTime], Connection) ->
  SQL = ["
    SELECT COUNT(*)
    FROM results
    WHERE
      results.task_id = $1 AND
      results.created_at BETWEEN $2 AND $3"
  ],
  Params = [TaskId, BeginTime, EndTime],
  {{select, 1}, [{Count}]} = pgsql_connection:extended_query(SQL, Params, Connection),
  Count.

get_results(ReturnPid, [TaskId, PageNum, PerPage, BeginTime, EndTime], Connection) ->
  ResultSQL = ["
    SELECT id, created_at, buckets_json
    FROM results
    WHERE
      results.task_id = $1 AND
      results.created_at BETWEEN $2 AND $3
    ORDER BY results.created_at DESC
    LIMIT $4
    OFFSET $5"
  ],
  ExceptionSQL = ["
    SELECT id, count, stacktrace
    FROM exception_results
    WHERE
      result_id = $1"
  ],
  ResultParams = [TaskId, BeginTime, EndTime, PerPage, ((PageNum-1) * PerPage)],
  air_db:call({fun(ExceptionConn) ->
        EachFun = fun({ResultId, _CreatedAt, _BucketsJson}=Result) ->
              {{select, _}, Exceptions} = pgsql_connection:extended_query(ExceptionSQL,
                  [ResultId], ExceptionConn),
              ReturnPid ! {data, Result, Exceptions}
            end,
        pgsql_connection:foreach(EachFun, ResultSQL, ResultParams, Connection)
      end, infinity}).

process_raw_result({_ResultId, CreatedAt, BucketsJson}, Exceptions) ->
  "{
    \"published_at\": " ++ date_time_to_epoch(CreatedAt) ++ ",
    \"buckets\": " ++ cloak_util:stringify(BucketsJson) ++ ",
    \"exceptions\": [" ++ lists:flatten(cloak_util:join(format_exceptions(Exceptions), ", ")) ++ "]}".

date_time_to_epoch({Date, {H, M, S}}) ->
  DateTime = {Date, {H, M, trunc(S)}},
  GregorianSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
  EpochSeconds = GregorianSeconds - 62167219200, % Seconds between year 0 and 1970
  integer_to_list(EpochSeconds).

format_exceptions(Exceptions) ->
  lists:map(fun({Id, Count, StackTrace}) ->
      [
        "{
          \"id\": " ++ integer_to_list(Id) ++ ",
          \"error\": \"" ++ cloak_util:stringify(StackTrace) ++ "\",
          \"count\": " ++ integer_to_list(Count) ++ "
        }"
      ]
    end, Exceptions).


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

clear_tables() ->
  db_test_helpers:simple_query("TRUNCATE TABLE results; TRUNCATE TABLE exception_results").

create_result(TaskId, CreatedAt, Labels) ->
  BucketsJson = cells_json(Labels),
  Params = [TaskId, CreatedAt, BucketsJson],
  db_test_helpers:insert_rows("results", ["task_id", "created_at", "buckets_json"], [Params]),
  FindIdSql = "SELECT id FROM results WHERE buckets_json = $1 AND created_at = $2",
  FindIdParams = [BucketsJson, CreatedAt],
  air_db:call(fun(C) ->
        {{select, 1}, [{ResultId}]} = pgsql_connection:extended_query(FindIdSql, FindIdParams, C),
        ResultId
      end).

create_exception(ResultId, Count, StackTrace) ->
  Params = [Count, ResultId, StackTrace],
  db_test_helpers:insert_rows("exception_results", ["count", "result_id", "stacktrace"], [Params]),
  FindIdSql = "SELECT id FROM exception_results WHERE stacktrace = $1 AND result_id = $2",
  FindIdParams = [StackTrace, ResultId],
  air_db:call(fun(C) ->
        {{select, 1}, [{ExceptionId}]} = pgsql_connection:extended_query(FindIdSql, FindIdParams, C),
        ExceptionId
      end).

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
        {"Should return a valid response even when no results exist", fun() ->
          clear_tables(),
          TaskId = 1,
          ExpectedResponse = mochijson2:encode({struct, [
                {<<"success">>, true},
                {<<"page">>, 1},
                {<<"per_page">>, 100},
                {<<"count">>, 0},
                {<<"results">>, []}
              ]}),
          Args = [TaskId, 1, 100, <<"19000101 12:00:00">>, <<"20500101 12:00:00">>],
          mecked_backend(Args, fun() -> verifyHttp(ExpectedResponse, get_result()) end)
        end},

        {"Should return a result if there is one", fun() ->
          clear_tables(),
          TaskId = 1,
          Time = {{1985, 11, 03}, {5, 21, 00}},
          ResultId = create_result(TaskId, Time, [1,2]),
          StackTrace = "stacktrace",
          ExceptionId = create_exception(ResultId, 1, StackTrace),
          ExpectedResponse = mochijson2:encode({struct, [
                {<<"success">>, true},
                {<<"page">>, 1},
                {<<"per_page">>, 100},
                {<<"count">>, 1},
                {<<"results">>, [
                  {struct, [
                    {<<"published_at">>, list_to_integer(date_time_to_epoch(Time))},
                    {<<"buckets">>, mochijson2:decode(cells_json([1,2]))},
                    {<<"exceptions">>, [mochijson2:decode(format_exceptions([{ExceptionId, 1, StackTrace}]))]}
                  ]}
                ]}
              ]}),
          Args = [TaskId, 1, 100, <<"1900/01/01 12:00:00">>, <<"2050/01/01 12:00:00">>],
          mecked_backend(Args, fun() -> verifyHttp(ExpectedResponse, get_result()) end)
        end},

        {"Should return paginated results", fun() ->
          clear_tables(),
          TaskId = 1,
          Time1 = {{1985, 11, 03}, {5, 21, 00}},
          create_result(TaskId, Time1, [1]),
          Time2 = {{1985, 12, 12}, {5, 21, 00}},
          create_result(TaskId, Time2, [2]),
          ExpectedResponse1 = mochijson2:encode({struct, [
                {<<"success">>, true},
                {<<"page">>, 1},
                {<<"per_page">>, 1},
                {<<"count">>, 2},
                {<<"results">>, [
                  {struct, [
                    {<<"published_at">>, list_to_integer(date_time_to_epoch(Time2))}, % Results are sorted DESC
                    {<<"buckets">>, mochijson2:decode(cells_json([2]))},
                    {<<"exceptions">>, []}
                  ]}
                ]}
              ]}),
          ExpectedResponse2 = mochijson2:encode({struct, [
                {<<"success">>, true},
                {<<"page">>, 2},
                {<<"per_page">>, 1},
                {<<"count">>, 2},
                {<<"results">>, [
                  {struct, [
                    {<<"published_at">>, list_to_integer(date_time_to_epoch(Time1))}, % Results are sorted DESC
                    {<<"buckets">>, mochijson2:decode(cells_json([1]))},
                    {<<"exceptions">>, []}
                  ]}
                ]}
              ]}),
          Args1 = [TaskId, 1, 1, <<"1900/01/01 12:00:00">>, <<"2050/01/01 12:00:00">>],
          mecked_backend(Args1, fun() -> verifyHttp(ExpectedResponse1, get_result()) end),
          Args2 = [TaskId, 2, 1, <<"1900/01/01 12:00:00">>, <<"2050/01/01 12:00:00">>],
          mecked_backend(Args2, fun() -> verifyHttp(ExpectedResponse2, get_result()) end)
        end},

        {"Should filter by start and end-time", fun() ->
          clear_tables(),
          TaskId = 1,
          Time1 = {{1985, 03, 10}, {5, 21, 00}},
          create_result(TaskId, Time1, [1]),
          Time2 = {{1985, 03, 11}, {5, 21, 00}},
          create_result(TaskId, Time2, [2]),
          Time3 = {{1985, 03, 12}, {5, 21, 00}},
          create_result(TaskId, Time3, [3]),
          ExpectedResponse = mochijson2:encode({struct, [
                {<<"success">>, true},
                {<<"page">>, 1},
                {<<"per_page">>, 1},
                {<<"count">>, 1},
                {<<"results">>, [
                  {struct, [
                    {<<"published_at">>, list_to_integer(date_time_to_epoch(Time2))},
                    {<<"buckets">>, mochijson2:decode(cells_json([2]))},
                    {<<"exceptions">>, []}
                  ]}
                ]}
              ]}),
          Args = [TaskId, 1, 1, <<"1985/03/11 04:00:00">>, <<"1985/03/11 06:00:00">>],
          mecked_backend(Args, fun() -> verifyHttp(ExpectedResponse, get_result()) end)
        end}
      ]
    ).

mecked_backend(Args, Fun) ->
  meck:new(request_proxy),
  RPCPayload = {ok, {struct, [
    {<<"rpc">>, <<"task_results_json">>},
    {<<"arguments">>, Args}
  ]}},
  meck:expect(request_proxy, forward_request, fun(_) -> RPCPayload end),
  Fun(),
  meck:unload(request_proxy).

verifyHttp(ExpectedBody, HttpResponse) ->
  ?assertMatch({ok, {{_, 200, _}, _, _}}, HttpResponse),
  {ok, {_, _, Body}} = HttpResponse,
  ?assertEqual(trim(ExpectedBody), trim(Body)).

trim(Body) ->
  re:replace(Body, "( |\n)*", "", [global, {return, binary}]).

get_result() ->
  get_result("").

get_result(QueryString) ->
  httpc:request(get, {"http://127.0.0.1:11000/backend/api/tasks/token/results?" ++ QueryString, []}, [], []).

-endif.
