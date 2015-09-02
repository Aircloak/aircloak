%% @doc Handles /task requests. See README.md for request examples.
-module(air_sandbox_task_resource).

%% webmachine callbacks
-export([
  init/1,
  allowed_methods/2,
  post_is_create/2,
  process_post/2
]).

-include("air.hrl").
-include_lib("webmachine/include/webmachine.hrl").


%% -------------------------------------------------------------------
%% webmachine callbacks
%% -------------------------------------------------------------------

%% @hidden
init([]) -> {ok, nostate}.

%% @hidden
allowed_methods(Req, State) -> {['POST'], Req, State}.

%% @hidden
post_is_create(Req, State) -> {false, Req, State}.

%% @hidden
process_post(Req, State) ->
  handle_action(post, wrq:path_info(action, Req), Req, State).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

handle_action(post, "run", Req, State) ->
  {{halt, 200}, resource_common:respond_json(execute_task_from_body(wrq:req_body(Req)), Req), State};
% This is a test hook used by the monitoring system to ensure that the air is operational.
% It will execute a dummy task in a sendbox, and report OK if all works as expected.
handle_action(post, "test", Req, State) ->
  case is_task_execution_ok() of
    true -> {{halt, 200}, resource_common:respond_json([{status, <<"ok">>}], Req), State};
    false ->
      Response = resource_common:respond_json([{status, failed}, {description, <<"Unexpected or no response">>}], Req),
      {{halt, 500}, Response, State}
  end.

run_task(TaskSpec) ->
  Analyst = air_sandbox_state:new_analyst_id(),
  gproc:reg({n, l, {task_executor, Analyst}}),
  FullTaskSpec = [{analyst, Analyst} | TaskSpec],
  {Responses, _} = lists:foldl(
        fun(UsersData, Acc) -> run_task(FullTaskSpec, UsersData, Acc) end,
        {[], []},
        proplists:get_value(<<"users_data">>, FullTaskSpec, [])
      ),
  lists:reverse(Responses).

execute_task_from_body(Body) ->
  TaskSpec = cloak_util:destructure_parsed_json(mochijson2:decode(Body)),
  run_task(TaskSpec).

run_task(TaskSpec, UsersData, {Responses, PreviousJobResponses}) ->
  ThisRunSpec = lists:keystore(<<"users_data">>, 1, TaskSpec, {<<"users_data">>, UsersData}),
  Response = air_sandbox_task_runner:run(PreviousJobResponses, ThisRunSpec),
  {[map_task_response(Response) | Responses], proplists:get_value(responses, Response)}.

map_task_response(Response) ->
  [
    {insert_actions, map_insert_actions(lists:sort(proplists:get_value(insert_actions, Response)))} |
    map_responses(proplists:get_value(responses, Response))
  ].

map_insert_actions([]) -> [];
map_insert_actions([{UserId, InsertActions} | Rest]) ->
  map_user_insert_actions(UserId, InsertActions) ++ map_insert_actions(Rest).

map_user_insert_actions(_, []) -> [];
map_user_insert_actions(UserId, InsertActions) ->
  [{
    UserId,
    [{Table, insert_data_to_json(Data)} || {Table, Data} <- InsertActions]
  }].

insert_data_to_json(Data) ->
  Columns = proplists:get_value(columns, Data),
  [lists:zip(Columns, map_insert_row(Row)) || Row <- proplists:get_value(data, Data)].

map_insert_row([]) -> [];
map_insert_row([CharList | Rest]) when is_list(CharList) -> [list_to_binary(CharList) | map_insert_row(Rest)];
map_insert_row([Other | Rest]) -> [Other | map_insert_row(Rest)].

map_responses(JobResponses) ->
  {Successes, Errors} = lists:foldl(fun map_response/2, {[], dict:new()}, JobResponses),
  FinalErrors = [[{message, Error}, {count, Count}] || {Error, Count} <- dict:to_list(Errors)],
  [{reported_properties, lists:sort(Successes)}, {errors, FinalErrors}].

map_response(#job_response{user_id=UserId, properties=Properties}, {Successes, Errors}) ->
  {OkValues, Errors1} = lists:foldl(fun map_property/2, {[], Errors}, Properties),
  {[{UserId, cloak_util:dedupe(OkValues)} | Successes], Errors1}.

map_property(#property{label= <<"__AC_INTERNAL_ERROR__">>, value=Error}, {Successes, Errors}) ->
  {Successes, dict:update_counter(Error, 1, Errors)};
map_property(#property{label=Label, value=Value}, {Successes, Errors}) ->
  {[[{label, Label}, {value, Value}] | Successes], Errors}.


%% -------------------------------------------------------------------
%% Monitoring hook
%% -------------------------------------------------------------------

% Used to validate that we can execute a task in the sandbox and get the expected result
% from our monitoring tools
-spec is_task_execution_ok() -> boolean().
is_task_execution_ok() ->
  TaskSpec =
      "{"
        "\"users_data\": [{"
          "\"t1\": {"
            "\"columns\": [\"c1\"],"
            "\"data\": {\"u1\": [[105]]}"
          "}"
        "}],"
        "\"code\": \"report_property('p1', load_user_table('t1')[1].c1);\""
      "}",
  ExpectedResult = [
    [
      {insert_actions, []},
      {reported_properties, [
        {<<"u1">>, [[{label,<<"p1">>},{value,<<"105">>}]]}
      ]},
      {errors, []}
    ]
  ],
  case execute_task_from_body(TaskSpec) of
    ExpectedResult -> true;
    _ -> false
  end.


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
        ?with_applications([gproc, webmachine, etcd]),
        ?with_processes([cloak_services_sup, air_api_sup]),
        ?api_web_server
      ],
      [
        {"Validate that monitoring check works", fun() ->
          ?assertEqual(true, is_task_execution_ok())
        end},
        {"Reporting a single property", fun() ->
          verifyHttp200(
                "[{\"insert_actions\":[],\"reported_properties\":{\"u1\":[{\"label\":\"p1\",\"value\":\"105\"}]},\"errors\":[]}]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[105]]}"
                          "}"
                        "}],"
                        "\"code\": \"report_property('p1', load_user_table('t1')[1].c1);\""
                      "}"
                    )
              )
        end},
        {"Reporting multiple values for the same property", fun() ->
          verifyHttp200(
                "[{\"insert_actions\":[],\"reported_properties\":"
                  "{\"u1\":[{\"label\":\"p1\",\"value\":\"2\"},{\"label\":\"p1\",\"value\":\"1\"}]},"
                  "\"errors\":[]"
                "}]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[105]]}"
                          "}"
                        "}],"
                        "\"code\": \"report_property('p1', 1); report_property('p1', 2);\""
                      "}"
                    )
              )
        end},
        {"Libraries", fun() ->
          verifyHttp200(
                "[{\"insert_actions\":[],\"reported_properties\":{\"u1\":[{\"label\":\"p1\",\"value\":\"10\"}]},\"errors\":[]}]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[100]]}"
                          "}"
                        "}],"
                        "\"code\": \"report_property('p1', tenth(load_user_table('t1')[1].c1));\","
                        "\"libraries\": ["
                          "{\"name\": \"foo\", \"code\": \"function tenth(x) return x/10 end\"}"
                        "]"
                      "}"
                    )
              )
        end},
        {"Accumulator", fun() ->
          verifyHttp200(
                "["
                  "{\"insert_actions\":[],\"reported_properties\":{"
                    "\"u1\":[{\"label\":\"acc\",\"value\":\"1\"}],"
                    "\"u2\":[{\"label\":\"acc\",\"value\":\"2\"}]},"
                    "\"errors\":[]"
                  "},"
                  "{\"insert_actions\":[],\"reported_properties\":{"
                    "\"u1\":[{\"label\":\"acc\",\"value\":\"4\"}],"
                    "\"u2\":[{\"label\":\"acc\",\"value\":\"6\"}]},"
                    "\"errors\":[]"
                  "}"
                "]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[1]], \"u2\": [[2]]}"
                          "}"
                        "},"
                        "{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[3]], \"u2\": [[4]]}"
                          "}"
                        "}"
                        "],"
                        "\"code\": \"
                          if accumulator == nil then accumulator = 0 end
                          report_property('acc', accumulator + load_user_table('t1')[1].c1)
                          accumulator = accumulator + load_user_table('t1')[1].c1
                        \""
                      "}"
                    )
              )
        end},
        {"Insert users", fun() ->
          verifyHttp200(
                "[{\"insert_actions\":{"
                    "\"u1\":{\"foo\":[{\"a\":1}],\"bar\":[{\"b\":\"2\"}]},"
                    "\"u2\":{\"foo\":[{\"a\":1}],\"bar\":[{\"b\":\"2\"}]},"
                    "\"u3\":{\"foo\":[{\"a\":1}],\"bar\":[{\"b\":\"2\"}]}"
                  "},"
                  "\"reported_properties\":{\"u1\":[],\"u2\":[],\"u3\":[]},"
                  "\"errors\":[]"
                "}]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[105]], \"u2\": [[110]], \"u3\":[[115], [120]]}"
                          "}"
                        "}],"
                        "\"code\": \""
                          "insert_row('foo', {a=1})\n"
                          "insert_row('bar', {b='2'})\n"
                        "\""
                      "}"
                    )
              )
        end},
        {"Reporting errors", fun() ->
          verifyHttp200(
                "[{\"insert_actions\":[],\"reported_properties\":{\"u1\":[]},"
                  "\"errors\":[{"
                    "\"message\":\"{sandbox_error,\\\"[string \\\\\\\"task_code\\\\\\\"]:1: attempt to index field '?' (a nil value)\\\"}\","
                    "\"count\":1"
                  "}]"
                "}]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[105]]}"
                          "}"
                        "}],"
                        "\"code\": \"report_property('p1', load_user_table('t1')[2].c1);\""
                      "}"
                    )
              )
        end},
        {"Reporting properties and errors", fun() ->
          verifyHttp200(
                "[{\"insert_actions\":[],\"reported_properties\":{\"u1\":[],\"u2\":[],\"u3\":[{\"label\":\"p1\",\"value\":\"120\"}]},"
                  "\"errors\":[{"
                    "\"message\":\"{sandbox_error,\\\"[string \\\\\\\"task_code\\\\\\\"]:1: attempt to index field '?' (a nil value)\\\"}\","
                    "\"count\":2"
                  "}]"
                "}]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[105]], \"u2\": [[110]], \"u3\":[[115], [120]]}"
                          "}"
                        "}],"
                        "\"code\": \"report_property('p1', load_user_table('t1')[2].c1);\""
                      "}"
                    )
              )
        end},
        {"Inserting users and errors", fun() ->
          verifyHttp200(
                "[{\"insert_actions\":{"
                    "\"u2\":{\"foo\":[{\"a\":1}]},"
                    "\"u3\":{\"foo\":[{\"a\":1}]}"
                  "},"
                  "\"reported_properties\":{\"u1\":[],\"u2\":[],\"u3\":[]},"
                  "\"errors\":[{\"message\":\"{sandbox_error,\\\"[string \\\\\\\"task_code\\\\\\\"]:2: foo\\\"}\",\"count\":1}]"
                "}]",
                request_task_run(
                      "{"
                        "\"users_data\": [{"
                          "\"t1\": {"
                            "\"columns\": [\"c1\"],"
                            "\"data\": {\"u1\": [[105]], \"u2\": [[110]], \"u3\":[[115], [120]]}"
                          "}"
                        "}],"
                        "\"code\": \""
                          "insert_row('foo', {a=1})\n"
                          "if load_user_table('t1')[1].c1 == 105 then error('foo') end"
                        "\""
                      "}"
                    )
              )
        end}
      ]
    ).

verifyHttp200(ExpectedBody, HttpResponse) ->
  ?assertMatch({ok, {{_, 200, "OK"}, _, _}}, HttpResponse),
  {ok, {{_, 200, "OK"}, _, Body}} = HttpResponse,
  ?assertEqual(ExpectedBody, Body).

request_task_run(TaskSpec) ->
  httpc:request(post,
        {db_test_helpers:http_url("/task/run"), [], "text/html", TaskSpec},
        [],
        []
      ).

-endif.
