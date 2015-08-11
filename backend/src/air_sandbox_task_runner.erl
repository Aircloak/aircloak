%% @doc Runs the task according to the given specification.
-module(air_sandbox_task_runner).

%% API
-export([
  run/2
]).

-include("air.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Runs the specified task in the sandbox and returns job responses.
-spec run([#job_response{}], [{binary(), term()}]) -> [{status, ok | timeout} | {responses, [#job_response{}]}].
run(PreviousJobResponses, TaskSpec) ->
  do_run(PreviousJobResponses, TaskSpec, air_conf:get_val(air_sandbox, task_timeout), fun handle_response/3).


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

do_run(PreviousJobResponses, TaskSpec, Timeout, ResponseHandler) ->
  ReqId = make_ref(),
  Jobs = jobs(PreviousJobResponses, TaskSpec),
  [run_job(ReqId, Job, ResponseHandler) || Job <- Jobs],
  TimerRef = erlang:send_after(Timeout, self(), {ReqId, timeout}),
  Responses = collect_responses(ReqId, length(Jobs)),
  InsertActions = collect_insert_actions(
        proplists:get_value(analyst, TaskSpec),
        proplists:get_value(responses, Responses)
      ),
  erlang:cancel_timer(TimerRef),
  Responses ++ [{insert_actions, InsertActions}].

collect_responses(ReqId, N) ->
  collect_responses(ReqId, N, []).

collect_responses(_, 0, ResponsesAcc) -> [{status, ok}, {responses, ResponsesAcc}];
collect_responses(ReqId, N, ResponsesAcc) ->
  receive
    {ReqId, timeout} -> [{status, timeout}, {responses, ResponsesAcc}];
    {ReqId, job_response, Response} -> collect_responses(ReqId, N-1, [Response | ResponsesAcc])
  after 5000 ->
    [{status, timeout}, {responses, ResponsesAcc}]
  end.

collect_insert_actions(Analyst, JobResponses) ->
  collect_insert_actions(Analyst, users_with_success_responses(JobResponses), []).

collect_insert_actions(_, [], Acc) -> Acc;
collect_insert_actions(Analyst, [UserId | RestUsers], Acc) ->
  receive
    {insert, Analyst, UserId, []} ->
      collect_insert_actions(Analyst, RestUsers, Acc);
    {insert, Analyst, UserId, UserData} ->
      collect_insert_actions(Analyst, RestUsers, [{UserId, UserData} | Acc])
  after 10 ->
    % Since job responses arrived, at this point insert messages should already be
    % in the queue for all successful responses, so we don't wait for the response
    % for too long.
    collect_insert_actions(Analyst, RestUsers, Acc)
  end.

users_with_success_responses(JobResponses) ->
  [UserId ||
    #job_response{user_id=UserId, properties=Properties} <- JobResponses,
    not lists:any(
          fun
            (#property{label= <<"__AC_INTERNAL_ERROR__">>}) -> true;
            (_) -> false
          end,
          Properties
        )
  ].

run_job(ReqId, {UserId, Accumulator, Task, JobInput}, ResponseHandler) ->
  Caller = self(),
  Timestamp = cloak_util:timestamp_to_epoch(os:timestamp()),
  job_runner:execute(1, Task#task{timestamp=Timestamp}, UserId, JobInput, Accumulator, ReqId,
      fun(JobReqId, Response) -> ResponseHandler(Caller, JobReqId, Response) end).

handle_response(Caller, ReqId, {_, JobResponse}) ->
  Caller ! {ReqId, job_response, JobResponse}.

jobs(PreviousJobResponses, TaskSpec) ->
  Code = proplists:get_value(<<"code">>, TaskSpec),
  Libraries = [
    {
      proplists:get_value(<<"name">>, LibDef),
      proplists:get_value(<<"code">>, LibDef)
    } || LibDef <- proplists:get_value(<<"libraries">>, TaskSpec, [])
  ],
  Analyst = proplists:get_value(analyst, TaskSpec),
  [{UserId, accumulator(UserId, PreviousJobResponses), make_task(Analyst, Libraries, Code), JobInput}
    || {UserId, JobInput} <- tables_data(TaskSpec)].

accumulator(UserId, PreviousJobResponses) ->
  case [
    Accumulator ||
      #job_response{user_id=UId, accumulator=Accumulator} <- PreviousJobResponses,
      UId =:= UserId
  ] of
    [] -> undefined;
    [Accumulator] -> Accumulator
  end.

make_task(Analyst, Libraries, Code) ->
  #task{
    task_id = <<"dummy_task">>,
    analyst_id = Analyst,
    libraries = Libraries,
    code = Code
  }.

tables_data(TaskSpec) ->
  AllData = [transform_table_data(TableName, TableData) ||
    {TableName, TableData} <- proplists:get_value(<<"users_data">>, TaskSpec)],
  dict:to_list(lists:foldl(
        fun({TableName, Columns, Rows}, Acc) ->
          lists:foldl(
                fun({UserId, JobInput}, InnerAcc) ->
                  dict:update(
                        UserId,
                        fun(ExistingInputs) -> JobInput ++ ExistingInputs end,
                        JobInput,
                        InnerAcc
                      )
                end,
                Acc,
                job_data_streamer:map_table_data_to_job_inputs(TableName, Columns, Rows)
              )
        end,
        dict:new(),
        AllData
      )).

transform_table_data(TableName, TableData) ->
  {
    TableName,
    proplists:get_value(<<"columns">>, TableData),
    [[UserId, undefined | Row] ||
      {UserId, Rows} <- proplists:get_value(<<"data">>, TableData),
      Row <- Rows]
  }.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

-define(PROP(L, V), #property{label = L, value = V}).

run_task(TaskSpec) -> run_task([], TaskSpec).

run_task(PreviousJobResponses, TaskSpec) ->
  Analyst = air_sandbox_state:new_analyst_id(),
  gproc:reg({n, l, {task_executor, Analyst}}),
  Result = run(PreviousJobResponses, [{analyst, Analyst} | TaskSpec]),
  R1 = lists:keyreplace(responses, 1, Result, {responses, lists:sort(proplists:get_value(responses, Result))}),
  lists:keyreplace(insert_actions, 1, R1, {insert_actions, lists:sort(proplists:get_value(insert_actions, R1))}).

?test_suite(standard_test_,
      setup,
      [
        ?load_conf,
        ?with_applications([gproc, webmachine]),
        ?with_processes([cloak_services_sup, air_api_sup])
      ],
      [
        {"Single user", ?_assertMatch(
              [{status, ok}, {responses, [#job_response{properties=[?PROP(<<"p1">>, <<"42">>)]}]}, {insert_actions, []}],
              run_task([
                    {<<"users_data">>,
                      [{<<"t1">>,
                        [{<<"columns">>, [<<"c1">>]},
                         {<<"data">>, [{<<"u1">>, [[42]]}]}]
                      }]
                    },
                    {<<"code">>, <<
                      "report_property('p1', load_user_table('t1')[1].c1)"
                    >>}
                  ])
            )},
        {"Library", ?_assertMatch(
              [{status, ok}, {responses, [#job_response{properties=[?PROP(<<"p1">>, <<"bar">>)]}]}, {insert_actions, []}],
              run_task([
                    {<<"users_data">>,
                      [{<<"t1">>,
                        [{<<"columns">>, [<<"c1">>]},
                         {<<"data">>, [{<<"u1">>, [[42]]}]}]
                      }]
                    },
                    {<<"code">>, <<"report_property('p1', foo())">>},
                    {<<"libraries">>, [
                      [{<<"name">>, <<"lib1">>}, {<<"code">>, <<"function foo() return 'bar' end">>}]
                    ]}
                  ])
            )},
        {"Two rows", ?_assertMatch(
              [{status, ok}, {responses, [#job_response{properties=[?PROP(<<"p1">>, <<"45.14">>)]}]}, {insert_actions, []}],
              run_task([
                    {<<"users_data">>,
                      [{<<"t1">>,
                        [{<<"columns">>, [<<"c1">>]},
                         {<<"data">>, [{<<"u1">>, [[42], [3.14]]}]}]
                      }]
                    },
                    {<<"code">>, <<"report_property('p1', load_user_table('t1')[1].c1 + load_user_table('t1')[2].c1)">>}
                  ])
            )},
        {"Two tables", ?_assertMatch(
              [{status, ok}, {responses, [#job_response{properties=[?PROP(<<"p1">>, <<"45.14">>)]}]}, {insert_actions, []}],
              run_task([
                    {<<"users_data">>,
                      [
                        {<<"t1">>, [{<<"columns">>, [<<"c1">>]}, {<<"data">>, [{<<"u1">>, [[42]]}]}]},
                        {<<"t2">>, [{<<"columns">>, [<<"c2">>]}, {<<"data">>, [{<<"u1">>, [[3.14]]}]}]}
                      ]
                    },
                    {<<"code">>, <<"report_property('p1', load_user_table('t1')[1].c1 + load_user_table('t2')[1].c2)">>}
                  ])
            )},
        {"Multiple users", ?_assertMatch(
              [{status, ok}, {responses, [
                #job_response{properties=[?PROP(<<"p1">>, <<"42">>)]},
                #job_response{properties=[?PROP(<<"p1">>, <<"3.14">>)]}
              ]}, {insert_actions, []}],
              run_task([
                    {<<"users_data">>,
                      [{<<"t1">>,
                        [{<<"columns">>, [<<"c1">>]},
                         {<<"data">>, [{<<"u1">>, [[42]]}, {<<"u2">>, [[3.14]]}]}]
                      }]
                    },
                    {<<"code">>, <<"report_property('p1', load_user_table('t1')[1].c1)">>}
                  ])
            )},
        {"Accumulator", fun() ->
          JobResponses = proplists:get_value(responses,
                run_task([
                    {<<"users_data">>,
                      [{<<"t1">>,
                        [{<<"columns">>, [<<"c1">>]},
                         {<<"data">>, [{<<"u1">>, [[42]]}, {<<"u2">>, [[3.14]]}]}]
                      }]
                    },
                    {<<"code">>, <<"if accumulator == nil then accumulator = load_user_table('t1')[1].c1 end">>}
                  ])
              ),
          ?assertMatch(
              [{status, ok}, {responses, [
                #job_response{user_id= <<"u1">>, properties=[?PROP(<<"acc">>, <<"42">>)]},
                #job_response{user_id= <<"u2">>, properties=[?PROP(<<"acc">>, <<"3.14">>)]}
              ]}, {insert_actions, []}],
              run_task(JobResponses, [
                    {<<"users_data">>,
                      [{<<"t1">>,
                        [{<<"columns">>, [<<"c1">>]},
                         {<<"data">>, [{<<"u1">>, [[42]]}, {<<"u2">>, [[3.14]]}]}]
                      }]
                    },
                    {<<"code">>, <<"report_property('acc', accumulator)">>}
                  ])
            )
        end},
        {"Inserting data", ?_assertMatch(
              [{status, ok}, {responses, _},
                {insert_actions,
                  [
                    {<<"u1">>,
                      [{<<"foo">>, [
                        {columns, [<<"a">>, <<"b">>, <<"c">>]},
                        {data, [[1, "2", null], [3, null, false]]}]}]
                    },
                    {<<"u2">>,
                      [{<<"foo">>, [
                        {columns, [<<"a">>, <<"b">>, <<"c">>]},
                        {data, [[1, "2", null], [3, null, false]]}]}]
                    }
                  ]
                }
              ],
              run_task([
                    {<<"users_data">>,
                      [{<<"t1">>,
                        [{<<"columns">>, [<<"c1">>]},
                         {<<"data">>, [{<<"u1">>, [[42]]}, {<<"u2">>, [[3.14]]}]}]
                      }]
                    },
                    {<<"code">>, <<"
                      insert_row('foo', {a=1, b='2'})
                      insert_row('foo', {a=3, c=false})
                    ">>}
                  ])
            )},
        {"Full timeout", ?_assertMatch(
              [{status,timeout}, {responses,[]}, {insert_actions, []}],
              do_run(
                    [],
                    [
                      {<<"users_data">>,
                        [{<<"t1">>,
                          [{<<"columns">>, [<<"c1">>]},
                           {<<"data">>, [{<<"u1">>, [[42]]}, {<<"u2">>, [[3.14]]}]}]
                        }]
                      },
                      {<<"code">>, <<"report_property('p1', load_user_table('t1')[1].c1)">>}
                    ],
                    1,
                    fun(_, _, _) -> ok end
                  )
            )},
        {"Partial timeout", ?_assertMatch(
              [{status, timeout}, {responses, [
                #job_response{properties=[?PROP(<<"p1">>, <<"42">>)]}
              ]}, {insert_actions, []}],
              do_run(
                    [],
                    [
                      {<<"users_data">>,
                        [{<<"t1">>,
                          [{<<"columns">>, [<<"c1">>]},
                           {<<"data">>, [{<<"u1">>, [[42]]}, {<<"u2">>, [[3.14]]}]}]
                        }]
                      },
                      {<<"code">>, <<"report_property('p1', load_user_table('t1')[1].c1)">>}
                    ],
                    100,
                    fun
                      (Caller, ReqId, {_, #job_response{properties=[?PROP(<<"p1">>, <<"42">>)]}} = Response) ->
                        handle_response(Caller, ReqId, Response);
                      (_, _, _) -> ok
                    end
                  )
            )}
      ]
    ).

-endif.
