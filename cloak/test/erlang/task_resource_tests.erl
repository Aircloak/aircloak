-module(task_resource_tests).

%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").

run_task_test_() ->
  {setup,
    fun() ->
      meck:new(progress_handler, [passthrough]),
      meck:expect(progress_handler, register_task, 2, fun(Task, undefined) -> Task end),
      server_tester:start()
    end,
    fun(Pid) ->
      server_tester:stop(Pid),
      meck:unload()
    end,
    [
      ?_assertMatch(
            {_, {{_, 401, _}, _, <<"{\"success\":false,\"error\":\"Missing analyst HTTP-HEADER\"}">>}},
            server_tester:post("task/run")
          ),
      fun() ->
        meck:new(task_coordinator, []),
        meck:expect(task_coordinator, run_task, 1, fun(_) -> self() ! {reply, <<"task_result">>}, ok end),
        ?assertMatch(
              {_, {{_, 200, _}, _, <<"task_result">>}},
              server_tester:post("task/run", [{"analyst", "1"}], task_body())
            ),
        meck:unload(task_coordinator)
      end,
      fun() ->
        ?assertMatch(
              {_, {{_, 422, _}, _, <<"{\"success\":false,\"error\":\"missing required headers: task_id, auth_token\"}">>}},
              server_tester:post("task/run", [{"analyst", "1"}, {"async_query", "true"}], task_body())
            )
      end,
      fun() ->
        Me = self(),
        meck:new(task_coordinator, []),
        meck:expect(task_coordinator, run_task, 1, fun(_) -> Me ! {reply, <<"task_result">>}, ok end),
        ?assertMatch(
              {_, {{_, 200, _}, _, <<"{\"success\":true}">>}},
              server_tester:post("task/run",
                    [{"analyst", "1"}, {"async_query", "true"}, {"task_id", "foo"}, {"auth_token", "bar"}],
                    task_body()
                  )
            ),
        ?assertReceived({reply, <<"task_result">>}, 500),
        meck:unload(task_coordinator)
      end
    ]
  }.

task_body() ->
  "
    {
      \"prefetch\": [
        {
          \"table\": \"test1\",
          \"columns\": [\"name\", \"priority\"],
          \"where\": {\"$$priority\": {\"$lt\": 3}}
        }
      ],
      \"post_processing\": {
        \"code\":\"report_property(tables.test1[0].name, tables.test1[0].priority)\"
      }
    }
  ".

-endif.
