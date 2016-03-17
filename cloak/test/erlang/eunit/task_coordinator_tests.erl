-module(task_coordinator_tests).

-ifdef(TEST).

-include("src/cloak.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").

-define(verifyAsync(Code, Expected), (fun() ->
      task_coordinator:run_task(test_task(Code)),
      {reply, Json} = ?assertReceived({reply, _}, 1000),
      {struct, Result} = mochijson2:decode(Json),
      ?assertEqual(Expected, lists:sort(Result))
    end)()).

-define(verifySync(Code, Expected), (fun() ->
      task_coordinator:block_run_task(test_task(Code)),
      {reply, Json} = ?assertReceived({reply, _}, 1000),
      {struct, Result} = mochijson2:decode(Json),
      ?assertEqual(Expected, lists:sort(Result))
    end)()).

test_task(Code) ->
  #task{
    task_id = "test_task",
    analyst_id = 1,
    prefetch = [[{table,<<"heights">>}]],
    code = Code,
    libraries = [],
    return_token = {json, {process, self()}}
  }.

task_test_() ->
  {setup,
    fun() ->
      db_test:setup(),
      db_test:create_analyst_schema(1),
      db_test:create_analyst_table(1, "user_heights",
          "height integer, ac_user_id varchar(40), ac_created_at timestamp"),
      meck:new(cloak_distributions),
      meck:expect(cloak_distributions, gauss, fun(_Sigma, N) -> round(N) end),
      meck:expect(cloak_distributions, gauss_s, fun(_Sigma, N, _Seed) -> round(N) end),
      ok
    end,
    fun(_) -> meck:unload() end,
    [
      {"no rows", fun() ->
        ?verifyAsync(
              <<"for row in user_table(\"heights\") do report_property(\"height\", row.height) end">>,
              [
                {<<"analyst_id">>, 1},
                {<<"buckets">>, []},
                {<<"exceptions">>, []},
                {<<"task_id">>, "test_task"}
              ]
            )
      end},
      {"async task", fun() ->
        Data = [{
            iolist_to_binary(io_lib:format("user-~p", [Index])),
            [{<<"user_heights">>, [{columns, [<<"height">>]}, {data, [[180]]}]}]
          } || Index <- lists:seq(1, 100)],
        ok = db_test:add_users_data(1, Data, timer:seconds(5)),
        ?verifyAsync(
              <<"for row in user_table(\"heights\") do report_property(\"height\", row.height) end">>,
              [
                {<<"analyst_id">>,1},
                {<<"buckets">>, [{struct, [
                  {<<"label">>, <<"height">>},
                  {<<"value">>, <<"180">>},
                  {<<"count">>, 100}
                ]}]},
                {<<"exceptions">>, []},
                {<<"task_id">>, "test_task"}
              ]
            )
      end},
      {"sync task", fun() ->
        Data = [{
            iolist_to_binary(io_lib:format("user-~p", [Index])),
            [{<<"user_heights">>, [{columns, [<<"height">>]}, {data, [[180]]}]}]
          } || Index <- lists:seq(1, 100)],
        ok = db_test:add_users_data(1, Data, timer:seconds(5)),
        ?verifySync(
              <<"for row in user_table(\"heights\") do report_property(\"height\", row.height) end">>,
              [
                {<<"analyst_id">>,1},
                {<<"buckets">>, [{struct, [
                  {<<"label">>, <<"height">>},
                  {<<"value">>, <<"180">>},
                  {<<"count">>, 100}
                ]}]},
                {<<"exceptions">>, []},
                {<<"task_id">>, "test_task"}
              ]
            )
      end},
      {"task with errors", fun() ->
        Data = [{
            iolist_to_binary(io_lib:format("user-~p", [Index])),
            [{<<"user_heights">>, [{columns, [<<"height">>]}, {data, [[180]]}]}]
          } || Index <- lists:seq(1, 100)],
        ok = db_test:add_users_data(1, Data, timer:seconds(5)),
        ?verifyAsync(
              <<"error('some_error')">>,
              [
                {<<"analyst_id">>,1},
                {<<"buckets">>,[]},
                {<<"exceptions">>, [{struct, [
                  {<<"error">>, <<"{sandbox_error,\"[string \\\"task_code\\\"]:1: some_error\"}">>},
                  {<<"count">>,100}
                ]}]},
                {<<"task_id">>,"test_task"}
              ]
            )
      end},
      {"timeout", fun() ->
        gen_server:start_link(task_coordinator, {test_task(<<"">>), undefined, undefined, fun timeout_runner/4}, []),
        {reply, Json} = ?assertReceived({reply, _}, 1000),
        {struct, Result} = mochijson2:decode(Json),
        ?assertEqual(
              [
                {<<"analyst_id">>, 1},
                {<<"buckets">>, []},
                {<<"exceptions">>, [{struct,[
                  {<<"error">>, <<"task execution timed out before processing all users">>},
                  {<<"count">>, 1}
                ]}]},
                {<<"task_id">>,"test_task"}
              ],
              lists:sort(Result)
            )
      end}
    ]
  }.

  timeout_runner(_, _, _, _) ->
    Me = self(),
    spawn(fun() -> Me ! timeout end),
    ok.

-endif.
