-module(task_coordinator_tests).

-ifdef(TEST).

-include("cloak.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").

-define(verifyAsync(Code, Expected), (fun() ->
      task_coordinator:run_task(test_task(Code)),
      {reply, Result} = ?assertReceived({reply, _}, 1000),
      ?assertEqual(Expected, Result)
    end)()).

-define(verifySync(Code, Expected), (fun() ->
      task_coordinator:block_run_task(test_task(Code)),
      {reply, Result} = ?assertReceived({reply, _}, 1000),
      ?assertEqual(Expected, Result)
    end)()).

test_task(Code) ->
  #task{
    task_id = "test_task",
    prefetch = [[{table, db_test:table_path(<<"heights">>)}]],
    code = Code,
    libraries = [],
    result_destination = {process, self()}
  }.

task_test_() ->
  {setup,
    fun() ->
      db_test:setup(),
      db_test:create_test_schema(),
      db_test:create_table(<<"heights">>, <<"height INTEGER">>),
      meck:new(cloak_distributions),
      meck:expect(cloak_distributions, gauss, fun(_Sigma, N) -> round(N) end),
      meck:expect(cloak_distributions, gauss_s, fun(_Sigma, N, _Seed) -> round(N) end),
      ok
    end,
    fun(_) -> meck:unload() end,
    [
      {"no rows", fun() ->
        ?verifyAsync(
          <<"for row in user_table(\"", (db_test:table_path(<<"heights">>))/binary,
              "\") do report_property(\"height\", row.height) end">>,
          #{buckets => [], exceptions => [], task_id => "test_task"}
        )
      end},
      {"async task", fun() ->
        Data = [{
            iolist_to_binary(io_lib:format("user-~p", [Index])),
            [{<<"heights">>, [{columns, [<<"height">>]}, {data, [[180]]}]}]
          } || Index <- lists:seq(1, 100)
				],
        ok = db_test:add_users_data(Data),
        ?verifyAsync(
          <<"for row in user_table(\"", (db_test:table_path(<<"heights">>))/binary,
              "\") do report_property(\"height\", row.height) end">>,
          #{
            buckets => [#{
              label => <<"height">>,
              value => <<"180">>,
              count => 100
            }],
            exceptions => [],
            task_id => "test_task"
          }
        )
      end},
      {"sync task", fun() ->
        Data = [{
            iolist_to_binary(io_lib:format("user-~p", [Index])),
            [{<<"heights">>, [{columns, [<<"height">>]}, {data, [[180]]}]}]
          } || Index <- lists:seq(1, 100)
				],
        ok = db_test:add_users_data(Data),
        ?verifySync(
          <<"for row in user_table(\"", (db_test:table_path(<<"heights">>))/binary,
              "\") do report_property(\"height\", row.height) end">>,
          #{
            buckets => [#{
              label => <<"height">>,
              value => <<"180">>,
              count => 100
            }],
            exceptions => [],
            task_id => "test_task"
          }
        )
      end},
      {"task with errors", fun() ->
        Data = [{
            iolist_to_binary(io_lib:format("user-~p", [Index])),
            [{<<"heights">>, [{columns, [<<"height">>]}, {data, [[180]]}]}]
          } || Index <- lists:seq(1, 100)
				],
        ok = db_test:add_users_data(Data),
        ?verifyAsync(
          <<"error('some_error')">>,
          #{
            buckets => [],
            exceptions => [#{
              error => <<"{sandbox_error,\"[string \\\"task_code\\\"]:1: some_error\"}">>,
              count => 100
            }],
            task_id => "test_task"
          }
        )
      end},
      {"timeout", fun() ->
        gen_server:start_link(task_coordinator, {test_task(<<"">>), undefined, undefined, fun timeout_runner/4}, []),
        {reply, Result} = ?assertReceived({reply, _}, 1000),
        ?assertEqual(
          #{
            buckets => [],
            exceptions => [#{
              error => <<"task execution timed out before processing all users">>,
              count => 1
            }],
            task_id => "test_task"
          },
          Result
        )
      end}
    ]
  }.

  timeout_runner(_, _, _, _) ->
    Me = self(),
    spawn(fun() -> Me ! timeout end),
    ok.

-endif.
