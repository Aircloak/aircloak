-module(task_coordinator_tests).

-ifdef(TEST).

-include("cloak.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eunit_helpers.hrl").

-define(runTaskAndVerifyResult(Query, Expected), (fun() ->
  task_coordinator:run_task(test_task(Query)),
  {reply, Result} = ?assertReceived({reply, _}, 1000),
  ?assertEqual(Expected, Result)
end)()).

test_task(Query) ->
  #task{
    task_id = "test_task",
    query = Query,
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
    ]
  }.

-endif.
