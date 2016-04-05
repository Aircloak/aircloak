%% @doc This is a test module to test the db operations (creating a table, removing a table, adding data).
%%      We check correctness by manually inspecting all the tables in the database.
%% @end
-module(full_batch_test).
-proper(extended).

-include_lib("proper/include/proper.hrl").
-include("cloak.hrl").
-include("full_test.hrl").
-include("prop_tracer.hrl").

%% API
-export([
  prop_batch/0
]).

%% Property state machine callbacks
-export([
  command/1,
  initial_state/0,
  next_state/3,
  precondition/2,
  postcondition/3
]).

%% Callback to execute a batch task.
-export([
  callback_run_batch_task/1
]).

%% Internal types.
-type batch_task_result() :: [{binary(), binary(), integer()}].


%% -------------------------------------------------------------------
%% Internal state representation and initial state
%% -------------------------------------------------------------------

-record(state, {
  db_state :: db_model:db_state(),
  buckets_not_ok = 0 :: non_neg_integer(),
  absolute_bucket_stats = [] :: [non_neg_integer()],
  relative_bucket_stats = [] :: [float()],
  bucket_stats = [] :: [{non_neg_integer(), non_neg_integer()}],
  bucket_list_stats = [] :: [{non_neg_integer(), non_neg_integer()}]
}).

initial_state() ->
  #state{
    db_state = db_model:create(?USERS, ?VALUES, ?MAX_TABLES, ?MAX_INITIAL_COLUMNS)
  }.


%% -------------------------------------------------------------------
%% Starting PropEr
%% -------------------------------------------------------------------

prop_batch() ->
  Users = 100,
  Values = 4,
  MaxTables = 3,
  MaxInitialColumns = 3,
  CommandsFactor = 10,
  InitialState = #state{
    db_state = db_model:create(Users, Values, MaxTables, MaxInitialColumns)
  },
  Result =
    numtests(3,
        ?FORALL(Commands, more_commands(CommandsFactor, proper_statem:commands(?MODULE, InitialState)),
            ?IMPLIES(
                  command_list_has_at_least_one_run_batch_task(Commands) andalso
                      db_model:command_list_has_at_least_one_add_data(Commands),
                  ?TRACEFAIL(State, Commands,
                        begin
                          db_model:prepare_database(),
                          proper_statem:run_commands(?MODULE, Commands)
                        end,
                        measure("Command sequence length", length(Commands),
                            measure("Outlier buckets", State#state.buckets_not_ok,
                                use_aggregators(
                                      [
                                        {fun my_command_names/1, [Commands]},
                                        {fun absolute_bucket_stats/1, [State]},
                                        {fun relative_bucket_stats/1, [State]},
                                        {fun bucket_sizes/1, [State]},
                                        {fun bucket_list_sizes/1, [State]}
                                      ],
                                      Result =:= ok andalso verify_buckets(State)
                                    )))
                      )
                ))),
  Result.

-spec command_list_has_at_least_one_run_batch_task([any()]) -> boolean().
command_list_has_at_least_one_run_batch_task(Commands) ->
  lists:any(
        fun
          ({set, {var, _}, {call, ?MODULE, callback_run_batch_task, _}}) ->
            true;
          (_) ->
            false
        end,
        Commands
      ).


%% -------------------------------------------------------------------
%% Functions to print aggregations
%% -------------------------------------------------------------------

-spec use_aggregators([{fun(), [any()]}], boolean()) -> boolean().
use_aggregators(Aggregators, Result) ->
  lists:foldr(
        fun({Function, Parameters}, Acc) ->
          aggregate(apply(Function, Parameters), Acc)
        end,
        Result,
        Aggregators
      ).

-spec my_command_names([any()]) -> [any()].
my_command_names(Commands) ->
  [my_command_name(Command) || {set, {var, _}, Command} <- Commands].

-spec my_command_name(any()) -> any().
my_command_name({call, Module, Function, Parameters}) ->
  {Module, Function, length(Parameters)};
my_command_name(_) ->
  unknown.

-spec absolute_bucket_stats(#state{}) -> [{absolute_bucket_difference, integer()}].
absolute_bucket_stats(State) ->
  [
    {absolute_bucket_difference, Difference}
  ||
    Difference <- State#state.absolute_bucket_stats
  ].

-spec relative_bucket_stats(#state{}) -> [{relative_bucket_difference, float()}].
relative_bucket_stats(State) ->
  [
    {relative_bucket_difference, Difference}
  ||
    Difference <- State#state.relative_bucket_stats
  ].

-spec bucket_sizes(#state{}) -> [{bucket_sizes, {integer(), integer()}}].
bucket_sizes(State) ->
  [
    {bucket_sizes, Sizes}
  ||
    Sizes <- State#state.bucket_stats
  ].

-spec bucket_list_sizes(#state{}) -> [{bucket_list_sizes, {integer(), integer()}}].
bucket_list_sizes(State) ->
  [
    {bucket_list_sizes, Sizes}
  ||
    Sizes <- State#state.bucket_list_stats
  ].


%% -------------------------------------------------------------------
%% Verification of bucket statistics
%% -------------------------------------------------------------------

%% We only accept if we have a quite low number of buckets which are not OK.  Not accepting a single one
%% is not good enough as with a positive probability we will have buckets which are not considered OK.
%% A bucket is not OK if the anonymized value is far off from the real value.
-spec verify_buckets(#state{}) -> boolean().
verify_buckets(State) ->
  case length(State#state.bucket_stats) of
    0 ->
      true;
    NumberOfBuckets ->
      State#state.buckets_not_ok / NumberOfBuckets =< ?PERCENTAGE_MAX_BUCKETS_NOT_OK
  end.


%% -------------------------------------------------------------------
%% PropEr state machine callbacks
%% -------------------------------------------------------------------

-spec command(#state{}) -> proper_gen:generator().
command(#state{db_state=DbState}) ->
  frequency(lists:flatten([
    db_model:command_list(1, DbState),
    [
      {20, run_batch_task(DbState)}
    ||
      db_model:has_at_least_one_table(DbState)
    ]
  ])).

next_state(State, Result, {call, ?MODULE, callback_run_batch_task, [Prefetch]}) ->
  next_state_run_batch_task(State, Result, Prefetch);
next_state(#state{db_state=DbState}=State, Result, Call) ->
  State#state{db_state=db_model:next_state(DbState, Result, Call)}.

precondition(_State, {call, ?MODULE, callback_run_batch_task, _}) ->
  true;
precondition(#state{db_state=DbState}, Call) ->
  true =:= db_model:precondition(DbState, Call).

postcondition(_State, {call, ?MODULE, callback_run_batch_task, _}, {error, _}) ->
  false;
postcondition(_State, {call, ?MODULE, callback_run_batch_task, _}, _Result) ->
  true;
postcondition(#state{db_state=DbState}, Call, Result) ->
  true =:= db_model:postcondition(DbState, Call, Result).


%% -------------------------------------------------------------------
%% Running a batch task
%% -------------------------------------------------------------------

-spec run_batch_task(db_model:db_state()) -> proper_gen:generator().
run_batch_task(DbState) ->
  Prefetch = [
    [{table, db_test:full_table_name(iolist_to_binary(TableName))}, {user_rows, 1}]
  ||
    TableName <- db_model:tables(DbState)
  ],
  return({call, ?MODULE, callback_run_batch_task, [Prefetch]}).

-spec callback_run_batch_task(prefetch_spec()) -> batch_task_result().
callback_run_batch_task(Prefetch) ->
  Task = #task{
    task_id = <<"task-id">>,
    prefetch = Prefetch,
    code = iolist_to_binary([
          "tables = get_user_tables()\n",
          "for index, table_name in ipairs (tables) do \n",
          "  rows = load_user_table(table_name)\n",
          "  for i = 1, #rows do\n",
          "    for column_name, column in pairs (rows[i]) do\n",
          "      report_property (table_name .. \":\" .. column_name, column)\n",
          "    end\n",
          "  end\n",
          "end\n"
        ]),
    return_token = {json, {process, self()}}
  },
  gproc:reg({p, l, {task_listener, Task#task.task_id}}),
  try
    task_coordinator:run_task(Task),
    TaskId = Task#task.task_id,
    receive
      {task, TaskId, {started, TaskCoordinator}} ->
        monitor(process, TaskCoordinator),
        receive
          {reply, Result} ->
            transform_json_result(Result);
          {'DOWN', _, process, TaskCoordinator, ExitReason} ->
            case ExitReason of
              normal ->
                % It's possible that this arrives before the result, so we'll wait a bit more.
                receive
                  {reply, Result} -> transform_json_result(Result)
                after 5000 -> {error, exit_without_result}
                end;
              _ ->
                io:format("~nTask crashed with reason ~p~n", [ExitReason]),
                {error, ExitReason}
            end
        after 60000 ->
          {error, timeout}
        end
    after 5000 ->
      {error, timeout}
    end
  after
    gproc:unreg({p, l, {task_listener, Task#task.task_id}})
  end.

-spec transform_json_result(binary()) -> [{binary(), binary()|undefined, integer()}].
transform_json_result(JSONResult) ->
  {struct, DecodedResult} = mochijson2:decode(JSONResult),
  JSONBuckets = proplists:get_value(<<"buckets">>, DecodedResult),
  lists:sort([transform_json_bucket(Bucket) || {struct, Bucket} <- JSONBuckets]).

-spec transform_json_bucket([{binary(), any()}]) -> {binary(), binary()|undefined, integer()}.
transform_json_bucket(Bucket) ->
  Label = proplists:get_value(<<"label">>, Bucket),
  Value = proplists:get_value(<<"value">>, Bucket, undefined),
  Count = proplists:get_value(<<"count">>, Bucket),
  {Label, Value, Count}.

-spec next_state_run_batch_task(#state{}, any(), prefetch_spec()) -> #state{}.
next_state_run_batch_task(State, {var, _}, _Prefetch) ->
  %% We currently process a symbolic state.  We do not want to change anything.
  State;
next_state_run_batch_task(#state{db_state=DbState}=State, Result, _Prefetch) ->
  ExpectedResult = generate_expected_result(DbState),
  StateWithUpdatedBucketListStats = update_bucket_list_stats(ExpectedResult, Result, State),
  collect_stats_from_results(StateWithUpdatedBucketListStats, ExpectedResult, Result, false).

-spec generate_expected_result(db_model:db_state()) -> batch_task_result().
generate_expected_result(DbState) ->
  Buckets = db_model:fold_tables(
        fun(TableName, Rows, Buckets0) ->
          lists:foldl(
                fun({_UserName, LastRow}, Buckets1) ->
                  lists:foldl(
                      fun({ColumnName, ColumnData}, Buckets2) ->
                        Key = {
                          iolist_to_binary([db_test:full_table_name(TableName), $:, ColumnName]),
                          iolist_to_binary(integer_to_list(ColumnData))
                        },
                        dict:update_counter(Key, 1, Buckets2)
                      end,
                      Buckets1,
                      LastRow
                    )
                end,
                Buckets0,
                Rows
              )
        end,
        dict:new(),
        DbState
      ),
  BucketList = dict:fold(fun({Label, Value}, Count, Acc) -> [{Label, Value, Count}|Acc] end, [], Buckets),
  lists:sort(BucketList).

-spec update_bucket_list_stats(batch_task_result(), batch_task_result(), #state{}) -> #state{}.
update_bucket_list_stats(ExpectedResult, Result, #state{bucket_list_stats=BLStats}=State) ->
  State#state{bucket_list_stats=[{length(ExpectedResult), length(Result)}|BLStats]}.

%% The parameters to this function are the current state and the result lists.
%% The result list consist of tuples of the form
%%    {Label, Value, Count}
%% where Label/Value is the corresponding bucket label and string value and count
%% is the expected count (the first list) resp. the noisy count returned by the cloak.
%% Both lists are sorted such that we can use a linear sweep through both of them.
-spec collect_stats_from_results(#state{}, batch_task_result(), batch_task_result(), boolean()) ->
    false|#state{}.
collect_stats_from_results(State, [], [], _Print) ->
  State;
% skip the LCF bucket
collect_stats_from_results(State, Expected,
    [{?AIRCLOAK_LABEL, ?LCF_TAIL_VALUE, _Count} | Rest], Print) ->
  collect_stats_from_results(State, Expected, Rest, Print);
collect_stats_from_results(State, [{Label, Value, CountExpected}|RestExpected],
    [{Label, Value, Count}|Rest], Print) ->
  collect_stats_from_bucket(State, Label, Value, CountExpected, Count, RestExpected, Rest, Print);
collect_stats_from_results(State, [{ExpectedLabel, ExpectedValue, CountExpected}|RestExpected],
    [{Label, Value, _Count}|_]=Result, Print) when {ExpectedLabel, ExpectedValue} < {Label, Value} ->
  collect_stats_from_bucket(State, ExpectedLabel, ExpectedValue, CountExpected, 0, RestExpected, Result,
      Print);
collect_stats_from_results(State, [{ExpectedLabel, ExpectedValue, CountExpected}|RestExpected], [], Print) ->
  collect_stats_from_bucket(State, ExpectedLabel, ExpectedValue, CountExpected, 0, RestExpected, [], Print);
collect_stats_from_results(_, _, [{Label, Value, _}|_], _Print) ->
  io:format("this should never happen (bucket {~p, ~p} should not exist)!~n", [Label, Value]),
  %% The label-value-pairs of the expected results have to be a super set of the corresponding pairs of the
  %% result generated in the cloak.  So we should never be in this branch.
  false.

-spec collect_stats_from_bucket(#state{}, binary(), binary()|undefined, integer(), integer(),
    batch_task_result(), batch_task_result(), boolean()) -> boolean().
collect_stats_from_bucket(State1, Label, Value, CountExpected, Count, RestExpected, Rest, Print) ->
  #state{absolute_bucket_stats=AbsStats1, relative_bucket_stats=RelStats1, bucket_stats=Stats1} = State1,
  AbsStats2 = [abs(CountExpected - Count)|AbsStats1],
  RelStats2 = if
    Count == 0 ->   %% special case we want to ignore
      RelStats1;
    CountExpected > Count ->
      [(CountExpected / Count) - 1.0|RelStats1];
    CountExpected == Count ->
      [0.0|RelStats1];
    CountExpected < Count ->
      [(Count / CountExpected) - 1.0|RelStats1]
  end,
  Stats2 = [{CountExpected, Count}|Stats1],
  State2 = State1#state{
    absolute_bucket_stats=AbsStats2,
    relative_bucket_stats=RelStats2,
    bucket_stats=Stats2
  },
  State3 = case check_count_difference(CountExpected, Count) of
    true ->
      State2;
    false ->
      case Print of
        true ->
          io:format("bucket {~p, ~p} does not match: ~p <-> ~p~n", [Label, Value, CountExpected, Count]);
        false ->
          ok
      end,
      State2#state{buckets_not_ok=State2#state.buckets_not_ok + 1}
  end,
  collect_stats_from_results(State3, RestExpected, Rest, Print).

%% We want to verify that the expected and the actual counts are within 4SD of each other.
%% If they are not, then something is terribly wrong, either with the anonymisation, or with
%% the data processing itself
-spec check_count_difference(integer(), integer()) -> boolean().
check_count_difference(CountExpected, CountReturned) ->
  CountDifference = abs(CountExpected - CountReturned),
  Params = anonymizer:default_params(),
  SigmaForCount = anonymizer:noise_sigma_for_count(CountExpected, anonymizer:default_params()),
  StaticSigma = Params#anonymizer_params.constant_noise_sd,
  CountDifference =< 4 * (SigmaForCount + StaticSigma).
