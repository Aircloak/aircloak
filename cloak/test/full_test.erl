%% @doc This is a test module to test the full cloak-core suite including the communication with the
%%      PostgreSQL database.  This will include quite intensive tests that always reset the database.
%%
%%      We simulate a full cycle of database creation, data insertion, task runs, and so on.  It will exercise
%%      the system in all its glory.
%%
%%      Current limitations:
%%
%%        - The system does not test removal of columns.  It would be possible to implement those later and
%%          might be a good idea.  For now we just do not support them.
%%
%%        - The system always retrieves the last row for each user stored in the data-base.  So we do not see
%%          if the database looses any data.  In fact this should be covered by a special data-prefetch and
%%          -insertion test.  The reason to not do that here is that we keep the system simple and that we
%%          keep the data pushed into the sandboxes small such that this PropEr test won't take too long.
%%          This is important to keep the automatic tests on the Travis CI fast enough that they don't slow us
%%          down too much.
%%
%%        - Table data is always of the type integer.
%% @end
-module(full_test).

-include("deps/proper/include/proper.hrl").
-include("src/cloak.hrl").
-include("test/prop_tracer.hrl").

%% API
-export([
  prop_full/5
]).

%% State machine API
-export([
  add_seen_users_table/0,
  add_table/3,
  remove_table/2,
  clear_table/1,
  add_column_to_table/3,
  add_lookup_table/2,
  remove_lookup_table/1,
  add_data/2,
  execute_batch_task/3,
  add_streaming_task/3,
  add_periodic_task/2
]).

%% Property state machine callbacks
-export([
  command/1,
  initial_state/0,
  next_state/3,
  precondition/2,
  postcondition/3
]).

-define(MAX_INITIAL_COLUMNS, 4).
-define(MAX_TABLES, 2).
-define(USERS, 100).
-define(USER_FREQ_YES, 2).
-define(USER_FREQ_NO, 1).
-define(VALUES, 4).
-define(TABLE_FREQ_YES, 1).
-define(TABLE_FREQ_NO, 1).
-define(ANALYST_ID, 5).
-define(MAX_LOOKUP_TABLES, 5).
-define(LOOKUP_TABLE_SIZE, 10).
-define(LOOKUP_MISS_PERCENTAGE, 5).
-define(STREAMING_TASK_REPORT_INTERVAL, 500).
-define(STREAMING_TASK_REPEATS, (30000 div ?STREAMING_TASK_REPORT_INTERVAL)).  % at least 30s
-define(ACCUMULATOR_REPETITIONS, 5).
-define(PERIODIC_TASK_PERIOD_SEC, 1).
-define(PERIODIC_TASK_PERIOD_MS, (?PERIODIC_TASK_PERIOD_SEC * 1000)).
-define(PERIODIC_TASK_REPEATS, (30000 div ?PERIODIC_TASK_PERIOD_MS)).  % at least 30s
-define(WAIT_FOR_INSERT_SLEEP, 1000).
-define(WAIT_FOR_INSERT_REPEATS, 30).


%% -------------------------------------------------------------------
%% Internal state representation and initial state
%% -------------------------------------------------------------------

-record(state, {
  cloak_node :: node(),
  users = ?USERS :: pos_integer(),
  values = ?VALUES :: pos_integer(),
  max_tables = ?MAX_TABLES :: pos_integer(),
  max_initial_columns = ?MAX_INITIAL_COLUMNS :: pos_integer(),
  max_lookup_tables = ?MAX_LOOKUP_TABLES :: pos_integer(),
  removed_tables = dict:new() :: dict:dict(),
  tables = dict:new() :: dict:dict(),
  lookup_tables = dict:new() :: dict:dict(),
  buckets_not_ok = 0 :: non_neg_integer(),
  absolute_bucket_stats = [] :: [non_neg_integer()],
  relative_bucket_stats = [] :: [float()],
  bucket_stats = [] :: [{non_neg_integer(), non_neg_integer()}],
  bucket_list_stats = [] :: [{non_neg_integer(), non_neg_integer()}],
  streaming_task :: undefined | abandoned | #task{},
  streaming_task_collector :: undefined | pid(),
  use_accumulator = false :: boolean(),
  periodic_task :: undefined | abandoned | #task{},
  periodic_task_collector :: undefined | pid,
  parallel_task :: undefined | streaming | periodic,
  seen_users_table_created = false :: boolean()
}).

-record(table_spec, {
  columns :: [string()],
  column_num :: integer(),
  migration :: non_neg_integer(),
  rows = dict:new() :: dict:dict()
}).

initial_state() ->
  #state{}.


%% -------------------------------------------------------------------
%% Starting PropEr
%% -------------------------------------------------------------------

prop_full(Users, Values, MaxTables, MaxInitialColumns, CommandsFactor) ->
  CloakNode = proper_cloak_instance:default_cloak_node(),
  InitialState = #state{
    cloak_node = CloakNode,
    users = Users,
    values = Values,
    max_tables = MaxTables,
    max_initial_columns = MaxInitialColumns
  },
  ?FORALL(Commands, more_commands(CommandsFactor, proper_statem:commands(?MODULE, InitialState)),
      ?TRACEFAIL(Commands,
          begin
            proper_cloak_instance:stop_instance(),
            {ok, CloakNode} = proper_cloak_instance:start_instance(),
            proper_statem:run_commands(?MODULE, Commands)
          end,
          measure("Command sequence length", length(Commands),
              aggregate([{absolute_bucket_difference, S} || S <- State#state.absolute_bucket_stats],
                  aggregate([{relative_bucket_difference, S} || S <- State#state.relative_bucket_stats],
                      aggregate(
                            [{bucket_sizes, S1, S2} || {S1, S2} <- State#state.bucket_stats],
                            aggregate([
                                {bucket_list_sizes, S1, S2} ||
                                    {S1, S2} <- State#state.bucket_list_stats],
                                aggregate(my_command_names(Commands), Result =:= ok))
                          ))))
          )).


%% -------------------------------------------------------------------
%% Command generation (PropEr state machine callbacks)
%% -------------------------------------------------------------------

my_command_names(Commands) ->
  [my_command_name(Command) || {set, {var, _}, Command} <- Commands].

my_command_name({call, rpc, call, [Node, Module, Function, Parameters]}) ->
  {Node, Module, Function, length(Parameters)};
my_command_name({call, Module, Function, Parameters}) ->
  {node(), Module, Function, length(Parameters)}.

command(#state{seen_users_table_created=SeenUsersTableCreated} = State) ->
  frequency(lists:flatten([
    [{1, add_seen_users_table_call(State)}]
  || not SeenUsersTableCreated] ++ [
    [{2, add_table_call(State)}]
  || SeenUsersTableCreated andalso dict:size(State#state.tables) < State#state.max_tables] ++ [
    [{2, add_lookup_table_call(State)}]
  || SeenUsersTableCreated andalso dict:size(State#state.lookup_tables) < State#state.max_lookup_tables] ++
    [{2, add_parallel_task(State)}
  || SeenUsersTableCreated andalso dict:size(State#state.tables) > 0 andalso State#state.parallel_task =:= undefined] ++ [
    [{1, remove_table_call(State)},
    {1, clear_table_call(State)},
    {1, remove_lookup_table_call(State)},
    {1, add_column_to_table_call(State)},
    {20, add_data_call(State)},
    {10, execute_batch_task_call(State)}]
  ||
    SeenUsersTableCreated andalso
    dict:size(State#state.tables) > 0 andalso
    dict:size(State#state.lookup_tables) > 0 andalso
    State#state.parallel_task =/= undefined]
  )).

add_seen_users_table_call(#state{cloak_node=CloakNode}) ->
  return({call, rpc, call, [CloakNode, ?MODULE, add_seen_users_table, []]}).

add_table_call(#state{cloak_node=CloakNode}=State) ->
  ?LET({NewTableName, MigrationNum}, return(table_name_and_initial_migration(State)),
      ?LET(Columns, integer(1, State#state.max_initial_columns),
          begin
            NewColumns = [column_name(I) || I <- lists:seq(1, Columns)],
            {call, rpc, call, [CloakNode, ?MODULE, add_table,
                [return(NewTableName), return(MigrationNum), return(NewColumns)]]}
          end)).

remove_table_call(#state{cloak_node=CloakNode}=State) ->
  Tables = dict:to_list(State#state.tables),
  ?LET({ToRemove, TableSpec}, oneof([return(Table) || Table <- Tables]),
      {call, rpc, call, [CloakNode, ?MODULE, remove_table,
          [return(ToRemove), return(TableSpec#table_spec.migration + 1)]]}).

clear_table_call(#state{cloak_node=CloakNode}=State) ->
  Tables = dict:to_list(State#state.tables),
  ?LET({ToRemove, _}, oneof([return(Table) || Table <- Tables]),
      {call, rpc, call, [CloakNode, ?MODULE, clear_table,
          [return(ToRemove)]]}).

add_lookup_table_call(#state{cloak_node=CloakNode}=State) ->
  ?LET(NewTableName, return(lookup_table_name(State)),
      ?LET(Num, integer(1, ?LOOKUP_TABLE_SIZE),
          begin
            Data = [kv_pair(I) || I <- lists:seq(1, Num)],
            {call, rpc, call, [CloakNode, ?MODULE, add_lookup_table, [NewTableName, Data]]}
          end)).

remove_lookup_table_call(#state{cloak_node=CloakNode}=State) ->
  Tables = dict:to_list(State#state.lookup_tables),
  ?LET({Table, _}, oneof([return(Table) || Table <- Tables]),
      {call, rpc, call, [CloakNode, ?MODULE, remove_lookup_table, [return(Table)]]}).

add_column_to_table_call(#state{cloak_node=CloakNode}=State) ->
  Tables = dict:fetch_keys(State#state.tables),
  ?LET(Table, oneof([return(Table) || Table <- Tables]),
      ?LET(TableSpec, return(dict:fetch(Table, State#state.tables)),
          {call, rpc, call, [CloakNode, ?MODULE, add_column_to_table, [
                return(Table),
                return(TableSpec#table_spec.column_num+1),
                return(TableSpec#table_spec.migration+1)
              ]]})).

generate_table_data(State, {TableName, TableSpec}) ->
  {iolist_to_binary(TableName), [
    {columns, [iolist_to_binary(Column) || Column <- TableSpec#table_spec.columns]},
    {data, [[random:uniform(State#state.values) || _ <- TableSpec#table_spec.columns]]}
  ]}.

add_data_for_user(User, State) ->
  ?LET(UserData,
      common_generators:subset_generic(
          fun(Table) -> return(generate_table_data(State, Table)) end,
          dict:to_list(State#state.tables),
          ?TABLE_FREQ_YES,
          ?TABLE_FREQ_NO),
      return({iolist_to_binary(User), UserData})).

add_data_call(#state{cloak_node=CloakNode}=State) ->
  ?LET(Data,
      common_generators:subset_generic(
          fun(I) ->
            UserName = "user" ++ integer_to_list(I),
            add_data_for_user(UserName, State)
          end,
          lists:seq(1, State#state.users),
          ?USER_FREQ_YES,
          ?USER_FREQ_NO),
      return({call, rpc, call, [CloakNode, ?MODULE, add_data,
          [State#state.use_accumulator, return([{User, List} || {User, List} <- Data, List /= []])]]})).

execute_batch_task_call(#state{cloak_node=CloakNode, tables=Tables, lookup_tables=LookupTables}) ->
  Prefetch = [[{table, iolist_to_binary(TableName)}, {user_rows, 1}] || TableName <- dict:fetch_keys(Tables)],
  AllLookupTables = [
      begin
        LookupMiss = (random:uniform(100) < ?LOOKUP_MISS_PERCENTAGE),
        Index = case LookupMiss of
          true -> -1;
          false -> random:uniform(Size)
        end,
        {TableName, Index, LookupMiss}
      end || {TableName, Size} <- dict:to_list(LookupTables)],
  Lookup = lists:nth(random:uniform(length(AllLookupTables)), AllLookupTables),
  {Mega, Sec, Micro} = now(),
  BatchTaskRunId = integer_to_list(Mega * 1000000 * 1000000 + Sec * 1000000 + Micro),
  return({call, rpc, call, [CloakNode, ?MODULE, execute_batch_task, [Prefetch, Lookup, BatchTaskRunId]]}).

add_parallel_task(State) ->
  case random:uniform(2) of
    1 -> add_streaming_task_call(State);
    2 -> add_periodic_task_call(State)
  end.

add_streaming_task_call(#state{cloak_node=CloakNode}=State) ->
  ?LET(Table, oneof([return(Table) || Table <- dict:fetch_keys(State#state.tables)]),
      begin
        UseAccumulator = (random:uniform(2) == 1),
        TaskID = iolist_to_binary(io_lib:format("task ~p", [now()])),
        {call, rpc, call, [CloakNode, ?MODULE, add_streaming_task, [
          #task{
            task_id=TaskID,
            type=streaming,
            user_expire_interval=10000,
            analyst_id=?ANALYST_ID,
            prefetch=[[{table, Table}]],
            report_interval=?STREAMING_TASK_REPORT_INTERVAL,
            code = iolist_to_binary(streaming_task_code(UseAccumulator))
          },
          start_task_collector(?ANALYST_ID, TaskID, CloakNode),
          UseAccumulator
        ]]}
      end).

streaming_task_code(false) ->
  [
    "tables = get_user_tables()\n",
    "for index, table_name in ipairs (tables) do\n",
    "  rows = load_user_table(table_name)\n",
    "  for i = 1, #rows do\n",
    "    for column_name, column in pairs (rows[i]) do\n",
    "      report_property (table_name .. \":\" .. column_name, column)\n",
    "    end\n",
    "  end\n",
    "end"
  ];
streaming_task_code(true) ->
  % When testing accumulator, we'll increase it by one until we reach the target
  % step, and only then we'll report property.
  [
    "accumulator = accumulator or 1\n",
    " if accumulator == ", integer_to_list(?ACCUMULATOR_REPETITIONS), " then\n",
    streaming_task_code(false),
    " end\n",
    "accumulator = accumulator + 1"
  ].

add_periodic_task_call(#state{cloak_node=CloakNode}=State) ->
  ?LET(Table, oneof([return(Table) || Table <- dict:fetch_keys(State#state.tables)]),
      begin
        TaskID = iolist_to_binary(io_lib:format("task ~p", [now()])),
        {call, rpc, call, [CloakNode, ?MODULE, add_periodic_task, [
          #task{
            task_id=TaskID,
            type=periodic,
            period=?EVERY_N_SEC(?PERIODIC_TASK_PERIOD_SEC),
            analyst_id=?ANALYST_ID,
            prefetch=[[{table, Table}, {user_rows, 1}]],
            code = iolist_to_binary(periodic_task_code())
          },
          start_task_collector(?ANALYST_ID, TaskID, CloakNode)
        ]]}
      end).

periodic_task_code() ->
  [
    "tables = get_user_tables()\n",
    "for index, table_name in ipairs (tables) do\n",
    "  rows = load_user_table(table_name)\n",
    "  for i = 1, #rows do\n",
    "    for column_name, column in pairs (rows[i]) do\n",
    "      report_property (table_name .. \":\" .. column_name, column)\n",
    "    end\n",
    "  end\n",
    "end"
  ].

%% -------------------------------------------------------------------
%% Rest of PropEr state machine callbacks
%% -------------------------------------------------------------------

next_state(State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, add_seen_users_table, []]}) ->
  State#state{seen_users_table_created=true};
next_state(State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, add_table,
    [NewTableName, MigrationNum, NewColumns]]}) ->
  error = dict:find(NewTableName, State#state.tables),
  TableSpec = #table_spec{
    columns = NewColumns,
    column_num = length(NewColumns),
    migration = MigrationNum
  },
  RemovedTables = dict:erase(NewTableName, State#state.removed_tables),
  State#state{tables=dict:store(NewTableName, TableSpec, State#state.tables), removed_tables=RemovedTables};
next_state(State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, remove_table, [TableName, _NewMigration]]}) ->
  State1 = case State#state.streaming_task of
    % We removed the table related to the queued streaming task, so we'll defer from verifying
    % task results.
    #task{prefetch=[[{table, TableName}]]} -> State#state{streaming_task=abandoned};
    _ -> State
  end,
  State2 = case State1#state.periodic_task of
    % We removed the table related to the queued streaming task, so we'll defer from verifying
    % task results.
    #task{prefetch=[[{table, TableName} | _]]} -> State1#state{periodic_task=abandoned};
    _ -> State1
  end,
  {ok, TableSpec} = dict:find(TableName, State2#state.tables),
  State2#state{
    tables=dict:erase(TableName, State2#state.tables),
    removed_tables=dict:store(TableName, TableSpec#table_spec.migration+1, State2#state.removed_tables)
  };
next_state(State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, clear_table, [TableName]]}) ->
  {ok, TableSpec} = dict:find(TableName, State#state.tables),
  State#state{
    tables=dict:store(TableName, TableSpec#table_spec{rows=dict:new()}, State#state.tables)
  };
next_state(State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, add_lookup_table, [NewTableName, Data]]}) ->
  error = dict:find(NewTableName, State#state.lookup_tables),
  State#state{lookup_tables=dict:store(NewTableName, length(Data), State#state.lookup_tables)};
next_state(State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, remove_lookup_table, [TableName]]}) ->
  {ok, _} = dict:find(TableName, State#state.lookup_tables),
  State#state{lookup_tables=dict:erase(TableName, State#state.lookup_tables)};
next_state(State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, add_column_to_table,
    [TableName, NewColumn, NewMigration]]}) ->
  NewColumnName = column_name(NewColumn),
  TableSpec1 = dict:fetch(TableName, State#state.tables),
  OldColumns = TableSpec1#table_spec.columns,
  TableSpec2 = TableSpec1#table_spec{
    column_num=NewColumn,
    columns=OldColumns++[NewColumnName],
    migration=NewMigration
  },
  State#state{tables=dict:store(TableName, TableSpec2, State#state.tables)};
next_state(#state{tables=Tables1}=State, _Res, {call, rpc, call, [_CloakNode, ?MODULE, add_data,
    [_UseAccumulator, Data]]}) ->
  Tables2 = lists:foldl(
        fun({User, UserRows}, Ts1) ->
          lists:foldl(
                fun({TableName, [{columns, ColumnNames}, {data, [ColumnDatas]}]}, Ts2) ->
                  TableSpec1 = dict:fetch(TableName, Ts2),
                  Rows1 = TableSpec1#table_spec.rows,
                  Columns = lists:zip(ColumnNames, ColumnDatas),
                  Rows2 = dict:store(User, Columns, Rows1),
                  TableSpec2 = TableSpec1#table_spec{rows=Rows2},
                  dict:store(TableName, TableSpec2, Ts2)
                end,
                Ts1,
                UserRows
              )
        end,
        Tables1,
        Data
      ),
  State#state{tables=Tables2};
next_state(State, Result, {call, rpc, call, [_CloakNode, ?MODULE, execute_batch_task, [_Prefetch, Lookup, _BatchTaskRunId]]}) ->
  update_state_after_execute(Result, Lookup, State, false);
next_state(State, _Result, {call, rpc, call, [_CloakNode, ?MODULE, add_streaming_task,
    [Task, Aggregator, UseAccumulator]]}) ->
  State#state{streaming_task = Task, streaming_task_collector=Aggregator, use_accumulator=UseAccumulator, parallel_task=streaming};
next_state(State, _Result, {call, rpc, call, [_CloakNode, ?MODULE, add_periodic_task, [Task, Aggregator]]}) ->
  State#state{periodic_task=Task, periodic_task_collector=Aggregator, parallel_task=periodic}.

precondition(#state{seen_users_table_created=SeenUsersTableCreated},
    {call, rpc, call, [_CloakNode, ?MODULE, add_seen_users_table, []]}) ->
  not SeenUsersTableCreated;
precondition(#state{max_tables=MaxTables, tables=Tables},
    {call, rpc, call, [_CloakNode, ?MODULE, add_table, [_, _, _]]}) ->
  dict:size(Tables) < MaxTables;
precondition(
  #state{max_lookup_tables=MaxLookupTables, lookup_tables=LookupTables},
  {call, rpc, call, [_CloakNode, ?MODULE, add_lookup_table, [_, _]]}
) ->
  dict:size(LookupTables) < MaxLookupTables;
precondition(#state{lookup_tables=LookupTables},
    {call, rpc, call, [_CloakNode, ?MODULE, remove_lookup_table, [_]]}) ->
  dict:size(LookupTables) > 0;
precondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, execute_batch_task, [_Prefetch, _Lookup, _BatchTaskRunId]]}) ->
  true;
precondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_streaming_task,
    [_Task, _Aggregator, _UseAccumulator]]}) ->
  true;
precondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_periodic_task, [_Task, _Aggregator]]}) ->
  true;
precondition(#state{tables=Tables}, {call, rpc, call, [_CloakNode, ?MODULE, Function, Params]}) ->
  case dict:size(Tables) > 0 of
    true -> precondition(Tables, Function, Params);
    false -> false
  end.

precondition(Tables, remove_table, [TableName, NewMigration]) ->
  case dict:find(TableName, Tables) of
    {ok, #table_spec{migration=Migration}} -> NewMigration == Migration + 1;
    error -> false
  end;
precondition(Tables, clear_table, [TableName]) ->
  case dict:find(TableName, Tables) of
    {ok, _} -> true;
    error -> false
  end;
precondition(Tables, add_column_to_table, [TableName, NewColumn, NewMigration]) ->
  case dict:find(TableName, Tables) of
    {ok, #table_spec{column_num=ColumnNum, migration=Migration}} ->
      ColumnNum == NewColumn - 1 andalso NewMigration == Migration + 1;
    error ->
      false
  end;
precondition(Tables, add_data, [_UseAccumulator, Data]) ->
  lists:all(
      fun({_User, UserRows}) ->
        lists:all(
            fun({TableName, [{columns, Columns}|_]}) ->
              case dict:find(TableName, Tables) of
                {ok, TableSpec} -> length(Columns) == TableSpec#table_spec.column_num;
                error -> false
              end
            end,
            UserRows)
      end,
      Data).

postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_seen_users_table, []]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_table, [_, _, _]]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, remove_table, [_, _]]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, clear_table, [_]]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_lookup_table, [_, _]]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, remove_lookup_table, [_]]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_column_to_table, [_, _, _]]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_data, [_, _]]}, Result) ->
  Result == true;
postcondition(State, {call, rpc, call, [_CloakNode, ?MODULE, execute_batch_task, [Prefetch, Lookup, BatchTaskRunId]]}, Result) ->
  verify_streaming_task(State) andalso
  verify_periodic_task(State) andalso
  verify_seen_users(BatchTaskRunId, State) andalso
  verify_result(Result, Lookup, Prefetch, State, true);
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_streaming_task, _]}, Result) ->
  Result == ok;
postcondition(_State, {call, rpc, call, [_CloakNode, ?MODULE, add_periodic_task, _]}, Result) ->
  Result == ok.


%% -------------------------------------------------------------------
%% Verify results
%% -------------------------------------------------------------------

verify_result(Result, Lookup, Prefetch, State1, Print) ->
  State2 = update_state_after_execute(Result, Lookup, State1, Print),
  case {State2#state.buckets_not_ok == 0, Print} of
    {false, true} ->
      Tables = State2#state.tables,
      io:format("Prefetch: ~p~nTables: ~p~nExpectedResult: ~p~nResult: ~p~n",
          [Prefetch, dict:to_list(Tables), generate_expected_result(Tables, Lookup), Result]),
      false;
    {VerifyResult, _} ->
      VerifyResult
  end.

update_state_after_execute({var, _}, _, State, _Print) ->
  %% symbolic state transition (here we do not need to do anything)
  State;
update_state_after_execute(Result, Lookup, #state{tables=Tables, bucket_list_stats=Stats1}=State1, Print) ->
  ExpectedResult = generate_expected_result(Tables, Lookup),
  Stats2 = [{length(ExpectedResult), length(Result)}|Stats1],
  State2 = State1#state{bucket_list_stats=Stats2},
  collect_stats_from_results(State2, ExpectedResult, Result, Print).

%% The parameters to this function are the current state and the result lists.
%% The result list consist of tuples of the form
%%    {Label, Value, Count}
%% where Label/Value is the corresponding bucket label and string value and count
%% is the expected count (the first list) resp. the noisy count returned by the cloak.
%% Both lists are sorted such that we can use a linear sweep through both of them.
collect_stats_from_results(State, [], [], _Print) ->
  State;
%% Skip the LCF bucket
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

%% From each batch task execution we insert users entry into seen_users table.
%% Here, we verify whether inserted rows are there.
verify_seen_users(BatchTaskRunId, State) ->
  verify_seen_users(?WAIT_FOR_INSERT_REPEATS, BatchTaskRunId, State).

verify_seen_users(Attempts, BatchTaskRunId, State) ->
  Fun = fun(Connection) ->
    sql_conn:simple_query(
          ["SELECT distinct ac_user_id FROM ",
            analyst_tables:sanitized_full_name(?ANALYST_ID, user, <<"seen_users">>),
            " WHERE c1='", BatchTaskRunId ,"' and c2=true and c3=3.14 and c4=42"],
          Connection
        )
  end,
  {{select, _}, DbUsers} = rpc:call(State#state.cloak_node, cloak_db, call, [undefined, Fun]),
  DbUsersSet = sets:from_list([UserId || {UserId} <- DbUsers]),
  ExpectedUsersSet = sets:from_list(
        [UserId ||
          {_, #table_spec{rows=Rows}} <- dict:to_list(State#state.tables),
          {UserId, _} <- dict:to_list(Rows)]
      ),
  case sets:size(sets:subtract(ExpectedUsersSet, DbUsersSet)) =:= 0 of
    true -> true;
    false ->
      if
        Attempts > 0 ->
          % In case rows are still not inserted, sleep awhile and retry
          timer:sleep(?WAIT_FOR_INSERT_SLEEP),
          verify_seen_users(Attempts - 1, BatchTaskRunId, State);
        true ->
          io:format("Expected inserted users ~p, but found ~p~n",
              [lists:sort(sets:to_list(ExpectedUsersSet)), lists:sort(sets:to_list(DbUsersSet))]),
          false
      end
  end.

%% -------------------------------------------------------------------
%% State machine API
%% -------------------------------------------------------------------

add_seen_users_table() ->
  Migration = {<<"seen_users">>, 0, {create, [
    {<<"c1">>, text, []},
    {<<"c2">>, boolean, []},
    {<<"c3">>, float, []},
    {<<"c4">>, integer, []}
  ]}},
  user_table_migration:migrate(?ANALYST_ID, Migration).

add_table(TableName, MigrationNum, Columns) ->
  Migration = {
    iolist_to_binary(TableName),
    MigrationNum,
    {create, [{iolist_to_binary(ColumnName), integer, []} || ColumnName <- Columns]}
  },
  user_table_migration:migrate(?ANALYST_ID, Migration).

remove_table(TableName, NewMigration) ->
  Migration = {
    iolist_to_binary(TableName),
    NewMigration,
    drop
  },
  user_table_migration:migrate(?ANALYST_ID, Migration).

clear_table(TableName) ->
  user_tables:clear_table(?ANALYST_ID, TableName).

add_lookup_table(TableName, Data) ->
  lookup_table_migrator:migrate(?ANALYST_ID, TableName, Data).

remove_lookup_table(TableName) ->
  lookup_table_migrator:remove(?ANALYST_ID, TableName).

add_column_to_table(TableName, NewColumn, NewMigration) ->
  Migration = {
    iolist_to_binary(TableName),
    NewMigration,
    {alter, [{add, {iolist_to_binary(column_name(NewColumn)), integer, []}}]}
  },
  user_table_migration:migrate(?ANALYST_ID, Migration).

add_data(UseAccumulator, Data) ->
  [({_, []} = insert_validator:validate(?ANALYST_ID, Rows)) || {_, Rows} <- Data],
  % When using accumulator, we'll repeat insertions to test accumulator propagation
  NumInsertions = case UseAccumulator of
    true -> ?ACCUMULATOR_REPETITIONS;
    false -> 1
  end,
  Results = [inserter:add_users_data(?ANALYST_ID, Data) || _ <- lists:seq(1, NumInsertions)],
  lists:all(fun(X) -> X =:= ok end, Results).

execute_batch_task(Prefetch, {TableName, KeyIndex, _}, BatchTaskRunId) ->
  Task = #task{
    task_id = <<"task-id">>,
    analyst_id = ?ANALYST_ID,
    prefetch = Prefetch,
    code = iolist_to_binary([
          "tables = get_user_tables()\n",
          "for index, table_name in ipairs (tables) do\n",
          "  rows = load_user_table(table_name)\n",
          "  for i = 1, #rows do\n",
          "    for column_name, column in pairs (rows[i]) do\n",
          "       if lookup('", TableName, "', '", element(1, kv_pair(KeyIndex)),"') then\n",
          "         report_property (table_name .. \":\" .. column_name, column)\n",
          "       end\n",
          "    end\n",
          "  end\n",
          "end\n",
          "insert_row('seen_users', {c1 = '", BatchTaskRunId ,"', c2=true, c3=3.14, c4=42})"
        ]),
    return_token = {json, {process, self()}}
  },
  task_coordinator:run_task(Task),
  receive
    {reply, Result} ->
      transform_json_result(Result)
  after 60000 ->
    {error, timeout}
  end.

add_streaming_task(Task, Aggregator, _UseAccumulator) ->
  cluster_cache:put_task(Task#task{return_token={json, {process, Aggregator}}}),
  streaming_task_reporter:ensure_started(Task#task.analyst_id, Task#task.task_id),
  ok.

add_periodic_task(Task, Aggregator) ->
  PeriodicTask = Task#task{return_token={json, {process, Aggregator}}},
  cluster_cache:put_task(PeriodicTask),
  periodic_task_runner:setup_cron(PeriodicTask),
  ok.

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

table_name_and_initial_migration(State) ->
  I = random:uniform(State#state.max_tables * 1000),
  TableName = iolist_to_binary(["table", integer_to_list(I)]),
  case dict:find(TableName, State#state.tables) of
    error ->
      MigrationNum = case dict:find(TableName, State#state.removed_tables) of
        {ok, MN} -> MN;
        error -> 0
      end,
      {TableName, MigrationNum};
    _ ->
      table_name_and_initial_migration(State)
  end.

column_name(I) ->
  iolist_to_binary(["column", integer_to_list(I)]).

generate_expected_result(_, {_, _, true}) -> [];
generate_expected_result(Tables, {_, _, false}) ->
  Buckets = dict:fold(fun(TableName, TableSpec, Buckets0) ->
        dict:fold(fun(_UserName, LastRow, Buckets1) ->
              lists:foldl(fun({ColumnName, ColumnData}, Buckets2) ->
                    Key = {
                      iolist_to_binary([TableName, $:, ColumnName]),
                      iolist_to_binary(integer_to_list(ColumnData))
                    },
                    dict:update_counter(Key, 1, Buckets2)
                  end, Buckets1, LastRow)
            end, Buckets0, TableSpec#table_spec.rows)
      end, dict:new(), Tables),
  BucketList = dict:fold(fun({Label, Value}, Count, Acc) -> [{Label, Value, Count}|Acc] end, [], Buckets),
  lists:sort(BucketList).

transform_json_result(JSONResult) ->
  {struct, DecodedResult} = mochijson2:decode(JSONResult),
  JSONBuckets = proplists:get_value(<<"buckets">>, DecodedResult),
  lists:sort([transform_json_bucket(Bucket) || {struct, Bucket} <- JSONBuckets]).

transform_json_bucket(Bucket) ->
  Label = proplists:get_value(<<"label">>, Bucket),
  Value = proplists:get_value(<<"value">>, Bucket, undefined),
  Count = proplists:get_value(<<"count">>, Bucket),
  {Label, Value, Count}.

lookup_table_name(State) ->
  I = random:uniform(State#state.max_lookup_tables * 1000),
  TableName = iolist_to_binary(["lookup_table", integer_to_list(I)]),
  case dict:find(TableName, State#state.lookup_tables) of
    error -> TableName;
    _ -> lookup_table_name(State)
  end.

kv_pair(I) ->
  {
    << <<"k">>/binary, (integer_to_binary(I))/binary >>,
    << <<"v">>/binary, (integer_to_binary(I))/binary >>
  }.


%% -------------------------------------------------------------------
%% Streaming and periodic task result collection
%% -------------------------------------------------------------------

%% Streaming and periodic task reports are collected separately to avoid clashing with
%% normal task workflow.
start_task_collector(AnalystID, TaskID, CloakNode) ->
  spawn_link(fun() -> task_collector(AnalystID, TaskID, CloakNode, undefined) end).

task_collector(AnalystID, TaskID, CloakNode, Result) ->
  receive
    {reply, NewResult} ->
      task_collector(AnalystID, TaskID, CloakNode, NewResult);
    {get, From} ->
      From ! {result, Result},
      task_collector(AnalystID, TaskID, CloakNode, Result);
    stop ->
      rpc:call(CloakNode, cluster_cache, delete_task, [AnalystID, TaskID]),
      ok;
    _ ->
      task_collector(AnalystID, TaskID, CloakNode, Result)
  end.

get_task_result(Pid) ->
  Pid ! {get, self()},
  receive
    {result, undefined} -> undefined;
    {result, Result} -> transform_json_result(Result)
  after timer:seconds(5) -> undefined
  end.

simplify_state(Task, State) ->
  [[{table, Table} | _]] = Task#task.prefetch,
  State#state{tables=dict:from_list([{Table, dict:fetch(Table, State#state.tables)}])}.

verify_collected_results(undefined, _Collector, _Repeats, _ReportInteval, _State) -> true;
verify_collected_results(abandoned, _Collector, _Repeats, _ReportInteval, _State) -> true;
verify_collected_results(_Task, _Collector, 0, _ReportInteval, _State) -> false;
verify_collected_results(Task, Collector, Repeats, ReportInteval, State) ->
  case process_info(Collector) of
    % Hacky due to how proper state machine works. On first retrieval, the collector
    % process terminates normally, so we don't perform excessive checks. Since the
    % collector is linked to the caller, any abnormal termination will cause an
    % error anyway.
    undefined -> true;
    _ ->
      timer:sleep(round(1.5 * ReportInteval)),
      DummyLookup = {"", 0, false},
      Result = get_task_result(Collector),
      case verify_result(Result, DummyLookup, Task#task.prefetch, simplify_state(Task, State), Repeats == 1) of
        false ->
          verify_collected_results(Task, Collector, Repeats - 1, ReportInteval, State);
        true ->
          Collector ! stop,
          true
      end
  end.

verify_streaming_task(State) ->
  verify_collected_results(State#state.streaming_task, State#state.streaming_task_collector,
      ?STREAMING_TASK_REPEATS, ?STREAMING_TASK_REPORT_INTERVAL, State).

verify_periodic_task(State) ->
  verify_collected_results(State#state.periodic_task, State#state.periodic_task_collector,
      ?PERIODIC_TASK_REPEATS, ?PERIODIC_TASK_PERIOD_MS, State).
