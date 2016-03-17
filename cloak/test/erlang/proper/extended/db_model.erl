%% @doc This module contains code to simulate a model of the database for property testing.
%%
%%      It basically provides facilities to insert data into the database and retrieve all data from the
%%      database.
%%
%%      Current limitations:
%%
%%        - Table data is always of the type integer.
%%
%%        - We only store the latest row per user and table.  This has to be taken into account when fetching
%%          data from the real table.
-module(db_model).

-include("deps/proper/include/proper.hrl").
-include("src/user_data.hrl").
-include("full_test.hrl").

%% API
-export([
  create/4,
  has_at_least_one_table/1,
  command_list/3,
  command_list_has_at_least_one_add_data/1,
  tables/1,
  fold_tables/3,
  rows_of_table/2
]).

%% Command generators
-export([
  add_table/2,
  remove_table/2,
  clear_table/2,
  add_column_to_table/2,
  add_data/2
]).

%% Callbacks for PropEr (these callbacks are designed to be combinable with callbacks of other modules).
-export([
  next_state/3,
  precondition/2,
  postcondition/3
]).

%% Callbacks for adding a table
-export([
  callback_add_table/3,
  next_state_add_table/5,
  precondition_add_table/4,
  postcondition_add_table/5
]).

%% Callbacks for removing a table
-export([
  callback_remove_table/2,
  next_state_remove_table/4,
  precondition_remove_table/3,
  postcondition_remove_table/4
]).

%% Callbacks for clearing a table
-export([
  callback_clear_table/1,
  next_state_clear_table/3,
  precondition_clear_table/2,
  postcondition_clear_table/3
]).

%% Callbacks for adding a column to a table
-export([
  callback_add_column_to_table/3,
  next_state_add_column_to_table/5,
  precondition_add_column_to_table/4,
  postcondition_add_column_to_table/5
]).

%% Callbacks for adding data to a table
-export([
  callback_add_data/2,
  next_state_add_data/4,
  precondition_add_data/3,
  postcondition_add_data/4
]).


%% -------------------------------------------------------------------
%% The representation of the state of the database.
%% -------------------------------------------------------------------

-record(table_spec, {
  columns :: [string()], %% The columns represented by name.
  column_num :: non_neg_integer(), %% The number of columns.
  migration :: non_neg_integer(), %% The current migration number.
  rows = dict:new() :: dict:dict(string(), [integer()]) %% The current set of rows.
}).

-record(state, {
  users :: pos_integer(), %% The number of users.
  values :: pos_integer(), %% This is the maximum value (+1) for data.
  max_tables :: pos_integer(), %% The maximum number of tables that get generated.
  max_initial_columns :: pos_integer(), %% The maximum number of columns initially created.
  tables = dict:new() :: dict:dict(string(), #table_spec{}) %% The stored tables.
}).
-type db_state() :: #state{}.
-export_type([db_state/0]).

-type db_rows() :: [{binary(), [{binary(), non_neg_integer()}]}].
-export_type([db_rows/0]).

% Auxiliary type used internally to represent the current contents of the database (for each user the last
% inserted row).
-type db_contents() :: [{string(), table_contents()}].
-type table_contents() :: {{select, non_neg_integer()}, [tuple()]}.


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Create the initial state without any tables.
-spec create(pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> db_state().
create(Users, Values, MaxTables, MaxInitialColumns) ->
  #state{
    users = Users,
    values = Values,
    max_tables = MaxTables,
    max_initial_columns = MaxInitialColumns
  }.

%% @doc Check if in the current state we have an active table.
-spec has_at_least_one_table(db_state()) -> boolean().
has_at_least_one_table(#state{tables=Tables}) ->
  dict:size(Tables) > 0.

%% @doc The command list usable for `frequency/1' generator.
-spec command_list(pos_integer(), node(), db_state()) -> [{integer(), proper_gen:generator()}].
command_list(Factor, CloakNode, State) ->
  lists:flatten(
        [
          [{1 * Factor, add_table(CloakNode, State)}]
        ] ++ [
          [
            {1 * Factor, remove_table(CloakNode, State)},
            {1 * Factor, clear_table(CloakNode, State)},
            {1 * Factor, add_column_to_table(CloakNode, State)},
            {20 * Factor, add_data(CloakNode, State)}
          ]
        ||
          has_at_least_one_table(State)
        ]
      ).

%% @doc Check if the command list has at least one add data (which implies we have at least one table).
-spec command_list_has_at_least_one_add_data([any()]) -> boolean().
command_list_has_at_least_one_add_data(Commands) ->
  lists:any(
        fun
          ({set, {var, _}, {call, rpc, call, [_Node, ?MODULE, callback_add_data, _]}}) ->
            true;
          (_) ->
            false
        end,
        Commands
      ).

%% @doc Return the names of all active tables.
-spec tables(db_state()) -> [string()].
tables(#state{tables=Tables}) ->
  dict:fetch_keys(Tables).

%% @doc Fold over all tables.
-spec fold_tables(fun((string(), db_rows(), any()) -> any()), any(), db_state()) -> any().
fold_tables(Folder, InitialAcc, #state{tables=Tables}) ->
  dict:fold(
        fun(TableName, TableSpec, Acc) ->
          Folder(TableName, extract_row_list(TableSpec), Acc)
        end,
        InitialAcc,
        Tables
      ).

%% @doc Return the rows for a specific table.
-spec rows_of_table(string(), db_state()) -> db_rows().
rows_of_table(TableName, #state{tables=Tables}) ->
  extract_row_list(dict:fetch(TableName, Tables)).


%% -------------------------------------------------------------------
%% The callbacks for PropEr tests
%% -------------------------------------------------------------------

%% @doc next_state processing.  This function will return the state unchanged if the call is unrelated
%%      to insertions.
-spec next_state(db_state(), any(), any()) -> db_state().
next_state(State, Res, {call, rpc, call, [_CloakNode, ?MODULE, CallbackFunction, Args]}) ->
  Function = function_name(next_state, CallbackFunction),
  apply(?MODULE, Function, [State, Res|Args]);
next_state(State, _Res, _Call) ->
  State.

%% @doc Like next-state this is provided to hook into the property testing facilities.
-spec precondition(db_state(), any()) -> boolean() | undefined.
precondition(State, {call, rpc, call, [_CloakNode, ?MODULE, CallbackFunction, Args]}) ->
  Function = function_name(precondition, CallbackFunction),
  apply(?MODULE, Function, [State|Args]);
precondition(_State, _Call) ->
  undefined.

%% @doc Like next-state this is provided to hook into the property testing facilities.
-spec postcondition(db_state(), any(), any()) -> boolean() | undefined.
postcondition(State, {call, rpc, call, [_CloakNode, ?MODULE, CallbackFunction, Args]}, Result) ->
  Function = function_name(postcondition, CallbackFunction),
  apply(?MODULE, Function, [State, Result|Args]);
postcondition(_State, _Call, _Result) ->
  undefined.


%% -------------------------------------------------------------------
%% Adding a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to create a new table.
-spec add_table(node(), db_state()) -> proper_gen:generator().
add_table(CloakNode, State) ->
  ?LET(Columns, integer(1, State#state.max_initial_columns),
      begin
        {NewTableName, MigrationNum} = table_name_and_initial_migration(),
        NewColumns = [column_name(I) || I <- lists:seq(1, Columns)],
        {call, rpc, call, [CloakNode, ?MODULE, callback_add_table,
            [return(NewTableName), return(MigrationNum), return(NewColumns)]]}
      end).

-spec column_name(non_neg_integer()) -> string().
column_name(I) ->
  "column_" ++ integer_to_list(I).

-spec table_name_and_initial_migration() -> {binary(), non_neg_integer()}.
table_name_and_initial_migration() ->
  I = cloak_util:timestamp_to_int(erlang:now()),  %% This is always unique
  TableName = iolist_to_binary(["table", integer_to_list(I)]),
  {TableName, 0}.

%% @hidden
-spec callback_add_table(binary(), non_neg_integer(), [string()]) -> ok | {error, any()}.
callback_add_table(TableName, MigrationNum, Columns) ->
  Migration = {
    iolist_to_binary(TableName),
    MigrationNum,
    {create, [{iolist_to_binary(ColumnName), integer, []} || ColumnName <- Columns]}
  },
  user_table_migration:migrate(?ANALYST_ID, Migration).

%% @hidden
-spec next_state_add_table(db_state(), any(), binary(), non_neg_integer(), [string()]) -> db_state().
next_state_add_table(State, _Result, NewTableName, MigrationNum, NewColumns) ->
  error = dict:find(NewTableName, State#state.tables),
  TableSpec = #table_spec{
    columns = NewColumns,
    column_num = length(NewColumns),
    migration = MigrationNum
  },
  State#state{tables=dict:store(NewTableName, TableSpec, State#state.tables)}.

%% @hidden
-spec precondition_add_table(db_state(), binary(), non_neg_integer(), [string()]) -> boolean().
precondition_add_table(#state{max_tables=MaxTables, tables=Tables}, _NewTableName, _MigrationNum,
    _NewColumns) ->
  dict:size(Tables) < MaxTables.

%% @hidden
-spec postcondition_add_table(db_state(), ok | {error, any()}, binary(), non_neg_integer(), [string()]) ->
    boolean().
postcondition_add_table(_State, Result, _NewTableName, _MigrationNum, _NewColumns) ->
  Result == ok.


%% -------------------------------------------------------------------
%% Removing a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to remove a user table.
-spec remove_table(node(), db_state()) -> proper_gen:generator().
remove_table(CloakNode, State) ->
  Tables = dict:to_list(State#state.tables),
  ?LET({ToRemove, TableSpec}, oneof([return(Table) || Table <- Tables]),
      {call, rpc, call, [CloakNode, ?MODULE, callback_remove_table,
          [return(ToRemove), return(TableSpec#table_spec.migration + 1)]]}).

%% @hidden
-spec callback_remove_table(binary(), non_neg_integer()) -> ok | {error, any()}.
callback_remove_table(TableName, MigrationNum) ->
  Migration = {
    iolist_to_binary(TableName),
    MigrationNum,
    drop
  },
  user_table_migration:migrate(?ANALYST_ID, Migration).

%% @hidden
-spec next_state_remove_table(db_state(), any(), binary(), non_neg_integer()) -> db_state().
next_state_remove_table(State, _Result, TableName, _MigrationNum) ->
  {ok, _OldTableSpec} = dict:find(TableName, State#state.tables),
  State#state{tables=dict:erase(TableName, State#state.tables)}.

%% @hidden
-spec precondition_remove_table(db_state(), binary(), non_neg_integer()) -> boolean().
precondition_remove_table(#state{tables=Tables}, TableName, _MigrationNum) ->
  dict:is_key(TableName, Tables).

%% @hidden
-spec postcondition_remove_table(db_state(), ok | {error, any()}, binary(), non_neg_integer()) -> boolean().
postcondition_remove_table(_State, Result, _TableName, _MigrationNum) ->
  Result == ok.


%% -------------------------------------------------------------------
%% Clear a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to clear a user table.
-spec clear_table(node(), db_state()) -> proper_gen:generator().
clear_table(CloakNode, State) ->
  Tables = dict:to_list(State#state.tables),
  ?LET({ToClear, _}, oneof([return(Table) || Table <- Tables]),
      {call, rpc, call, [CloakNode, ?MODULE, callback_clear_table, [return(ToClear)]]}).

%% @hidden
-spec callback_clear_table(binary()) -> ok | {error, any()}.
callback_clear_table(TableName) ->
  user_tables:clear_table(?ANALYST_ID, TableName).

%% @hidden
-spec next_state_clear_table(db_state(), any(), binary()) -> db_state().
next_state_clear_table(State, _Result, TableName) ->
  {ok, OldTableSpec} = dict:find(TableName, State#state.tables),
  NewTableSpec = OldTableSpec#table_spec{rows=dict:new()},
  State#state{tables=dict:store(TableName, NewTableSpec, State#state.tables)}.

%% @hidden
-spec precondition_clear_table(db_state(), binary()) -> boolean().
precondition_clear_table(#state{tables=Tables}, TableName) ->
  dict:is_key(TableName, Tables).

%% @hidden
-spec postcondition_clear_table(db_state(), ok | {error, any()}, binary()) -> boolean().
postcondition_clear_table(_State, Result, _TableName) ->
  Result == ok.


%% -------------------------------------------------------------------
%% Add a new column to a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to add a new column to a user table.
-spec add_column_to_table(node(), db_state()) -> proper_gen:generator().
add_column_to_table(CloakNode, State) ->
  Tables = dict:to_list(State#state.tables),
  ?LET({TableName, TableSpec}, oneof([return(Table) || Table <- Tables]),
      {call, rpc, call, [CloakNode, ?MODULE, callback_add_column_to_table, [
            return(TableName),
            return(TableSpec#table_spec.column_num+1),
            return(TableSpec#table_spec.migration+1)
          ]]}).

%% @hidden
-spec callback_add_column_to_table(binary(), non_neg_integer(), non_neg_integer()) -> ok | {error, any()}.
callback_add_column_to_table(TableName, NewColumn, MigrationNum) ->
  Migration = {
    iolist_to_binary(TableName),
    MigrationNum,
    {alter, [{add, {iolist_to_binary(column_name(NewColumn)), integer, []}}]}
  },
  user_table_migration:migrate(?ANALYST_ID, Migration).

%% @hidden
-spec next_state_add_column_to_table(db_state(), any(), binary(), non_neg_integer(), non_neg_integer()) ->
    db_state().
next_state_add_column_to_table(State, _Result, TableName, NewColumn, MigrationNum) ->
  NewColumnName = column_name(NewColumn),
  OldTableSpec = #table_spec{columns=OldColumns} = dict:fetch(TableName, State#state.tables),
  TableSpecWithNewColumn = OldTableSpec#table_spec{
    column_num = NewColumn,
    columns = OldColumns ++ [NewColumnName],
    migration = MigrationNum
  },
  State#state{tables=dict:store(TableName, TableSpecWithNewColumn, State#state.tables)}.

%% @hidden
-spec precondition_add_column_to_table(db_state(), binary(), non_neg_integer(), non_neg_integer()) ->
    boolean().
precondition_add_column_to_table(#state{tables=Tables}, TableName, NewColumn, MigrationNum) ->
  case dict:find(TableName, Tables) of
    {ok, #table_spec{column_num=ColumnNum, migration=OldMigrationNum}} ->
      ColumnNum == NewColumn - 1 andalso OldMigrationNum == MigrationNum - 1;
    error ->
      false
  end.

%% @hidden
-spec postcondition_add_column_to_table(db_state(), ok | {error, any()}, binary, non_neg_integer(),
    non_neg_integer()) -> boolean().
postcondition_add_column_to_table(_State, Result, _TableName, _NewColumn, _MigrationNum) ->
  Result == ok.


%% -------------------------------------------------------------------
%% Add data to a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to add data to a user table.
-spec add_data(node(), db_state()) -> proper_gen:generator().
add_data(CloakNode, State) ->
  ?LET(Data, data_generator(State),
      return({call, rpc, call, [CloakNode, ?MODULE, callback_add_data, [
            % We need to pack this as a binary as PropEr dislikes state with improper lists and dictionaries
            % may contain improper lists...
            return(term_to_binary(State#state.tables)),
            return([{User, List} || {User, List} <- Data, List /= []])
          ]]})).

-spec data_generator(db_state()) -> proper_gen:generator().
data_generator(State) ->
  common_generators:subset_generic(
      fun(I) ->
        UserName = "user" ++ integer_to_list(I),
        user_data_generator(UserName, State)
      end,
      lists:seq(1, State#state.users),
      ?USER_FREQ_YES,
      ?USER_FREQ_NO).

-spec user_data_generator(string(), db_state()) -> proper_gen:generator().
user_data_generator(UserName, State) ->
  ?LET(UserData,
      common_generators:subset_generic(
            fun({TableName, TableSpec}) -> return(generate_table_data(State, TableName, TableSpec)) end,
            dict:to_list(State#state.tables),
            ?TABLE_FREQ_YES,
            ?TABLE_FREQ_NO
          ),
      return({iolist_to_binary(UserName), UserData})).

-spec generate_table_data(db_state(), binary(), #table_spec{}) -> table_data().
generate_table_data(State, TableName, TableSpec) ->
  {iolist_to_binary(TableName), [
    {columns, [iolist_to_binary(Column) || Column <- TableSpec#table_spec.columns]},
    {data, [[random:uniform(State#state.values) || _ <- TableSpec#table_spec.columns]]}
  ]}.

%% @hidden
-spec callback_add_data(binary(), table_data()) -> {ok, db_contents()} | {error, any()}.
callback_add_data(Tables, Data) ->
  [({_, []} = insert_validator:validate(?ANALYST_ID, Rows)) || {_, Rows} <- Data],
  case inserter:add_users_data(?ANALYST_ID, Data) of
    ok ->
      {ok, read_all_user_tables(binary_to_term(Tables))};
    Error ->
      Error
  end.

%% We include in the reply of the insert function a complete list of data from the database.
%% This is done to check if the data really arrived in the tables.  This way we can check if the raw
%% data arrived at the tables.
-spec read_all_user_tables(dict:dict(string(), #table_spec{})) -> db_contents().
read_all_user_tables(Tables) ->
  % First get all tables.
  AllTables = [TableName || {?ANALYST_ID, TableName} <- user_tables:tables_that_are_not_dropped()],
  % Now get all data for each table and create the corresponding dictionary.
  lists:foldl(
        fun(TableName, TaDAcc) ->
          case dict:find(TableName, Tables) of
            {ok, TableSpec} ->
              SanitizedTableName = analyst_tables:sanitized_full_name(?ANALYST_ID, user, TableName),
              SQL = [
                "SELECT t.ac_user_id", column_names("t", TableSpec), " ",
                "FROM ("
                "  SELECT ac_user_id, max(ac_created_at) AS last FROM ", SanitizedTableName,
                "  GROUP BY ac_user_id",
                ") AS x ",
                "INNER JOIN ", SanitizedTableName, " AS t "
                "ON t.ac_user_id = x.ac_user_id AND t.ac_created_at = x.last;"
              ],
              ReadTable = fun(Connection) ->
                sql_conn:simple_query(SQL, timer:minutes(5), Connection)
              end,
              [{TableName, cloak_db:call(db_model, ReadTable)} | TaDAcc];
            error ->
              TaDAcc
          end
        end,
        [],
        AllTables
      ).

-spec column_names(string(), #table_spec{}) -> iolist().
column_names(Prefix, #table_spec{columns=Columns}) ->
  [[", ", Prefix, ".", ColumnName] || ColumnName <- Columns].

%% @hidden
-spec next_state_add_data(db_state, any(), binary(), table_data()) -> db_state().
next_state_add_data(#state{tables=OldTables}=State, _Result, _OldTables, Data) ->
  TablesWithNewData = lists:foldl(
        fun({User, UserRows}, TablesAcc1) ->
          lists:foldl(
                fun({TableName, [{columns, ColumnNames}, {data, [ColumnDatas]}]}, TablesAcc2) ->
                  TableSpec1 = dict:fetch(TableName, TablesAcc2),
                  Rows1 = TableSpec1#table_spec.rows,
                  Columns = lists:zip(ColumnNames, ColumnDatas),
                  Rows2 = dict:store(User, Columns, Rows1),
                  TableSpec2 = TableSpec1#table_spec{rows=Rows2},
                  dict:store(TableName, TableSpec2, TablesAcc2)
                end,
                TablesAcc1,
                UserRows
              )
        end,
        OldTables,
        Data
      ),
  State#state{tables=TablesWithNewData}.

%% @hidden
-spec precondition_add_data(db_state(), binary(), table_data()) -> boolean().
precondition_add_data(#state{tables=Tables}, _Tables, Data) ->
  lists:all(
        fun({_User, UserRows}) ->
          lists:all(
                fun({TableName, [{columns, Columns}|_]}) ->
                  case dict:find(TableName, Tables) of
                    {ok, TableSpec} -> length(Columns) == TableSpec#table_spec.column_num;
                    error -> false
                  end
                end,
                UserRows
              )
        end,
        Data
      ).

%% @hidden
-spec postcondition_add_data(db_state(), {ok, db_contents()} | {error, any()}, binary(), table_data()) ->
    boolean().
postcondition_add_data(State, Result, Tables, Data) ->
  case Result of
    {ok, DataInTables} ->
      NewState = next_state_add_data(State, Result, Tables, Data),
      compare_data_in_tables_with_state(DataInTables, NewState#state.tables);
    _Otherwise ->
      false
  end.

-spec compare_data_in_tables_with_state(db_contents(), dict:dict(string(), #table_spec{})) -> boolean().
compare_data_in_tables_with_state(DataInTables, Tables) ->
  CheckTableData = fun({TableName, {{select, _}, Rows}}) ->
    case dict:find(TableName, Tables) of
      {ok, TableSpec} ->
        CheckRowData = fun(Data) ->
          [User|RawDataOfUser] = tuple_to_list(Data),
          DataOfUser = [Item || Item <- RawDataOfUser, Item /= null],
          case dict:find(User, TableSpec#table_spec.rows) of
            {ok, RawUserColumns} ->
              [Value || {_, Value} <- RawUserColumns] =:= DataOfUser;
            _Otherwise ->
              false
          end
        end,
        length(Rows) == dict:size(TableSpec#table_spec.rows) andalso
            lists:all(CheckRowData, Rows);
      error ->
        false
    end
  end,
  length(DataInTables) == dict:size(Tables) andalso
      lists:all(CheckTableData, DataInTables).



%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

-spec function_name(string(), atom()) -> atom().
function_name(NewPrefix, CallbackFunction) ->
  "callback_" ++ BaseFunctionName = atom_to_list(CallbackFunction),
  list_to_atom(atom_to_list(NewPrefix) ++ "_" ++ BaseFunctionName).

-spec extract_row_list(#table_spec{}) -> db_rows().
extract_row_list(#table_spec{rows=Rows}) ->
  dict:to_list(Rows).
