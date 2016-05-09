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

-include_lib("proper/include/proper.hrl").
-include("full_test.hrl").
-include("cloak.hrl").

%% API
-export([
  prepare_database/0,
  create/4,
  has_at_least_one_table/1,
  command_list/2,
  command_list_has_at_least_one_add_data/1,
  tables/1,
  fold_tables/3,
  rows_of_table/2
]).

%% Command generators
-export([
  add_table/1,
  remove_table/1,
  clear_table/1,
  add_data/1
]).

%% Callbacks for PropEr (these callbacks are designed to be combinable with callbacks of other modules).
-export([
  next_state/3,
  precondition/2,
  postcondition/3
]).

%% Callbacks for adding a table
-export([
  callback_add_table/2,
  next_state_add_table/4,
  precondition_add_table/3,
  postcondition_add_table/4
]).

%% Callbacks for removing a table
-export([
  callback_remove_table/1,
  next_state_remove_table/3,
  precondition_remove_table/2,
  postcondition_remove_table/3
]).

%% Callbacks for clearing a table
-export([
  callback_clear_table/1,
  next_state_clear_table/3,
  precondition_clear_table/2,
  postcondition_clear_table/3
]).

%% Callbacks for adding data to a table
-export([
  callback_add_data/1,
  next_state_add_data/3,
  precondition_add_data/2,
  postcondition_add_data/3
]).


%% -------------------------------------------------------------------
%% The representation of the state of the database.
%% -------------------------------------------------------------------

-record(table_spec, {
  columns :: [string()], %% The columns represented by name.
  column_num :: non_neg_integer(), %% The number of columns.
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

%% @doc Prepares the empty database for testing.
-spec prepare_database() -> ok.
prepare_database() ->
  db_test:setup(),
  db_test:create_test_schema(),
  ok.

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
-spec command_list(pos_integer(), db_state()) -> [{integer(), proper_gen:generator()}].
command_list(Factor, State) ->
  lists:flatten(
    [
      [{1 * Factor, add_table(State)}]
    ] ++ [
      [
        {1 * Factor, remove_table(State)},
        {1 * Factor, clear_table(State)},
        {20 * Factor, add_data(State)}
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
      ({set, {var, _}, {call, ?MODULE, callback_add_data, _}}) ->
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
next_state(State, Res, {call, ?MODULE, CallbackFunction, Args}) ->
  Function = function_name(next_state, CallbackFunction),
  apply(?MODULE, Function, [State, Res|Args]);
next_state(State, _Res, _Call) ->
  State.

%% @doc Like next-state this is provided to hook into the property testing facilities.
-spec precondition(db_state(), any()) -> boolean() | undefined.
precondition(State, {call, ?MODULE, CallbackFunction, Args}) ->
  Function = function_name(precondition, CallbackFunction),
  apply(?MODULE, Function, [State|Args]);
precondition(_State, _Call) ->
  undefined.

%% @doc Like next-state this is provided to hook into the property testing facilities.
-spec postcondition(db_state(), any(), any()) -> boolean() | undefined.
postcondition(State, {call, ?MODULE, CallbackFunction, Args}, Result) ->
  Function = function_name(postcondition, CallbackFunction),
  apply(?MODULE, Function, [State, Result|Args]);
postcondition(_State, _Call, _Result) ->
  undefined.


%% -------------------------------------------------------------------
%% Adding a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to create a new table.
-spec add_table(db_state()) -> proper_gen:generator().
add_table(State) ->
  ?LET(Columns, integer(1, State#state.max_initial_columns),
    begin
      NewTableName = unique_table_name(),
      NewColumns = [column_name(I) || I <- lists:seq(1, Columns)],
      {call, ?MODULE, callback_add_table, [return(NewTableName), return(NewColumns)]}
    end
  ).

-spec column_name(non_neg_integer()) -> string().
column_name(I) ->
  "column_" ++ integer_to_list(I).

-spec unique_table_name() -> binary().
unique_table_name() ->
  I = erlang:unique_integer([positive, monotonic]),
  iolist_to_binary(["table", integer_to_list(I)]).

%% @hidden
-spec callback_add_table(binary(), [string()]) -> ok | {error, any()}.
callback_add_table(TableName, Columns) ->
  Data = {
    iolist_to_binary(TableName),
    {create, [{iolist_to_binary(ColumnName), integer, []} || ColumnName <- Columns]}
  },
  create_table(Data).

%% @hidden
-spec next_state_add_table(db_state(), any(), binary(), [string()]) -> db_state().
next_state_add_table(State, _Result, NewTableName, NewColumns) ->
  error = dict:find(NewTableName, State#state.tables),
  TableSpec = #table_spec{
    columns = NewColumns,
    column_num = length(NewColumns)
  },
  State#state{tables=dict:store(NewTableName, TableSpec, State#state.tables)}.

%% @hidden
-spec precondition_add_table(db_state(), binary(), [string()]) -> boolean().
precondition_add_table(#state{max_tables=MaxTables, tables=Tables}, _NewTableName, _NewColumns) ->
  dict:size(Tables) < MaxTables.

%% @hidden
-spec postcondition_add_table(db_state(), ok | {error, any()}, binary(), [string()]) ->
    boolean().
postcondition_add_table(_State, Result, _NewTableName, _NewColumns) ->
  Result == ok.

create_table({Table, {create, Columns}}) ->
  ColumnDefs = [[Column, " INTEGER"] || {Column, integer, []} <- Columns],
  case db_test:create_table(Table, iolist_to_binary(cloak_util:join(ColumnDefs, ","))) of
    {ok, _} -> ok;
    {error, Reason} -> {error, Reason}
  end.


%% -------------------------------------------------------------------
%% Removing a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to remove a user table.
-spec remove_table(db_state()) -> proper_gen:generator().
remove_table(State) ->
  Tables = dict:to_list(State#state.tables),
  ?LET({ToRemove, _TableSpec}, oneof([return(Table) || Table <- Tables]),
    {call, ?MODULE, callback_remove_table, [return(ToRemove)]}).

%% @hidden
-spec callback_remove_table(binary()) -> ok | {error, any()}.
callback_remove_table(TableName) ->
  case db_test:drop_table(TableName) of
    {ok, _} -> ok;
    {error, Reason} -> {error, Reason}
  end.

%% @hidden
-spec next_state_remove_table(db_state(), any(), binary()) -> db_state().
next_state_remove_table(State, _Result, TableName) ->
  {ok, _OldTableSpec} = dict:find(TableName, State#state.tables),
  State#state{tables=dict:erase(TableName, State#state.tables)}.

%% @hidden
-spec precondition_remove_table(db_state(), binary()) -> boolean().
precondition_remove_table(#state{tables=Tables}, TableName) ->
  dict:is_key(TableName, Tables).

%% @hidden
-spec postcondition_remove_table(db_state(), ok | {error, any()}, binary()) -> boolean().
postcondition_remove_table(_State, Result, _TableName) ->
  Result == ok.


%% -------------------------------------------------------------------
%% Clear a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to clear a user table.
-spec clear_table(db_state()) -> proper_gen:generator().
clear_table(State) ->
  Tables = dict:to_list(State#state.tables),
  ?LET({ToClear, _}, oneof([return(Table) || Table <- Tables]),
    {call, ?MODULE, callback_clear_table, [return(ToClear)]}).

%% @hidden
-spec callback_clear_table(binary()) -> ok | {error, any()}.
callback_clear_table(TableName) ->
  case db_test:clear_table(TableName) of
    {ok, _} -> ok;
    {error, Reason} -> {error, Reason}
  end.

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
%% Add data to a table
%% -------------------------------------------------------------------

%% @doc The PropEr generator to add data to a user table.
-spec add_data(db_state()) -> proper_gen:generator().
add_data(State) ->
  ?LET(Data, data_generator(State),
    return({call, ?MODULE, callback_add_data, [
      % We need to pack this as a binary as PropEr dislikes state with improper lists and dictionaries
      % may contain improper lists...
      return([{User, List} || {User, List} <- Data, List /= []])
    ]})
  ).

-spec data_generator(db_state()) -> proper_gen:generator().
data_generator(State) ->
  common_generators:subset_generic(
    fun(I) ->
      UserName = "user" ++ integer_to_list(I),
      user_data_generator(UserName, State)
    end,
    lists:seq(1, State#state.users),
    ?USER_FREQ_YES,
    ?USER_FREQ_NO
  ).

-spec user_data_generator(string(), db_state()) -> proper_gen:generator().
user_data_generator(UserName, State) ->
  ?LET(UserData,
    common_generators:subset_generic(
      fun({TableName, TableSpec}) -> return(generate_table_data(State, TableName, TableSpec)) end,
      dict:to_list(State#state.tables),
      ?TABLE_FREQ_YES,
      ?TABLE_FREQ_NO
    ),
    return({iolist_to_binary(UserName), UserData})
  ).

-spec generate_table_data(db_state(), binary(), #table_spec{}) -> table_data().
generate_table_data(State, TableName, TableSpec) ->
  {iolist_to_binary(TableName), [
    {columns, [iolist_to_binary(Column) || Column <- TableSpec#table_spec.columns]},
    {data, [[random:uniform(State#state.values) || _ <- TableSpec#table_spec.columns]]}
  ]}.

%% @hidden
-spec callback_add_data(table_data()) -> ok | any().
callback_add_data(Data) ->
  db_test:add_users_data(Data).

%% @hidden
-spec next_state_add_data(db_state, any(), table_data()) -> db_state().
next_state_add_data(#state{tables=OldTables}=State, _Result, Data) ->
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
-spec precondition_add_data(db_state(), table_data()) -> boolean().
precondition_add_data(#state{tables=Tables}, Data) ->
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
-spec postcondition_add_data(db_state(), {ok, db_contents()} | {error, any()}, table_data()) ->
  boolean().
postcondition_add_data(_State, Result, _Data) ->
  Result =:= ok.


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
