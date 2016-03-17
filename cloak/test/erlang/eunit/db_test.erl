%% @doc Helper functions for running database dependent tests
%%
%%      You can invoke {@link setup/0} and {@link teardown/0} in your suite
%%      setup/teardown. All invocations to {@link cloak_db:call/2} will go
%%      through the test database.
%%
%%      This module exposes some other helper functions which you can use to
%%      setup the state of the database.
-module(db_test).

-include("src/cloak.hrl").

%% API functions
-export([
  conn_params/0,
  setup/0,
  teardown/0,
  create_analyst_schema/1,
  create_analyst_table/3,
  add_users_data/3
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Returns connection parameters for the test database
conn_params() ->
  [{host, "127.0.0.1"}, {database, "cloaktest1"}, {user, "postgres"}, {port, 5432}].

%% @doc Sets up a database dependent test by creating required processes and mocks.
setup() ->
  error_logger:tty(false),
  cloak_db_pool_sup:start_link(),
  cloak_db_def:start_link(),
  meck:new(cloak_db, [passthrough]),
  meck:expect(cloak_db, call, 2, fun(Type, Fun) -> cloak_db:call(Type, conn_params(), Fun) end),
  clean_db(),
  error_logger:tty(true).

%% @doc Tears down a database dependent test.
teardown() ->
  error_logger:tty(false),
  unlink(whereis(cloak_db_def)),
  exit(whereis(cloak_db_def), kill),
  unlink(whereis(cloak_db_pool_sup)),
  exit(whereis(cloak_db_pool_sup), kill),
  meck:unload(),
  error_logger:tty(true).

%% @doc Creates the analyst schema.
create_analyst_schema(Analyst) ->
  cloak_db:call(test, fun(Connection) ->
        {{create, schema}, _} = sql_conn:simple_query(
              ["CREATE SCHEMA ", sql_util:sanitize_db_object([analyst_tables:schema_for_analyst(Analyst)])],
              Connection
            )
    end).

%% @doc Creates the analyst table.
create_analyst_table(Analyst, TableName, Definition) ->
  cloak_db:call(test, fun(Connection) ->
        {{create, table}, _} = sql_conn:simple_query(
              [
                "CREATE TABLE ",
                  sql_util:sanitize_db_object([analyst_tables:schema_for_analyst(Analyst), ".", TableName]),
                "(", Definition, ")"
              ],
              Connection
            )
      end).

%% @doc Adds the data for the given user
add_users_data(Analyst, Data, Timeout) ->
  cloak_db:call(test, fun(Connection) ->
        [insert_rows(Analyst, UserId, TableName, TableData, Timeout, Connection) ||
          {UserId, UserData} <- Data,
          {TableName, TableData} <- UserData]
      end),
  ok.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

insert_rows(Analyst, UserId, TableName, TableData, Timeout, Connection) ->
  FullTableName = sql_util:sanitize_db_object([analyst_tables:schema_for_analyst(Analyst), ".", TableName]),
  Columns = lists:map(fun sql_util:sanitize_db_object/1,
      ["ac_user_id", "ac_created_at"] ++ proplists:get_value(columns, TableData)),
  Rows = lists:map(
        fun(Row) -> [UserId, cloak_util:timestamp_to_datetime(os:timestamp()) | Row] end,
        proplists:get_value(data, TableData)
      ),
  {PlaceHolders, _} = lists:foldr(
        fun(_Column, {PlaceholdersAcc, Index}) ->
          {[sql_util:param_placeholder(Index) | PlaceholdersAcc], Index - 1}
        end,
        {[], length(Columns)},
        Columns
      ),
  Results = sql_conn:batch_query(
        [
          "INSERT INTO ", FullTableName,
          "(", cloak_util:join(Columns, ","), ") ",
          "VALUES(", cloak_util:join(PlaceHolders, ","), ")"
        ],
        Rows,
        Timeout,
        Connection
      ),
  [{{insert, _, _}, _} = Result || Result <- Results],
  ok.

clean_db() ->
  cloak_db:call(undefined, fun(Connection) ->
          sql_conn:simple_query(["DROP SCHEMA IF EXISTS aircloak CASCADE;"], Connection),
          % Drop all analyst schemas
          {{select, _}, Schemas} = sql_conn:simple_query(
                "SELECT schema_name "
                "FROM information_schema.schemata "
                "where schema_name like 'analyst_%' ",
                Connection
              ),
          [{{drop, schema}, _} = sql_conn:simple_query(["DROP SCHEMA ", Schema, " CASCADE"], Connection)
            || {Schema} <- Schemas],
          ok
      end).
