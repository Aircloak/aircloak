%% @doc Helper functions for running database dependent tests
%%
%%      You can invoke {@link setup/0} and {@link teardown/0} in your suite
%%      setup/teardown. All invocations to {@link cloak_db:call/2} will go
%%      through the test database.
%%
%%      This module exposes some other helper functions which you can use to
%%      setup the state of the database.
-module(db_test).

-include("cloak.hrl").

%% API functions
-export([
  conn_params/0,
  setup/0,
  create_test_schema/0,
  create_table/2,
  add_users_data/2,
  drop_table/1,
  clear_table/1,
  full_table_name/1
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Returns connection parameters for the test database
conn_params() ->
  [{host, "127.0.0.1"}, {database, "cloaktest1"}, {user, "postgres"}, {port, 5432}].

%% @doc Sets up a database dependent test by creating required processes and mocks.
setup() ->
  clean_db().

%% @doc Creates the test schema.
create_test_schema() ->
  cloak_db:call(test, fun(Connection) ->
        {{create, schema}, _} = sql_conn:simple_query("CREATE SCHEMA cloak_test", Connection)
    end).

%% @doc Creates a test table.
create_table(TableName, Definition) ->
  cloak_db:call(test, fun(Connection) ->
        {{create, table}, _} = sql_conn:simple_query(
              ["CREATE TABLE ", sanitized_table(TableName), "(", Definition, ")"],
              Connection
            )
      end).

%% @doc Adds the data for the given user
add_users_data(Data, Timeout) ->
  cloak_db:call(test, fun(Connection) ->
        [insert_rows(UserId, TableName, TableData, Timeout, Connection) ||
          {UserId, UserData} <- Data,
          {TableName, TableData} <- UserData]
      end),
  ok.

%% @doc Drops a test table
drop_table(TableName) ->
  cloak_db:call(test, fun(Connection) ->
        sql_conn:simple_query(["DROP TABLE ", sanitized_table(TableName)], Connection)
      end).

%% @doc Clears a test table
clear_table(TableName) ->
  cloak_db:call(test, fun(Connection) ->
        sql_conn:simple_query(
              ["TRUNCATE TABLE ", sanitized_table(TableName)],
              Connection
            )
      end).

%% @doc Returns the full name for a test table
full_table_name(TableName) ->
  <<"cloak_test.", TableName/binary>>.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

insert_rows(UserId, TableName, TableData, Timeout, Connection) ->
  FullTableName = sanitized_table(TableName),
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
          sql_conn:simple_query(["DROP SCHEMA IF EXISTS cloak_test CASCADE;"], Connection),
          ok
      end).

sanitized_table(TableName) ->
    sql_util:sanitize_db_object(["cloak_test.", TableName]).
