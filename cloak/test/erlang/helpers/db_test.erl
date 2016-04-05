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
  setup/0,
  create_test_schema/0,
  create_table/2,
  add_users_data/1,
  drop_table/1,
  clear_table/1,
  full_table_name/1
]).


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Sets up a database dependent test by creating required processes and mocks.
setup() ->
  clean_db().

%% @doc Creates the test schema.
create_test_schema() ->
  'Elixir.Cloak.DataSource.PostgreSQL':execute(<<"CREATE SCHEMA cloak_test">>, []).

%% @doc Creates a test table.
create_table(TableName, Definition) ->
  Query = iolist_to_binary([<<"CREATE TABLE ">>, sanitized_table(TableName),
      <<" (row_id SERIAL, user_id VARCHAR(64), ">>, Definition, $)]),
  Result = 'Elixir.Cloak.DataSource.PostgreSQL':execute(Query, []),
  case Result of
    {ok, _} ->
      'Elixir.Cloak.DataSource':register_test_table(full_table_name(TableName), <<"user_id">>, <<"row_id">>),
      Result;
    {error, _} -> Result
  end.

%% @doc Adds the data for the given user
add_users_data(Data) ->
  [insert_rows(UserId, TableName, TableData) || {UserId, UserData} <- Data, {TableName, TableData} <- UserData],
  ok.

%% @doc Drops a test table
drop_table(TableName) ->
  case 'Elixir.Cloak.DataSource.PostgreSQL':execute(<<"DROP TABLE ", (sanitized_table(TableName))/binary>>, []) of
    {ok, Result} ->
      'Elixir.Cloak.DataSource':unregister_test_table(full_table_name(TableName)),
      {ok, Result};
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Clears a test table
clear_table(TableName) ->
  'Elixir.Cloak.DataSource.PostgreSQL':execute(<<"TRUNCATE TABLE ", (sanitized_table(TableName))/binary>>, []).

%% @doc Returns the full name for a test table
full_table_name(TableName) when is_list(TableName) ->
  full_table_name(list_to_binary(TableName));
full_table_name(TableName) when is_binary(TableName) ->
  <<"cloak_test.", TableName/binary>>.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

insert_rows(UserId, TableName, TableData) ->
  FullTableName = sanitized_table(TableName),
  Columns = lists:map(fun sql_util:sanitize_db_object/1,
      [<<"user_id">>] ++ proplists:get_value(columns, TableData)),
  Rows = lists:map(
        fun(Row) -> [UserId | Row] end,
        proplists:get_value(data, TableData)
      ),
  {PlaceHolders, _} = lists:foldr(
        fun(_Column, {PlaceholdersAcc, Index}) ->
          {[sql_util:param_placeholder(Index) | PlaceholdersAcc], Index - 1}
        end,
        {[], length(Columns)},
        Columns
      ),
  Query = iolist_to_binary([
    <<"INSERT INTO ">>, FullTableName,
    $(, cloak_util:join(Columns, $,), $), $\s,
    <<"VALUES(">>, cloak_util:join(PlaceHolders, $,), $)
  ]),
  [{ok, _} = 'Elixir.Cloak.DataSource.PostgreSQL':execute(Query, Row) || Row <- Rows],
  ok.

clean_db() ->
  'Elixir.Cloak.DataSource.PostgreSQL':execute(<<"DROP SCHEMA IF EXISTS cloak_test CASCADE">>, []),
  'Elixir.Cloak.DataSource':clear_test_tables().

sanitized_table(TableName) ->
    sql_util:sanitize_db_object(<<"cloak_test.", TableName/binary>>).
