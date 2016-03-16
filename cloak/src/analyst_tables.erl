%% @doc Helper functions for converting logical table name (as provided by
%%      analysts in web UI, or in HTTP requests) into full database name.
%%
%%      For example, a user table locations for an analyst with id 42 will
%%      have a full name analyst_42.user_locations
-module(analyst_tables).

%% API
-export([
  schema_for_analyst/1,
  table_name/2,
  partition_table_name/3,
  schema_and_partition_table_name/1,
  schema_and_table_name/3,
  schema_and_table_name_for_partition/4,
  schema_and_table_name_for_partition/1,
  sanitized_full_name/3,
  sanitized_full_partition_name/4,
  sanitized_full_partition_name/1,
  all_analysts/0,
  existing_user_tables/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Returns analyst specific schema name.
-spec schema_for_analyst(analyst()) -> binary().
schema_for_analyst(AnalystId) ->
  << <<"analyst_">>/binary, (integer_to_binary(AnalystId))/binary >>.

%% @doc Returns short table name (without schema) for the given table type and name.
-spec table_name(analyst_table_type(), table_name()) -> binary().
table_name(TableType, TableName) ->
  true = sql_util:valid_table_name(TableName),
  <<
    (table_prefix(TableType))/binary,
    TableName/binary
  >>.

%% @doc Returns the table name for a given partition (without schema).
-spec partition_table_name(analyst_table_type(), table_name(), partition_id()) -> binary().
partition_table_name(TableType, TableName, PartitionId) ->
  <<
    (cloak_util:binarify("ac_partition_" ++ integer_to_list(PartitionId) ++ "_"))/binary,
    (table_name(TableType, TableName))/binary
  >>.

%% @doc Returns a pair of the schema and the table name for a given partition.
-spec schema_and_partition_table_name(#partition_table{}) -> {binary(), binary()}.
schema_and_partition_table_name(#partition_table{analyst_id=AnalystId, table_type=TableType,
    table_name=TableName, partition_id=PartitionId}) ->
  {schema_for_analyst(AnalystId), partition_table_name(TableType, TableName, PartitionId)}.

%% @doc Returns full table name (including analyst schema) for the given analyst, and table type and name.
-spec schema_and_table_name(analyst(), analyst_table_type(), table_name()) -> binary().
schema_and_table_name(AnalystId, TableType, TableName) ->
  <<
    (schema_for_analyst(AnalystId))/binary,
    <<".">>/binary,
    (table_name(TableType, TableName))/binary
  >>.

%% @doc Returns full table name (including analyst schema) for the given analyst, and table type and name.
-spec schema_and_table_name_for_partition(analyst(), analyst_table_type(), table_name(), partition_id()) -> binary().
schema_and_table_name_for_partition(AnalystId, TableType, TableName, PartitionId) ->
  <<
    (schema_for_analyst(AnalystId))/binary,
    <<".">>/binary,
    (partition_table_name(TableType, TableName, PartitionId))/binary
  >>.

%% @doc Equivalent to {@link schema_and_table_name_for_partition/4} except that the partition table is given
%%      by a `#partition_table{}' record.
-spec schema_and_table_name_for_partition(#partition_table{}) -> binary().
schema_and_table_name_for_partition(#partition_table{analyst_id=AnalystId, table_type=TableType,
    table_name=TableName, partition_id=PartitionId}) ->
  schema_and_table_name_for_partition(AnalystId, TableType, TableName, PartitionId).

%% @doc Returns sanitized full table name (including analyst schema) for the given table type and name.
%%      The result is safe to use in SQL queries.
-spec sanitized_full_name(analyst(), analyst_table_type(), table_name()) -> binary().
sanitized_full_name(AnalystId, TableType, TableName) ->
  sql_util:sanitize_db_object(schema_and_table_name(AnalystId, TableType, TableName)).

-spec sanitized_full_partition_name(analyst(), analyst_table_type(), table_name(), partition_id()) -> binary().
sanitized_full_partition_name(AnalystId, TableType, TableName, PartitionId) ->
  sql_util:sanitize_db_object(schema_and_table_name_for_partition(AnalystId, TableType, TableName, PartitionId)).

-spec sanitized_full_partition_name(#partition_table{}) -> binary().
sanitized_full_partition_name(#partition_table{analyst_id=AnalystId, table_type=TableType,
    table_name=TableName, partition_id=PartitionId}) ->
  sanitized_full_partition_name(AnalystId, TableType, TableName, PartitionId).

%% @doc Retrieves all analysts currently existing in the local database.
-spec all_analysts() -> [analyst()].
all_analysts() ->
  {{select, _}, Schemas} = ?QUEUED_GENERIC(cloak_db:call(
        all_analysts,
        fun(Conn) ->
          sql_conn:simple_query(
                "SELECT schema_name "
                "FROM information_schema.schemata "
                "where schema_name like 'analyst_%' ",
                Conn
              )
        end
      )),
  [binary_to_integer(Analyst) || {<<"analyst_", Analyst/binary>>} <- Schemas].

%% @doc Retrieves all existing (not deleted) user tables for the given analyst.
-spec existing_user_tables(analyst()) -> [table_name()].
existing_user_tables(AnalystId) ->
  % Although list of all tables exists in the aircloak.analyst_tables, we keep deleted tables there so we know
  % their migration version. Thus, that table can't be used to get the list of not deleted tables so we query
  % table_schema instead.
  {{select, _}, Tables} = ?QUEUED_GENERIC(cloak_db:call(
        existing_user_tables,
        fun(Conn) ->
          sql_conn:extended_query(
                "SELECT regexp_replace(table_name, '^user_', '') "
                "FROM information_schema.tables "
                "WHERE table_schema=$1 AND table_name like 'user_%'",
                [schema_for_analyst(AnalystId)],
                Conn
              )
        end
      )),
  [Table || {Table} <- Tables].


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

table_prefix(user) -> <<"user_">>;
table_prefix(system) -> <<"">>.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

table_name_generation_test_() ->
  [
    ?_assertEqual(<<"\"analyst_1\".\"user_table_name\"">>, sanitized_full_name(1, user, <<"table_name">>)),
    ?_assertEqual(<<"\"analyst_3\".\"ac_partition_4_user_table_name\"">>, sanitized_full_partition_name(3, user, <<"table_name">>, 4))
  ].

-endif.
