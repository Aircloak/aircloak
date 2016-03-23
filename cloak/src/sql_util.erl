%% @doc Set of utilities for working with sql in the cloak.
%%      Examples include functions for validating the validity
%%      of names used for columns and tables, and escaping names
%%      to avoid nasty little bobby tables.
-module(sql_util).

%% API
-export([
  split_table_name/1,
  valid_table_name/1,
  sanitize_db_object/1,
  sql_name_regex/0,
  user_table_name_regex/0,
  param_placeholder/1
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc Splits the fully qualified table name into schema and table name
-spec split_table_name(string() | binary()) -> {schema_name(), table_name()}.
split_table_name(FullyQualifiedTableName) ->
  UnquotedFQTN = binary:replace(cloak_util:binarify(FullyQualifiedTableName), <<$">>, <<>>, [global]),
  case binary:split(UnquotedFQTN, <<$.>>) of
    [TableName] -> {<<"public">>, TableName};
    [Schema, TableName] -> {Schema, TableName}
  end.

%% @doc Checks if the table name is valid. Currently we only allow English alphabet letters.
-spec valid_table_name(table_name()) -> boolean().
valid_table_name(TableName) when is_binary(TableName) ->
  re:run(TableName, sql_name_regex()) =/= nomatch;
valid_table_name(_) ->
  false.

%% @doc Returns the compiled regular expression
%%      used to validate names used for tables and columns.
%%
%%      The names accepted mostly match what Postgres allows:
%%      http://www.postgresql.org/docs/current/static/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
%%      One notable exception is that we do not allows dollar signs
%%      as part of the name (which is not strictly speaking allowed
%%      in the SQL standard, but supported by Postgres).
-spec sql_name_regex() -> re:mp().
sql_name_regex() ->
  Pattern = "^[a-z_][a-z0-9_]*$",
  {ok, Regex} = re:compile(Pattern, [caseless]),
  Regex.

%% @doc Returns the compiled regular expression used to validate identifiers
%%      (tables and columns). This works mostly like {@link sql_name_regex/0} but
%%      it also verifies some special cases, such as `ac_' prefixes.
-spec user_table_name_regex() -> re:mp().
user_table_name_regex() ->
  Pattern = "^(?!ac_)[a-z_][a-z0-9_]*$",
  {ok, Regex} = re:compile(Pattern, [caseless]),
  Regex.

%% @doc Sanitizes the database object (table, column) name by surrounding each qualifier in double quotes
%%      and replacing all inner " with "". Returns a binary.
-spec sanitize_db_object(iodata()) -> binary().
sanitize_db_object(DbObject) ->
  SanitizedParts = [
      ("\"" ++ re:replace(Part, "\"", "\"\"", [global, {return, list}]) ++ "\"") ||
        Part <- re:split(DbObject, "\\.", [{return, list}])
    ],
  list_to_binary(string:join(SanitizedParts, ".")).

%% @doc Generates binary representation of a query parameter with the given index.
-spec param_placeholder(pos_integer()) -> binary().
param_placeholder(Index) ->
  << <<"$">>/binary, (integer_to_binary(Index))/binary >>.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

validate_table_name_test_() ->
  [
    {"Require name to be a binary", [
      ?_assertNot(valid_table_name("hello")),
      ?_assertNot(valid_table_name(hello))
    ]},
    {"A binary string with only characters a-z, numerals and underscores should be valid", [
      ?_assert(valid_table_name(<<"hello">>)),
      ?_assert(valid_table_name(<<"Az">>)),
      ?_assert(valid_table_name(<<"smart1">>)),
      ?_assert(valid_table_name(<<"smart_1">>))
    ]},
    {"The table name should start with a character or an underscore", [
      ?_assert(valid_table_name(<<"hello1">>)),
      ?_assert(valid_table_name(<<"Hello1">>)),
      ?_assert(valid_table_name(<<"_Hello1">>)),
      ?_assertNot(valid_table_name(<<"1hello">>))
    ]},
    {"We do not allow punctuations or special characters", [
      ?_assertNot(valid_table_name(<<"he.llo">>)),
      ?_assertNot(valid_table_name(<<" hello">>)),
      ?_assertNot(valid_table_name(<<"!hello">>)),
      ?_assertNot(valid_table_name(<<",hello">>)),
      ?_assertNot(valid_table_name(<<"hello?">>)),
      ?_assertNot(valid_table_name(<<"@aircloak">>)),
      ?_assertNot(valid_table_name(<<"aircloak.test">>))
    ]},
    {"We refuse to cooperate with the Germans", [
      ?_assertNot(valid_table_name(<<"Was glänzt ist für den Augenblick geboren">>)),
      ?_assertNot(valid_table_name(<<"glänzt">>)),
      ?_assertNot(valid_table_name(<<"für">>))
    ]}
  ].

user_table_name_test_() ->
  [
    ?_assertEqual({match, [{0, 3}]}, re:run(<<"foo">>, user_table_name_regex())),
    ?_assertEqual({match, [{0, 5}]}, re:run(<<"acfoo">>, user_table_name_regex())),
    ?_assertEqual(nomatch, re:run(<<"ac_foo">>, user_table_name_regex()))
  ].

sanitize_db_object_test_() ->
  [
    ?_assertEqual(<<"\"field\"">>, sanitize_db_object(<<"field">>)),
    ?_assertEqual(<<"\"schema\".\"table\"">>, sanitize_db_object(<<"schema.table">>)),
    ?_assertEqual(
      <<"\"Robert\"\"; drop table students; --\"">>,
      sanitize_db_object(<<"Robert\"; drop table students; --">>)
    )
  ].

-endif.
