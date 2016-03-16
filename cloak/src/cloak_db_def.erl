%% @doc Helper for retrieving table definitions from the cloak database. This is a
%%      server that caches definitions inside a private ets table.
-module(cloak_db_def).
-behaviour(gen_server).

%% API
-export([
  start_link/0,
  table_def/3,
  table_def/2,
  table_columns/1,
  table_columns/2,
  invalidate_cached_def/2
]).

%% Callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("cloak.hrl").
-include("cloak_db.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Starts the server
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

%% @doc Retrieves a table definition. The result is a dictionary, with keys
%%      being names, and values being `#db_column{}' records.
-spec table_def(schema_name(), table_name(), sql_conn:connection() | undefined) -> dict:dict() | {error, term()}.
table_def(Schema, Table, Connection) ->
  Key = {Schema, Table},
  case get_def(Key) of
    undefined -> gen_server:call(?MODULE, {cache_def, Key, Connection});
    TableDef -> TableDef
  end.

%% @doc Retrieves a table definition. The result is a dictionary, with keys
%%      being names, and values being `#db_column{}' records.
-spec table_def(schema_name(), table_name()) -> dict:dict() | {error, term()}.
table_def(Schema, Table) ->
  table_def(Schema, Table, undefined).

%% @doc Retrieves column names of a fully qualified db table. Ordering is not guaranteed.
-spec table_columns(string() | binary()) -> [column_name()].
table_columns(FullyQualifiedTableName) ->
  {Schema, Table} = sql_util:split_table_name(FullyQualifiedTableName),
  table_columns(Schema, Table).

%% @doc Retrieves column names of a db table. Ordering is not guaranteed.
-spec table_columns(schema_name(), table_name()) -> [column_name()].
table_columns(Schema, Table) -> dict:fetch_keys(table_def(Schema, Table)).

%% @doc Removes a definition of the given table from the cache. This
%%      operation is performed on the current node. It is the responsibility
%%      of the client to call this function on each node after the table is migrated.
-spec invalidate_cached_def(schema_name(), table_name()) -> ok.
invalidate_cached_def(Schema, Table) ->
  ok = gen_server:call(
        ?MODULE, {invalidate_cached_def, {Schema, Table}},
        timer:seconds(30)
      ).


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------

%% @hidden
init(_) ->
  ets:new(?MODULE, [set, protected, named_table, {read_concurrency, true}]),
  {ok, undefined}.

%% @hidden
handle_call({cache_def, Key, Connection}, _, State) ->
  TableDef = case get_def(Key) of
    undefined -> cache_def(Key, Connection);
    Cached -> Cached
  end,
  {reply, TableDef, State};
handle_call({invalidate_cached_def, Key}, _From, State) ->
  ?INFO("Invalidating table cache definition. Presumably a table migration has taken place"),
  ets:delete(?MODULE, Key),
  {reply, ok, State};
handle_call(Message, From, _State) ->
  throw({unexpected_call, Message, from, From, to, ?MODULE}).

%% @hidden
-spec handle_cast(any(), any()) -> no_return().
handle_cast(Message, _State) ->
  throw({unexpected_cast, Message, to, ?MODULE}).

%% @hidden
handle_info(_, State) -> {noreply, State}.

%% @hidden
terminate(_, _) -> ok.

%% @hidden
code_change(_, State, _) -> {ok, State}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

get_def(Key) ->
  case ets:lookup(?MODULE, Key) of
    [{Key, Columns}] -> Columns;
    _ -> undefined
  end.

cache_def(Key, Connection) ->
  {{select, _}, RawTableDef} = load_def(Key, Connection),
  case RawTableDef of
    [] -> {error, {non_existing_table, Key}};
    _ ->
      TableDef = map_columns(RawTableDef),
      ets:insert(?MODULE, {Key, TableDef}),
      TableDef
  end.

load_def({Schema, Table}, undefined) ->
  LoadDef =
    fun(Connection) ->
      load_def({Schema, Table}, Connection)
    end,
  cloak_db:call(load_def, LoadDef);
load_def({Schema, Table}, Connection) ->
  sql_conn:extended_query(
        [
          "select column_name, is_nullable, udt_name "
          "from information_schema.columns "
          "where table_schema=$1 and table_name=$2"
        ],
        [Schema, Table],
        Connection
      ).

map_columns(RawTableDef) ->
  lists:foldl(
        fun({Name, IsNullable, Type}, Acc) ->
          dict:store(Name, #db_column{is_nullable=yes_no_to_bool(IsNullable), type=Type}, Acc)
        end,
        dict:new(),
        RawTableDef
      ).

yes_no_to_bool(<<"YES">>) -> true;
yes_no_to_bool(_) -> false.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

exec_sql(Sql) ->
  cloak_db:call(undefined, fun(Conn) -> sql_conn:simple_query(Sql, Conn) end).

table_def() ->
  lists:sort(dict:to_list(table_def(<<"public">>, <<"test1">>))).

cloak_db_def_test_() ->
  {
    foreach,
    fun() ->
      db_test:setup(),
      timer:sleep(200) % Makes sure server is started
    end,
    fun(_) -> db_test:teardown() end,
    [
      {"basic test", fun() ->
        Sql = [
          "CREATE TABLE test1 ("
          "  i integer NOT NULL,"
          "  bi bigint,",
          "  d double precision,"
          "  s varchar(255)"
          ")"
        ],
        exec_sql("drop table if exists test1"),
        ?assertEqual({{create, table}, []}, exec_sql(Sql)),
        Expected = [
          {<<"bi">>, #db_column{type = <<"int8">>, is_nullable=true}},
          {<<"d">>, #db_column{type = <<"float8">>, is_nullable=true}},
          {<<"i">>, #db_column{type = <<"int4">>, is_nullable=false}},
          {<<"s">>, #db_column{type = <<"varchar">>, is_nullable=true}}
        ],
        ?assertEqual(Expected, table_def()),
        ?assertEqual([<<"bi">>, <<"d">>, <<"i">>, <<"s">>], lists:sort(table_columns(<<"test1">>))),
        ?assertEqual(
              {error, {non_existing_table, {<<"public">>, <<"non_existing_table">>}}},
              table_def(<<"public">>, <<"non_existing_table">>)
            )
      end},
      {"cache invalidation", fun() ->
        exec_sql("drop table if exists test1"),
        ?assertEqual({{create, table}, []}, exec_sql("CREATE TABLE test1 (i integer)")),
        ?assertEqual([{<<"i">>, #db_column{type = <<"int4">>, is_nullable=true}}], table_def()),
        ?assertEqual({{alter,table},[]}, exec_sql("ALTER TABLE test1 ALTER COLUMN i TYPE bigint")),
        ?assertEqual([{<<"i">>, #db_column{type = <<"int4">>, is_nullable=true}}], table_def()),
        ?assertEqual(ok, invalidate_cached_def(<<"public">>, <<"another_table">>)),
        ?assertEqual([{<<"i">>, #db_column{type = <<"int4">>, is_nullable=true}}], table_def()),
        ?assertEqual(ok, invalidate_cached_def(<<"public">>, <<"test1">>)),
        ?assertEqual([{<<"i">>, #db_column{type = <<"int8">>, is_nullable=true}}], table_def())
      end}
    ]
  }.

-endif.
