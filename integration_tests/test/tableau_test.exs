defmodule IntegrationTest.TableauTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  setup context do
    {:ok, conn} = connect(context.user)
    {:ok, Map.put(context, :conn, conn)}
  end

  test "tableau table list query", context do
    query =
      'BEGIN;declare "SQL_CUR04645D10" cursor for select relname, nspname, relkind from pg_catalog.pg_class c, pg_catalog.pg_namespace n where relkind in (\'r\', \'v\') and nspname not in (\'pg_catalog\', \'information_schema\', \'pg_toast\', \'pg_temp_1\') and n.oid = relnamespace order by nspname, relname;fetch 2048 in "SQL_CUR04645D10";'

    assert :odbc.sql_query(context.conn, query) == [
             {:updated, 0},
             {:updated, 0},
             {:selected, ['relname', 'nspname', 'relkind'], [{'users', 'public', 'r'}]}
           ]
  end

  test "tableau table info query", context do
    query =
      'select n.nspname, c.relname, a.attname, a.atttypid, t.typname, a.attnum, a.attlen, a.atttypmod, a.attnotnull, c.relhasrules, c.relkind, c.oid, pg_get_expr(d.adbin, d.adrelid), case t.typtype when \'d\' then t.typbasetype else 0 end, t.typtypmod, c.relhasoids from (((pg_catalog.pg_class c inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname = \'users\') inner join pg_catalog.pg_attribute a on (not a.attisdropped) and a.attnum > 0 and a.attrelid = c.oid) inner join pg_catalog.pg_type t on t.oid = a.atttypid) left outer join pg_attrdef d on a.atthasdef and d.adrelid = a.attrelid and d.adnum = a.attnum order by n.nspname, c.relname, attnum'

    assert {
             :selected,
             [
               'nspname',
               'relname',
               'attname',
               'atttypid',
               'typname',
               'attnum',
               'attlen',
               'atttypmod',
               'attnotnull',
               'relhasrules',
               'relkind',
               'oid',
               'pg_get_expr',
               'case',
               'typtypmod',
               'relhasoids'
             ],
             [
               {'public', 'users', 'user_id', 25, 'text', 1, -1, -1, false, false, 'r', _, 'null', 0, -1, false},
               {'public', 'users', 'name', 25, 'text', 2, -1, -1, false, false, 'r', _, 'null', 0, -1, false},
               {'public', 'users', 'height', 23, 'int4', 3, 4, -1, false, false, 'r', _, 'null', 0, -1, false}
             ]
           } = :odbc.sql_query(context.conn, query)
  end

  test "table info query 2", context do
    query =
      ~c/select n.nspname, c.relname, a.attname, a.atttypid, t.typname, a.attnum, a.attlen, a.atttypmod, a.attnotnull, c.relhasrules, c.relkind, c.oid, pg_get_expr(d.adbin, d.adrelid), case t.typtype when 'd' then t.typbasetype else 0 end, t.typtypmod, c.relhasoids from (((pg_catalog.pg_class c inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname like 'monthly_income_expenses') inner join pg_catalog.pg_attribute a on (not a.attisdropped) and a.attnum > 0 and a.attrelid = c.oid) inner join pg_catalog.pg_type t on t.oid = a.atttypid) left outer join pg_attrdef d on a.atthasdef and d.adrelid = a.attrelid and d.adnum = a.attnum order by n.nspname, c.relname, attnum/

    assert {
             :selected,
             [
               'nspname',
               'relname',
               'attname',
               'atttypid',
               'typname',
               'attnum',
               'attlen',
               'atttypmod',
               'attnotnull',
               'relhasrules',
               'relkind',
               'oid',
               'pg_get_expr',
               'case',
               'typtypmod',
               'relhasoids'
             ],
             []
           } = :odbc.sql_query(context.conn, query)
  end

  test "table schema query", context do
    query =
      ~c/SELECT nspname AS TABLE_SCHEM, NULL AS TABLE_CATALOG FROM pg_catalog.pg_namespace  WHERE nspname <> 'pg_toast' AND (nspname !~ '^pg_temp_'  OR nspname = (pg_catalog.current_schemas(true))[1]) AND (nspname !~ '^pg_toast_temp_'  OR nspname = replace((pg_catalog.current_schemas(true))[1], 'pg_temp_', 'pg_toast_temp_'))  ORDER BY TABLE_SCHEM/

    assert {
             :selected,
             ['table_schem', 'table_catalog'],
             [
               {'information_schema', 'null'},
               {'pg_catalog', 'null'},
               {'public', 'null'}
             ]
           } = :odbc.sql_query(context.conn, query)
  end

  test "tableau query for related fields", context do
    query =
      'BEGIN;declare "SQL_CUR06145EE8" cursor for select c.relname, i.indkey, i.indisunique, i.indisclustered, a.amname, c.relhasrules, n.nspname, c.oid, d.relhasoids, 0 from pg_catalog.pg_index i, pg_catalog.pg_class c, pg_catalog.pg_class d, pg_catalog.pg_am a, pg_catalog.pg_namespace n where d.relname = \'transactions\' and n.nspname = \'public\' and n.oid = d.relnamespace and d.oid = i.indrelid and i.indexrelid = c.oid and c.relam = a.oid order by i.indisprimary desc, i.indisunique, n.nspname, c.relname;fetch 2048 in "SQL_CUR06145EE8"'

    assert :odbc.sql_query(context.conn, query) == [
             {:updated, 0},
             {:updated, 0},
             {:selected,
              [
                'relname',
                'indkey',
                'indisunique',
                'indisclustered',
                'amname',
                'relhasrules',
                'nspname',
                'oid',
                'relhasoids',
                '?column?'
              ], []}
           ]
  end

  test "tableau query for indexed columns", context do
    query =
      'BEGIN;declare "SQL_CUR04AD8270" cursor for select ta.attname, ia.attnum, ic.relname, n.nspname, tc.relname from pg_catalog.pg_attribute ta, pg_catalog.pg_attribute ia, pg_catalog.pg_class tc, pg_catalog.pg_index i, pg_catalog.pg_namespace n, pg_catalog.pg_class ic where tc.relname = \'accounts\' AND n.nspname = \'public\' AND tc.oid = i.indrelid AND n.oid = tc.relnamespace AND i.indisprimary = \'t\' AND ia.attrelid = i.indexrelid AND ta.attrelid = i.indrelid AND ta.attnum = i.indkey[ia.attnum-1] AND (NOT ta.attisdropped) AND (NOT ia.attisdropped) AND ic.oid = i.indexrelid order by ia.attnum;fetch 2048 in "SQL_CUR04AD8270"'

    assert :odbc.sql_query(context.conn, query) == [
             {:updated, 0},
             {:updated, 0},
             {:selected, ['attname', 'attnum', 'relname', 'nspname', 'relname'], []}
           ]
  end

  test "arbitrary query through a cursor", context do
    query = 'BEGIN;declare "SQL_CUR04AD8270" cursor for show tables;fetch 2048 in "SQL_CUR04AD8270"'

    assert :odbc.sql_query(context.conn, query) == [
             {:updated, 0},
             {:updated, 0},
             {:selected, ['name'], [{'users'}]}
           ]
  end

  test "arbitrary query through a cursor with hold", context do
    query = 'BEGIN;declare "SQL_CUR04AD8270" cursor with hold for show tables;fetch 2048 in "SQL_CUR04AD8270"'

    assert :odbc.sql_query(context.conn, query) == [
             {:updated, 0},
             {:updated, 0},
             {:selected, ['name'], [{'users'}]}
           ]
  end

  test "multiline query through a cursor", context do
    query = 'BEGIN;declare "SQL_CUR04AD8270" cursor for show\ntables;fetch 2048 in "SQL_CUR04AD8270"'

    assert :odbc.sql_query(context.conn, query) == [
             {:updated, 0},
             {:updated, 0},
             {:selected, ['name'], [{'users'}]}
           ]
  end

  test "partial fetching through a cursor", context do
    query = 'BEGIN;declare "my_cursor" cursor for show columns from users;fetch 1 in "my_cursor"'

    assert :odbc.sql_query(context.conn, query) == [
             {:updated, 0},
             {:updated, 0},
             {:selected, ['name', 'type'], [{'user_id', 'text'}]}
           ]

    assert :odbc.sql_query(context.conn, 'fetch 2 in "my_cursor"') ==
             {:selected, ['name', 'type'], [{'name', 'text'}, {'height', 'integer'}]}

    assert :odbc.sql_query(context.conn, 'fetch 1 in "my_cursor"') == {:selected, ['name', 'type'], []}

    assert :odbc.sql_query(context.conn, 'close "my_cursor"') == {:updated, 0}
  end

  test "fetching from an unexisting cursor", context do
    assert {:error, error} = :odbc.sql_query(context.conn, 'fetch 2 in "unknown_cursor"')
    assert to_string(error) =~ ~r/^ERROR: cursor `unknown_cursor` does not exist.*/
  end

  test "invalid query through a cursor", context do
    query = 'BEGIN;declare "SQL_CUR04AD8270" cursor for foo bar;fetch 2048 in "SQL_CUR04AD8270"'
    assert {:error, error} = :odbc.sql_query(context.conn, query)
    assert to_string(error) =~ ~r/^ERROR: Expected `select or show`/
  end

  test "selecting a regclass value", context do
    query =
      ~c/SELECT c.oid::pg_catalog.regclass FROM pg_catalog.pg_class c, pg_catalog.pg_inherits i WHERE c.oid=i.inhparent AND i.inhrelid = '18119' ORDER BY inhseqno;/

    assert {:selected, ['oid'], []} = :odbc.sql_query(context.conn, query)
  end

  test(
    "deallocate statement",
    context,
    do: assert(:odbc.sql_query(context.conn, 'DEALLOCATE "foobar"') == {:updated, 0})
  )

  test "select current schema", context do
    assert(:odbc.sql_query(context.conn, 'select current_schema()') == {:selected, ['current_schema'], [{'public'}]})
  end

  test(
    "selecting into a temporary table",
    context,
    do:
      assert(
        {:error, %Postgrex.Error{postgres: %{message: "permission denied"}}} =
          postgrex_query(
            context.user,
            "-- statement does not return rows\nSELECT *\nINTO TEMPORARY TABLE \"#Tableau_5_1_Connect\"\nFROM (SELECT 1 AS COL) AS CHECKTEMP\nLIMIT 1"
          )
      )
  )

  test(
    "dropping a temp table",
    context,
    do:
      assert(
        {:error, %Postgrex.Error{postgres: %{message: "permission denied"}}} =
          postgrex_query(context.user, "DROP TABLE \"#Tableau_5_1_Connect\"")
      )
  )

  test(
    "creating a temp table",
    context,
    do:
      assert(
        {:error, %Postgrex.Error{postgres: %{message: "permission denied"}}} =
          postgrex_query(
            context.user,
            "CREATE LOCAL TEMPORARY TABLE \"#Tableau_5_2_Connect\" (\n\t\"COL\" INTEGER\n\t) ON COMMIT PRESERVE ROWS"
          )
      )
  )

  test(
    "show lc_collate",
    context,
    do:
      assert(
        {:ok, %Postgrex.Result{columns: ["lc_collate"], command: :select, rows: [[_collation]]}} =
          postgrex_query(context.user, "show \"lc_collate\"")
      )
  )

  describe "simple queries for testing connectivity" do
    test "integer", context do
      assert :odbc.sql_query(context.conn, ' \n seLect  \n  -1234\n') == {:selected, ['?column?'], [{-1234}]}
    end

    test "float", context do
      assert :odbc.sql_query(context.conn, 'SELECT  \n 34.457::float ') == {:selected, ['float8'], [{34.457}]}
    end

    test "bool", context do
      assert :odbc.sql_query(context.conn, 'seLect True;') == {:selected, ['bool'], [{true}]}
    end

    test "text", context do
      assert :odbc.sql_query(context.conn, 'seLect \'ab\'\'cd\'') == {:selected, ['?column?'], [{'ab\'cd'}]}
    end
  end

  defp connect(user, params \\ []) do
    params =
      Keyword.merge(
        [
          user: user.login,
          password: Manager.user_password(),
          database: Manager.data_source_name(),
          sslmode: "require"
        ],
        params
      )

    connection_string =
      %{
        "DSN" => "PostgreSQL",
        "Server" => "localhost",
        "Port" => Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
        "sslmode" => params[:sslmode],
        "Uid" => params[:user],
        "Pwd" => params[:password],
        "Database" => params[:database]
      }
      |> Enum.map(fn {name, value} -> "#{name}=#{value};" end)
      |> Enum.join()
      |> to_charlist()

    :odbc.connect(connection_string, [])
  end

  defp postgrex_connect(user) do
    Postgrex.start_link(
      hostname: "localhost",
      port: Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
      username: user.login,
      password: Manager.user_password(),
      database: Manager.data_source_name(),
      ssl: true
    )
  end

  defp postgrex_query(user, query, params \\ []) do
    # Postgrex is used if we want to test prepared statements (describe, bind, execute cycle)
    {:ok, conn} = postgrex_connect(user)
    Postgrex.query(conn, query, params)
  end
end
