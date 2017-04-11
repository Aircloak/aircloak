defmodule IntegrationTest.OdbcTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    {:ok, user: Manager.create_air_user()}
  end

  test "invalid connect params", context do
    assert {:error, msg} = connect(context.user, user: "invalid user")
    assert to_string(msg) =~ ~r/Authentication failed/

    assert {:error, msg} = connect(context.user, password: "invalid password")
    assert to_string(msg) =~ ~r/Authentication failed/

    assert {:error, msg} = connect(context.user, database: "invalid database")
    assert to_string(msg) =~ ~r/Authentication failed/
  end

  test "ssl mode is required", context do
    assert {:error, msg} = connect(context.user, sslmode: "disable")
    assert to_string(msg) =~ ~r/Connection refused/
  end

  test "connecting", context do
    assert {:ok, _} = connect(context.user, sslmode: "require")
    assert {:ok, _} = connect(context.user, sslmode: "prefer")
    assert {:ok, _} = connect(context.user, sslmode: "allow")
  end

  describe "connection tests" do
    setup context do
      {:ok, conn} = connect(context.user)
      {:ok, Map.put(context, :conn, conn)}
    end

    test "disconnecting", context, do:
      assert :ok = :odbc.disconnect(context.conn)

    test "show tables", context, do:
      assert :odbc.sql_query(context.conn, 'show tables') == {:selected, ['name'], [{'users'}]}

    test "show columns", context, do:
      assert :odbc.sql_query(context.conn, 'show columns from users') == {:selected, ['name', 'type'], [
        {'user_id', 'text'},
        {'name', 'text'},
        {'height', 'integer'}
      ]}

    test "select", context do
      assert {:selected, ['name', 'height'], rows} = :odbc.sql_query(context.conn, 'select name, height from users')
      assert Enum.uniq(rows) == [{'john', '180'}]
    end

    test "parameterized query with a tiny integer", context, do:
      assert param_select(context.conn, :sql_tinyint, 42) == '42'

    test "parameterized query with a small integer", context, do:
      assert param_select(context.conn, :sql_smallint, 42) == '42'

    test "parameterized query with an integer", context, do:
      # The reason that the result is a string is because the server returns `int8`, and it appears that
      # either the ODBC driver, or ODBC itself converts this into a string.
      assert param_select(context.conn, :sql_integer, 42) == '42'

    test "parameterized query with a float", context, do:
      assert param_select(context.conn, {:sql_float, 32}, 3.14, "real") == 3.14

    test "parameterized query with a double", context, do:
      assert param_select(context.conn, :sql_double, 3.14, "real") == 3.14

    test "parameterized query with a decimal", context, do:
      assert param_select(context.conn, {:sql_decimal, 10, 2}, 3.14, "real") == 3.14

    test "parameterized query with a real", context, do:
      assert param_select(context.conn, :sql_real, 3.14, "real") == 3.14

    test "parameterized query with a varchar", context, do:
      assert param_select(context.conn, {:sql_varchar, 6}, 'foobar') == 'foobar'

    test "parameterized query with a char", context, do:
      assert param_select(context.conn, {:sql_char, 6}, 'foobar') == 'foobar'

    test "tableau relkind query", context do
      query =
        'BEGIN;' ++
        'declare "SQL_CUR04645D10" cursor for ' ++
        'select relname, nspname, relkind from pg_catalog.pg_class c, pg_catalog.pg_namespace n ' ++
        'where relkind in (\'r\', \'v\') and ' ++
        'nspname not in (\'pg_catalog\', \'information_schema\', \'pg_toast\', \'pg_temp_1\') and ' ++
        'n.oid = relnamespace order by nspname, relname;' ++
        'fetch 2048 in "SQL_CUR04645D10";'

      assert :odbc.sql_query(context.conn, query) == [
        {:updated, 0},
        {:updated, 0},
        {:selected, ['relname', 'nspname', 'relkind'], [{'users', 'public', 'r'}]}
      ]
    end

    test "tableau table info query", context do
      query =
        'select n.nspname, c.relname, a.attname, a.atttypid, t.typname, a.attnum, a.attlen, a.atttypmod, ' ++
        'a.attnotnull, c.relhasrules, c.relkind, c.oid, pg_get_expr(d.adbin, d.adrelid), case t.typtype ' ++
        'when \'d\' then t.typbasetype else 0 end, t.typtypmod, c.relhasoids from (((pg_catalog.pg_class c ' ++
        'inner join pg_catalog.pg_namespace n on n.oid = c.relnamespace and c.relname like \'users\' ' ++
        'and n.nspname like \'public\') inner join pg_catalog.pg_attribute a on (not a.attisdropped) and ' ++
        'a.attnum > 0 and a.attrelid = c.oid) inner join pg_catalog.pg_type t on t.oid = a.atttypid) left ' ++
        'outer join pg_attrdef d on a.atthasdef and d.adrelid = a.attrelid and d.adnum = a.attnum order by ' ++
        'n.nspname, c.relname, attnum'

      assert {
        :selected,
        [
          'nspname', 'relname', 'attname', 'atttypid', 'typname', 'attnum', 'attlen', 'atttypmod', 'attnotnull',
          'relhasrules', 'relkind', 'oid', 'pg_get_expr', 'case', 'typtypmod', 'relhasoids'
        ],
        [
          {'public', 'users', 'user_id', '25', 'text', 1, -1, -1, false, false, 'r', oid, [], '0', -1, false},
          {'public', 'users', 'name', '25', 'text', 2, -1, -1, false, false, 'r', oid, [], '0', -1, false},
          {'public', 'users', 'height', '20', 'int8', 3, 8, -1, false, false, 'r', oid, [], '0', -1, false}
        ]
      } = :odbc.sql_query(context.conn, query)
    end

    test "closing a cursor", context, do:
      assert :odbc.sql_query(context.conn, 'close "some_cursor"') == {:updated, 0}

    test "select error", context, do:
      ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.sql_query(context.conn, 'invalid query') end)

    test "extended query error", context, do:
      ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.param_query(context.conn, 'invalid query', []) end)
  end


  defp param_select(conn, type, value, cast \\ nil) do
    cast = if cast != nil, do: "::#{cast}"
    {:selected, ['x'], rows} = :odbc.param_query(conn, 'select $1#{cast} as x from users', [{type, [value]}])
    [{result}] = Enum.uniq(rows)
    result
  end

  defp connect(user, params \\ []) do
    params = Keyword.merge(
      [
        user: user.email,
        password: Manager.user_password(),
        database: Manager.data_source_global_id(),
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
      |> Enum.map(fn({name, value}) -> "#{name}=#{value};" end)
      |> Enum.join()
      |> to_charlist()

    :odbc.connect(connection_string, [])
  end
end
