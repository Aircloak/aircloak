defmodule IntegrationTest.OdbcTest do
  use ExUnit.Case, async: false

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

  for {client_ssl_mode, ssl_config, should_succeed?} <- [
    # supported successful combinations
    {"disable", %{"require_ssl" => false}, true},
    {"allow", %{"require_ssl" => false}, true},
    {"allow", %{"require_ssl" => true}, true},
    {"prefer", %{"require_ssl" => false}, true},
    {"prefer", %{"require_ssl" => true}, true},
    {"require", %{"require_ssl" => false}, true},
    {"require", %{"require_ssl" => true}, true},
    # errors when ssl is required
    {"disable", %{"require_ssl" => true}, false},
    {"allow", %{"require_ssl" => true, "certfile" => "invalid_file"}, false},
    {"allow", %{"require_ssl" => true, "certfile" => nil}, false},
    {"allow", %{"require_ssl" => true, "keyfile" => "invalid_file"}, false},
    {"allow", %{"require_ssl" => true, "keyfile" => nil}, false},
    {"prefer", %{"require_ssl" => true, "certfile" => "invalid_file"}, false},
    {"prefer", %{"require_ssl" => true, "certfile" => nil}, false},
    {"prefer", %{"require_ssl" => true, "keyfile" => "invalid_file"}, false},
    {"prefer", %{"require_ssl" => true, "keyfile" => nil}, false},
    {"require", %{"require_ssl" => true, "certfile" => "invalid_file"}, false},
    {"require", %{"require_ssl" => true, "certfile" => nil}, false},
    {"require", %{"require_ssl" => false, "certfile" => "invalid_file"}, false},
    {"require", %{"require_ssl" => false, "certfile" => nil}, false},
    {"require", %{"require_ssl" => true, "keyfile" => "invalid_file"}, false},
    {"require", %{"require_ssl" => true, "keyfile" => nil}, false},
    {"require", %{"require_ssl" => false, "keyfile" => "invalid_file"}, false},
    {"require", %{"require_ssl" => false, "keyfile" => nil}, false},
  ] do
    test "connecting when client sslmode=`#{client_ssl_mode}`, server config #{inspect ssl_config}", context do
      ExUnit.CaptureLog.capture_log(fn ->
        original_settings = Aircloak.DeployConfig.fetch!(:air, "psql_server")
        Aircloak.DeployConfig.update(:air, "psql_server", &Map.merge(&1, unquote(Macro.escape(ssl_config))))
        var!(connect_result) = connect(var!(context).user, sslmode: unquote(client_ssl_mode))

        unquote(
          if should_succeed? do
            quote do
              assert {:ok, _} = var!(connect_result)
            end
          else
            quote do
              assert {:error, _} = var!(connect_result)
            end
          end
        )

        Aircloak.DeployConfig.update(:air, "psql_server", fn(_) -> original_settings end)
      end)
    end
  end

  describe "connection tests" do
    setup context do
      {:ok, conn} = connect(context.user)
      {:ok, Map.put(context, :conn, conn)}
    end

    test "disconnecting", context, do:
      assert :ok = :odbc.disconnect(context.conn)

    test "query context is properly set", context do
      :odbc.sql_query(context.conn, 'show tables')
      query = Air.Service.Query.last_for_user(context.user, :psql)
      assert query.context == :psql
      assert query.statement == "show tables"
    end

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

    test "closing a cursor", context, do:
      assert :odbc.sql_query(context.conn, 'close "some_cursor"') == {:updated, 0}

    test "select error", context, do:
      ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.sql_query(context.conn, 'invalid query') end)

    test "extended query error", context, do:
      ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.param_query(context.conn, 'invalid query', []) end)
  end


  defp param_select(conn, type, value, cast \\ nil) do
    cast = if cast != nil, do: "::#{cast}"
    {:selected, ['x'], rows} = :odbc.param_query(conn, 'select ?#{cast} as x from users', [{type, [value]}])
    [{result}] = Enum.uniq(rows)
    result
  end

  defp connect(user, params \\ []) do
    params = Keyword.merge(
      [
        user: user.email,
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
      |> Enum.map(fn({name, value}) -> "#{name}=#{value};" end)
      |> Enum.join()
      |> to_charlist()

    :odbc.connect(connection_string, [])
  end
end
