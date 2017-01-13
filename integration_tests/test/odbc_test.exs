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

    test "select an integer", context, do:
      # The reason that the result is a string is because the server returns `int8`, and it appears that
      # either the ODBC driver, or ODBC itself converts this into a string.
      assert param_select(context.conn, :sql_integer, 42) == '42'

    test "select a boolean", context, do:
      assert param_select(context.conn, :sql_bit, true) == true

    test "select a real", context, do:
      assert param_select(context.conn, :sql_real, 3.14) == 3.14

    test "select error", context, do:
      ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.sql_query(context.conn, 'invalid query') end)

    test "extended query error", context, do:
      ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.param_query(context.conn, 'invalid query', []) end)
  end


  defp param_select(conn, type, value) do
    {:selected, ['x'], rows} = :odbc.param_query(conn, 'select $1 as x from users', [{type, [value]}])
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
