defmodule IntegrationTest.PsqlTest do
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

  test "disconnecting", context do
    {:ok, conn} = connect(context.user)
    assert :ok = :odbc.disconnect(conn)
  end

  test "show tables", context do
    {:ok, conn} = connect(context.user)
    assert :odbc.sql_query(conn, 'show tables') == {:selected, ['name'], [{'users'}]}
  end

  test "show columns", context do
    {:ok, conn} = connect(context.user)
    assert :odbc.sql_query(conn, 'show columns from users') == {:selected, ['name', 'type'], [
      {'user_id', 'text'},
      {'name', 'text'},
      {'height', 'integer'}
    ]}
  end

  test "select", context do
    {:ok, conn} = connect(context.user)
    assert {:selected, ['name', 'height'], rows} = :odbc.sql_query(conn, 'select name, height from users')
    assert Enum.uniq(rows) == [{'john', '180'}]
  end

  test "extended query", context do
    {:ok, conn} = connect(context.user)
    assert {:selected, ['name', 'height'], rows} = :odbc.param_query(
      conn,
      'select name, height + $1 as height from users where height = $2',
      [{:sql_integer, [1]}, {:sql_integer, [180]}]
    )
    assert Enum.uniq(rows) == [{'john', '181'}]
  end

  test "select error", context do
    {:ok, conn} = connect(context.user)
    ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.sql_query(conn, 'invalid query') end)
  end

  test "extended query error", context do
    {:ok, conn} = connect(context.user)
    ExUnit.CaptureLog.capture_log(fn -> assert {:error, _} = :odbc.param_query(conn, 'invalid query', []) end)
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
