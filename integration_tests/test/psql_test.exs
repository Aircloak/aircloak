defmodule IntegrationTest.PsqlTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  test "invalid connect params" do
    assert {:error, msg} = connect(user: "invalid user")
    assert to_string(msg) =~ ~r/Authentication failed/

    assert {:error, msg} = connect(password: "invalid password")
    assert to_string(msg) =~ ~r/Authentication failed/

    assert {:error, msg} = connect(database: "invalid database")
    assert to_string(msg) =~ ~r/Authentication failed/
  end

  test "show tables" do
    {:ok, conn} = connect()
    assert :odbc.sql_query(conn, 'show tables') == {:selected, ['name'], [{'users'}]}
  end

  test "show columns" do
    {:ok, conn} = connect()
    assert :odbc.sql_query(conn, 'show columns from users') == {:selected, ['name', 'type'], [
      {'user_id', 'text'},
      {'name', 'text'},
      {'height', 'integer'}
    ]}
  end

  test "select" do
    {:ok, conn} = connect()
    assert {:selected, ['name', 'height'], rows} = :odbc.sql_query(conn, 'select name, height from users')
    assert Enum.all?(rows, &(&1 == {'john', '180'}))
  end

  test "extended query" do
    {:ok, conn} = connect()
    assert {:selected, ['name', 'height'], rows} = :odbc.param_query(
      conn,
      'select name, height + $1 as height from users where height = $2',
      [{:sql_integer, [1]}, {:sql_integer, [180]}]
    )
    assert Enum.all?(rows, &(&1 == {'john', '181'}))
  end

  defp connect(params \\ []) do
    params = Keyword.merge(
      [user: Manager.user_mail(), password: Manager.user_password(), database: Manager.data_source_global_id()],
      params
    )

    connection_string =
      %{
        "DSN" => "PostgreSQL",
        "Server" => "localhost",
        "Port" => Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
        "sslmode" => "require",
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
