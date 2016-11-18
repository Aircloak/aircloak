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
