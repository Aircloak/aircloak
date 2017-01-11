defmodule IntegrationTest.PostgrexTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    user = Manager.create_air_user()
    {:ok, conn} = connect(user)
    {:ok, user: user, conn: conn}
  end

  test "select a string", context do
    result = Postgrex.query!(
      context.conn,
      "select cast($1 as text) from users",
      ["foobar"]
    )
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [["foobar"]]
  end

  test "select an integer", context do
    result = Postgrex.query!(
      context.conn,
      "select cast($1 as integer) from users",
      [42]
    )
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[42]]
  end

  defp connect(user) do
    Postgrex.start_link(
      hostname: "localhost",
      port: Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
      username: user.email,
      password: Manager.user_password(),
      database: Manager.data_source_global_id(),
      ssl: true
    )
  end
end
