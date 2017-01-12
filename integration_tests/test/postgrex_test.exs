defmodule IntegrationTest.PostgrexTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup_all do
    user = Manager.create_air_user()
    {:ok, user: user}
  end

  setup context do
    {:ok, conn} = connect(context.user)
    {:ok, Map.put(context, :conn, conn)}
  end

  test "error in describe", context do
    assert {:error, error} = Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    assert error.postgres.message == "The type for the `$1` parameter cannot be determined."
  end

  test "select a string", context do
    result = Postgrex.query!(context.conn, "select cast($1 as text) from users", ["foobar"])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [["foobar"]]
  end

  test "select an integer", context do
    result = Postgrex.query!(context.conn, "select cast($1 as integer) from users", [42])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[42]]
  end

  test "select a boolean", context do
    result = Postgrex.query!(context.conn, "select cast($1 as boolean) from users", [true])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[true]]
  end

  test "multiple queries on the same connection", context do
    Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    Postgrex.query!(context.conn, "select cast($1 as text) from users", ["foobar"])
    Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    Postgrex.query!(context.conn, "select cast($1 as text) from users", ["foobar"])
  end

  test "recovery after an error", context do
    Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    Postgrex.query!(context.conn, "select cast($1 as text) from users", ["foobar"])
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
