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
    assert error.postgres.message == "The type for parameter `$1` cannot be determined."
  end

  test "select a string", context do
    result = Postgrex.query!(context.conn, "select $1::text from users", ["foobar"])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [["foobar"]]
  end

  test "select an integer", context do
    result = Postgrex.query!(context.conn, "select $1::integer from users", [42])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[42]]
  end

  test "select a boolean", context do
    result = Postgrex.query!(context.conn, "select $1::boolean from users", [true])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[true]]
  end

  test "select a real", context do
    result = Postgrex.query!(context.conn, "select $1::real from users", [3.14])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[3.14]]
  end

  test "select a date", context do
    result = Postgrex.query!(context.conn, "select $1::date from users",
      [%Postgrex.Date{year: 2017, month: 1, day: 31}])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[%Postgrex.Date{year: 2017, month: 1, day: 31}]]
  end

  test "select a time", context do
    result = Postgrex.query!(context.conn, "select $1::time from users",
      [%Postgrex.Time{hour: 1, min: 2, sec: 3, usec: 4}])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[%Postgrex.Time{hour: 1, min: 2, sec: 3, usec: 4}]]
  end

  test "select a datetime", context do
    result = Postgrex.query!(context.conn, "select $1::datetime from users",
      [%Postgrex.Timestamp{year: 2017, month: 1, day: 31, hour: 1, min: 2, sec: 3, usec: 4}])
    assert result.columns == ["cast"]
    assert Enum.uniq(result.rows) == [[%Postgrex.Timestamp{year: 2017, month: 1, day: 31, hour: 1, min: 2, sec: 3, usec: 4}]]
  end

  test "multiple queries on the same connection", context do
    assert {:error, _} = Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    assert {:ok, _} = Postgrex.query(context.conn, "select $1::text from users", ["foobar"])
    assert {:error, _} = Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    assert {:ok, _} = Postgrex.query(context.conn, "select $1::text from users", ["foobar"])
  end

  test "recovery after an error", context do
    assert {:error, _} = Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    assert {:ok, _} = Postgrex.query(context.conn, "select $1::text from users", ["foobar"])
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
