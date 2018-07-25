defmodule IntegrationTest.PostgrexTest do
  use ExUnit.Case, async: true

  alias IntegrationTest.Manager

  setup do
    user = Manager.create_air_user()
    {:ok, conn} = connect(user)
    {:ok, user: user, conn: conn}
  end

  test "error in describe", context do
    assert {:error, error} = Postgrex.query(context.conn, "select $1 from users", ["foobar"])
    assert error.postgres.message == "The type for parameter `$1` cannot be determined."
  end

  test(
    "select a string",
    context,
    do: assert(param_select(context.conn, "foobar", "text") == "foobar")
  )

  test("select an integer", context, do: assert(param_select(context.conn, 42, "integer") == 42))

  test "select a boolean", context do
    assert param_select(context.conn, true, "boolean") == true
    assert param_select(context.conn, false, "boolean") == false
  end

  test("select a real", context, do: assert(param_select(context.conn, 3.14, "real") == 3.14))

  test(
    "select a date",
    context,
    do: assert(param_select(context.conn, ~D[2017-01-31], "date") == ~D[2017-01-31])
  )

  test(
    "select a time",
    context,
    do: assert(param_select(context.conn, ~T[01:02:03.004000], "time") == ~T[01:02:03.004000])
  )

  test "select a datetime", context do
    datetime = ~N[2017-01-31 01:02:03.0040000]
    assert param_select(context.conn, datetime, "datetime") == datetime
  end

  for type <- ["text", "integer", "real", "boolean", "date", "time", "datetime"] do
    test(
      "select null as #{type}",
      context,
      do: assert(param_select(context.conn, nil, unquote(type)) == nil)
    )
  end

  test "anonymized text value returns a *", context do
    assert Enum.uniq(Postgrex.query!(context.conn, "select user_id from users", []).rows) == [
             ["*"]
           ]
  end

  test "anonymized integer value returns nil", context do
    assert Enum.uniq(Postgrex.query!(context.conn, "select cast(user_id as integer) from users", []).rows) == [[nil]]
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

  test "can't query on active connection once user is disabled", context do
    assert {:ok, _} = Postgrex.query(context.conn, "select 1 FROM users", [])
    # required in order to be able to disable the user
    _secondary_user = Manager.create_air_user()
    assert {:ok, _} = Air.Service.User.disable!(context.user)
    assert_raise DBConnection.ConnectionError, fn -> Postgrex.query(context.conn, "select 1 FROM users", []) end
  end

  defp param_select(conn, value, cast) do
    result = Postgrex.query!(conn, "select $1::#{cast} from users", [value])
    [[value]] = Enum.uniq(result.rows)
    value
  end

  defp connect(user) do
    Postgrex.start_link(
      hostname: "localhost",
      port: Application.fetch_env!(:air, Air.PsqlServer) |> Keyword.fetch!(:port),
      username: user.email,
      password: Manager.user_password(),
      database: Manager.data_source_name(),
      ssl: true
    )
  end
end
