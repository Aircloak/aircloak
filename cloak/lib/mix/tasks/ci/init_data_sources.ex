defmodule Mix.Tasks.Ci.InitDataSources do
  @moduledoc """
  Initializes data sources.
  """

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task

  @impl Mix.Task
  def run(config), do:
    config
    |> Compliance.DataSources.all_from_config()
    |> Enum.map(&Task.async(fn -> establish_connection(&1) end))
    |> Task.yield_many(:timer.minutes(3))

  defp establish_connection(%{driver: Cloak.DataSource.PostgreSQL, parameters: params}) do
    Application.ensure_all_started(:postgrex)

    await_port(params.hostname, 5432)

    me = self()
    ref = make_ref()

    {:ok, conn} = Postgrex.start_link(
      after_connect: fn(_) -> send(me, ref) end,
      database: "postgres",
      hostname: params.hostname,
      username: "postgres"
    )
    receive do
      ^ref -> :ok
    after :timer.seconds(10) ->
      raise "Timeout connecting to the database."
    end

    case Postgrex.query!(
      conn,
      "SELECT count(*) FROM pg_catalog.pg_user WHERE usename = '#{params.username}'",
      []
    ).rows do
      [[1]] -> :ok
      [[0]] -> Postgrex.query!(conn, "CREATE USER #{params.username}", [])
    end

    Postgrex.query!(conn, "DROP DATABASE IF EXISTS #{params.database}", [])
    Postgrex.query!(conn, "CREATE DATABASE #{params.database} ENCODING 'UTF8'", [])
    Postgrex.query!(conn, "GRANT ALL PRIVILEGES ON DATABASE #{params.database} TO #{params.username}", [])
  end
  defp establish_connection(%{driver: Cloak.DataSource.SQLServer, parameters: params}) do
    Application.ensure_all_started(:odbc)

    await_port(params.hostname, 1433)

    conn =
      Cloak.DataSource.SQLServer.connect!(
        hostname: params.hostname,
        database: "master",
        username: "sa",
        password: "7fNBjlaeoRwz*zH9"
      )

    :odbc.sql_query(conn, ~c/
      IF EXISTS(select * from sys.databases where name='#{params.database}')
        DROP DATABASE #{params.database}

      CREATE DATABASE #{params.database}
    /)
  end
  defp establish_connection(%{driver: Cloak.DataSource.MySQL, parameters: params}) do
    Application.ensure_all_started(:mariaex)

    await_port(params.hostname, 3306)

    me = self()
    ref = make_ref()

    {:ok, conn} =
      Mariaex.start_link(
        database: "mysql",
        hostname: params.hostname,
        username: "root",
        after_connect: fn(_) -> send(me, ref) end,
      )

    receive do
      ^ref -> :ok
    after :timer.seconds(10) ->
      raise "Timeout connecting to the database."
    end

    case Mariaex.query!(
      conn,
      "SELECT COUNT(*) FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = '#{params.database}'"
    ).rows do
      [[1]] -> :ok
      [[0]] -> Mariaex.query!(conn, "CREATE DATABASE #{params.database}")
    end
  end
  defp establish_connection(%{driver: Cloak.DataSource.SAPHana}), do:
    :ok

  defp await_port(host, port) do
    IO.puts "waiting for #{host}:#{port}..."

    task =
      Task.async(fn ->
        fn -> :gen_tcp.connect(to_charlist(host), port, []) end
        |> Stream.repeatedly()
        |> Stream.drop_while(&match?({:error, _}, &1))
        |> Enum.take(1)
      end)

    case Task.yield(task, :timer.minutes(2)) do
      nil -> Mix.raise "#{host}:#{port} is not available"
      _ -> IO.puts "#{host}:#{port} is available"
    end
  end
end
