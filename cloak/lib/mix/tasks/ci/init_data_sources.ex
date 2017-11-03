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
    |> Enum.each(&establish_connection(&1))

  defp establish_connection(%{driver: Cloak.DataSource.PostgreSQL, parameters: params}) do
    Application.ensure_all_started(:postgrex)
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
end
