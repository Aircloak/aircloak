if Mix.env == :dev do
  defmodule Mix.Tasks.GenDevData do
    @shortdoc "Generates test data for dev database."
    @moduledoc false

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    use Mix.Task

    @doc false
    def run(_args) do
      Application.ensure_all_started(:postgrex)
      db_params =
        Aircloak.DeployConfig.fetch!(:cloak, "data_sources")
        |> Enum.find(&(&1["name"] == "cloak_postgres_native"))
        |> Map.fetch!("parameters")

      {:ok, conn} = Postgrex.start_link(
        database: db_params["database"],
        hostname: db_params["hostname"],
        username: db_params["username"],
        password: db_params["password"]
      )

      Postgrex.query!(conn, "DROP TABLE IF EXISTS integers", [])
      Postgrex.query!(conn, "CREATE TABLE integers (user_id INTEGER NOT NULL, value INTEGER NOT NULL)", [])
      for value <- 1..2500 do
        values_str =
          1..10
          |> Enum.map(fn(user_id) -> "(#{user_id}, #{value})" end)
          |> Enum.intersperse(",")

        Postgrex.query!(conn, "INSERT INTO integers (user_id, value) VALUES #{values_str}", [])
      end
    end
  end
end
