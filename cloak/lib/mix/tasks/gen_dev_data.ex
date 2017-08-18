if Mix.env == :dev do
  defmodule Mix.Tasks.GenDevData do
    @shortdoc "Generates test data for dev database."
    @moduledoc false

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    use Mix.Task

    @doc false
    def run(_args) do
      Enum.each(
        %{
          postgresql: postgresql_connection()
        },
        &insert(&1, integers_data())
      )
    end

    defp integers_data() do
      %{
        name: "integers",
        columns: [{"user_id", "integer"}, {"value", "integer"}],
        data:
          Stream.flat_map(
            1..2500,
            fn(value) -> Stream.map(1..10, &[&1, value]) end
          )
      }
    end

    defp insert({:postgresql, conn}, table_spec) do
      Postgrex.query!(conn, "DROP TABLE IF EXISTS #{table_spec.name}", [])

      create_statement = "
        CREATE TABLE #{table_spec.name} (
          #{
            table_spec.columns
            |> Enum.map(fn({name, type}) -> "#{name} #{type}" end)
            |> Enum.join(", ")
          }
        )"

      Postgrex.query!(conn, create_statement, [])

      table_spec.data
      |> Stream.map(&"(#{Enum.join(&1, ", ")})")
      |> Stream.chunk(10, 10, [])
      |> Stream.map(&Enum.join(&1, ","))
      |> Enum.each(
        fn(chunk_sql) ->
          Postgrex.query!(
            conn,
            [
              "
                INSERT INTO #{table_spec.name} (#{table_spec.columns |> Enum.map(&elem(&1, 0)) |> Enum.join(", ")})
                VALUES #{chunk_sql}
              "
            ],
            []
          )
        end
      )
    end

    defp postgresql_connection() do
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

      conn
    end
  end
end
