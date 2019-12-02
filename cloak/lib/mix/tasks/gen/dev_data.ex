defmodule Mix.Tasks.Gen.DevData do
  @shortdoc "Generates test data for dev database."
  @moduledoc false

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task
  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  # Mix.Task callbacks
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run(_args) do
    generate_integers()
    Cloak.PerformanceData.generate(num_users: 100)
  end

  # -------------------------------------------------------------------
  # Integers table
  # -------------------------------------------------------------------

  defp generate_integers() do
    IO.puts("generating data")
    data = integers_data()

    Compliance.DataSources.all_from_config("dev")
    |> Stream.filter(&(&1.name in ["cloak_postgres_native"]))
    |> Stream.map(&{&1, open_connection(&1)})
    |> Stream.filter(fn {_datasource, connect_result} -> match?({:ok, _conn}, connect_result) end)
    |> Stream.map(fn {datasource, {:ok, conn}} -> {datasource, conn} end)
    |> Enum.each(fn {datasource, _} = descriptor ->
      IO.puts("importing to #{datasource.name}")
      insert(descriptor, data)
      IO.puts("#{IO.ANSI.green()}done#{IO.ANSI.reset()}\n")
    end)
  end

  defp integers_data() do
    %{
      name: "integers",
      columns: [{"user_id", "integer"}, {"value", "integer"}],
      data: Stream.flat_map(1..10_000, fn value -> Stream.map(1..10, &[&1, value]) end)
    }
  end

  defp insert({%{driver: Cloak.DataSource.PostgreSQL}, conn}, table_spec) do
    Postgrex.query!(conn, "DROP TABLE IF EXISTS #{table_spec.name}", [])
    Postgrex.query!(conn, create_statement(table_spec), [])
    insert_chunks(table_spec.data, &postgresql_insert_rows(conn, table_spec, &1))
  end

  defp insert_chunks(rows, inserter) do
    chunks = Enum.chunk_every(rows, 1000)
    num_chunks = length(chunks)

    chunks
    |> Enum.with_index()
    |> Enum.each(fn {rows, index} ->
      IO.write("\rchunk #{index + 1}/#{num_chunks}")
      inserter.(rows)
    end)

    IO.puts("\n")
  end

  defp postgresql_insert_rows(conn, table_spec, rows),
    do:
      Postgrex.query!(
        conn,
        [
          "
          INSERT INTO #{table_spec.name} (#{table_spec.columns |> Enum.map(&elem(&1, 0)) |> Enum.join(", ")})
          VALUES #{rows |> Stream.map(&"(#{Enum.join(&1, ", ")})") |> Enum.join(",")}
        "
        ],
        []
      )

  defp open_connection(%{driver: Cloak.DataSource.PostgreSQL} = datasource) do
    Application.ensure_all_started(:postgrex)

    Postgrex.start_link(
      database: datasource.parameters[:database],
      hostname: datasource.parameters[:hostname],
      port: datasource.parameters[:port],
      username: datasource.parameters[:username],
      password: datasource.parameters[:password]
    )
  end

  defp create_statement(table_spec), do: "CREATE TABLE #{table_spec.name} (#{table_def(table_spec)})"

  defp table_def(table_spec),
    do:
      table_spec.columns
      |> Enum.map(fn {name, type} -> ~s/"#{name}" #{type}/ end)
      |> Enum.join(", ")
end
