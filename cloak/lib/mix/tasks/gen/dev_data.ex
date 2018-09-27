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
    |> Stream.filter(&(&1.name in ["saphana", "cloak_postgres_native"]))
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

  defp insert({%{driver: Cloak.DataSource.SAPHana}, conn}, table_spec) do
    Cloak.SapHanaHelpers.recreate_table!(
      conn,
      __AC__DEFAULT_SAP_HANA_SCHEMA__!(),
      table_spec.name,
      table_def(table_spec)
    )

    column_names = Enum.map(table_spec.columns, &elem(&1, 0))

    insert_chunks(
      table_spec.data,
      &Cloak.SapHanaHelpers.insert_rows!(
        conn,
        __AC__DEFAULT_SAP_HANA_SCHEMA__!(),
        table_spec.name,
        column_names,
        &1
      )
    )
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
      username: datasource.parameters[:username],
      password: datasource.parameters[:password]
    )
  end

  defp open_connection(%{driver: Cloak.DataSource.SAPHanaRODBC} = datasource) do
    with :ok <- sap_hana_connectivity_possible(),
         {:ok, _} <- Application.ensure_all_started(:odbc),
         {:ok, default_schema} <- __AC__DEFAULT_SAP_HANA_SCHEMA__(),
         connection_params = Map.put(datasource.parameters, :default_schema, default_schema),
         :ok <- Cloak.SapHanaHelpers.ensure_schema(connection_params, default_schema),
         do: Cloak.SapHanaHelpers.connect(connection_params)
  end

  defp __AC__DEFAULT_SAP_HANA_SCHEMA__!() do
    {:ok, default_schema} = __AC__DEFAULT_SAP_HANA_SCHEMA__()
    default_schema
  end

  defp __AC__DEFAULT_SAP_HANA_SCHEMA__() do
    case Cloak.DataSource.SAPHana.default_schema() do
      nil ->
        [
          "",
          "Default schema for SAP HANA not specified. SAP HANA data will not be recreated.",
          "To use SAP HANA datasource, add the following to `dev.local.exs`:",
          "",
          "  config :cloak, :sap_hana, default_schema: your_schema_name",
          "",
          "See `README.md` for more details.",
          ""
        ]
        |> Enum.join("\n")
        |> IO.puts()

        {:error, :default_schema_not_specified}

      default_schema ->
        {:ok, default_schema}
    end
  end

  defp sap_hana_connectivity_possible() do
    if :os.type() == {:unix, :darwin} do
      [
        "#{IO.ANSI.red()}",
        "Can't connect to SAP HANA data source from a macOS machine. SAP HANA data will not be recreated.",
        "To work with SAP HANA data sources, start a CI container with `make ci.compliance.debug`.",
        "See `README.md` for more details.",
        "#{IO.ANSI.reset()}"
      ]
      |> Enum.join("\n")
      |> IO.puts()

      {:error, :sap_hana_not_supported}
    else
      :ok
    end
  end

  defp create_statement(table_spec), do: "CREATE TABLE #{table_spec.name} (#{table_def(table_spec)})"

  defp table_def(table_spec),
    do:
      table_spec.columns
      |> Enum.map(fn {name, type} -> ~s/"#{name}" #{type}/ end)
      |> Enum.join(", ")
end
