defmodule Mix.Tasks.Gen.DevData do
  if Mix.env == :dev do
    @shortdoc "Generates test data for dev database."
    @moduledoc false

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    use Mix.Task

    @impl Mix.Task
    def run(_args), do:
      [:postgresql, :saphana]
      |> Enum.map(&{&1, open_connection(&1)})
      |> Enum.filter(fn({_adapter, connect_result}) -> match?({:ok, _conn}, connect_result) end)
      |> Enum.map(fn({adapter, {:ok, conn}}) -> {adapter, conn} end)
      |> Enum.each(fn({adapter, _} = descriptor) ->
        IO.puts "importing to #{adapter}"
        insert(descriptor, integers_data())
        IO.puts "done\n"
      end)

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

      Postgrex.query!(conn, create_statement(table_spec), [])

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
    defp insert({:saphana, conn}, table_spec) do
      column_names = Enum.map(table_spec.columns, &elem(&1, 0))

      Cloak.SapHanaHelpers.recreate_table!(conn, default_sap_hana_schema!(), table_spec.name, table_def(table_spec))

      chunks = Enum.chunk(table_spec.data, 1000, 1000, [])

      chunks
      |> Enum.with_index()
      |> Enum.each(fn({rows, index}) ->
        IO.puts "chunk #{index+1}/#{length(chunks)}"
        Cloak.SapHanaHelpers.insert_rows!(conn, default_sap_hana_schema!(), table_spec.name, column_names, rows)
      end)
    end

    defp open_connection(:postgresql) do
      Application.ensure_all_started(:postgrex)
      db_params =
        Aircloak.DeployConfig.fetch!(:cloak, "data_sources")
        |> Cloak.DataSource.Utility.load_individual_data_source_configs()
        |> Enum.find(&(&1["name"] == "cloak_postgres_native"))
        |> Map.fetch!("parameters")

      Postgrex.start_link(
        database: db_params["database"],
        hostname: db_params["hostname"],
        username: db_params["username"],
        password: db_params["password"]
      )
    end
    defp open_connection(:saphana) do
      with \
        :ok <- sap_hana_connectivity_possible(),
        {:ok, _} <- Application.ensure_all_started(:odbc),
        {:ok, default_schema} <- default_sap_hana_schema(),
        connection_params = sap_hana_connection_params(default_schema),
        :ok <- Cloak.SapHanaHelpers.ensure_schema(connection_params, default_schema),
      do:
        Cloak.SapHanaHelpers.connect(connection_params)
    end

    defp default_sap_hana_schema!() do
      {:ok, default_schema} = default_sap_hana_schema()
      default_schema
    end

    defp default_sap_hana_schema() do
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
            "",
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
          "",
          "Can't connect to SAP HANA data source from a macOS machine. SAP HANA data will not be recreated.",
          "To work with SAP HANA data sources, start a dev container with `make dev-container`.",
          "See `README.md` for more details.",
          "",
        ]
        |> Enum.join("\n")
        |> IO.puts()

        {:error, :sap_hana_not_supported}
      else
        :ok
      end
    end

    defp sap_hana_connection_params(default_schema), do:
      Aircloak.DeployConfig.fetch!(:cloak, "data_sources")
      |> Cloak.DataSource.Utility.load_individual_data_source_configs()
      |> Enum.find(&(&1["name"] == "saphana"))
      |> Map.fetch!("parameters")
      |> Enum.map(fn({key, value}) -> {String.to_atom(key), value} end)
      |> Enum.into(%{})
      |> Map.put(:default_schema, default_schema)

    defp create_statement(table_spec), do:
      "CREATE TABLE #{table_spec.name} (#{table_def(table_spec)})"

    defp table_def(table_spec), do:
      table_spec.columns
      |> Enum.map(fn({name, type}) -> ~s/"#{name}" #{type}/ end)
      |> Enum.join(", ")
  end
end
