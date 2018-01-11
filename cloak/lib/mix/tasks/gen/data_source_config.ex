defmodule Mix.Tasks.Gen.DataSourceConfig do
  if Mix.env == :dev do
    @shortdoc "Generates a database JSON schema for a test data source."
    @moduledoc """
    Generates a complete data source definition for the datasets generated
    by `mix gen.test_data`, given a scaffoldded datasource definition.

    This comes in useful when you want to generate a dataset that can
    subsequently be queried by a normal cloak.

    Example usage.

    Say you want to produce a performance test dataset based on the
    test data generator. You provide a data source scaffold named
    `performance` stored under `/priv/config/performance.json`, as well
    as the data source definition in some auxiliary folder referenced
    from the config (the convention for local data sources is to give
    the folder the same name as the data source config, `performance`
    in this case):

    It could look as follows:

    ```
    {
      "salt": "some salt",
      "data_sources": "performance"
      ]
    }
    ```

    With a data source definition stored under `/priv/config/performance/data_source.json`:

    ```
    {
      "driver": "postgresql",
      "name": "data_source",
      "parameters": {
        "hostname": "localhost",
        "username": "some user",
        "database": "some database"
      },
      "tables": {
      }
    }
    ```

    Running `mix gen.data_source_config performance`
    will print the data source definitions referenced
    in the performance data source config to STDOUT.

    Running `mix gen.data_source_config performance output_dir_path` will
    write the data source definitions as individual files in the specified directory.
    """

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    use Mix.Task

    @impl Mix.Task
    def run([config_name, output_dir]) do
      IO.puts "Writing data source definitions for #{config_name} to directory #{output_dir}"
      build_data_source_definitions(config_name)
      |> Enum.each(fn(data_source_config) ->
        path = Path.join([output_dir, data_source_config[:name] <> ".json"])
        File.write!(path, Poison.encode!(data_source_config, pretty: true))
        IO.puts "OK: #{path}"
      end)
    end
    def run([config_name]) do
      build_data_source_definitions(config_name)
      |> Enum.each(fn(data_source_config) ->
        IO.puts("# #{data_source_config[:name]}:")
        IO.puts(Poison.encode!(data_source_config, pretty: true))
        IO.puts("\n")
      end)
    end
    def run(_) do
      IO.puts """

      Please run as:
        mix gen.data_source_config <config-name>
      """
    end


    # -------------------------------------------------------------------
    # Internal functions
    # -------------------------------------------------------------------

    defp build_data_source_definitions(config_name) do
      config_name
      |> Compliance.DataSources.all_from_config()
      |> Compliance.DataSources.complete_data_source_definitions()
      |> Enum.map(fn(data_source) ->
        {:ok, name} = Cloak.DataSource.Utility.driver_to_name(data_source.driver)
        Map.put(data_source, :driver, name)
      end)
      |> Enum.map(& Map.take(&1, [:marker, :name, :parameters, :tables, :driver]))
    end
  end
end
