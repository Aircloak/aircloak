if Mix.env == :dev do
  defmodule Mix.Tasks.Gen.DataSourceConfig do
    @shortdoc "Generates a database JSON schema for a test data source."
    @moduledoc """
    Generates a complete data source definition for the datasets generated
    by `mix gen.test_data`, given a scaffoldded datasource definition.

    This comes in useful when you want to generate a dataset that can
    subsequently be queried by a normal cloak.

    Example usage.

    Say you want to produce a performance test dataset based on the
    test data generator. You provide a data source scaffold named
    `performance` stored under `/priv/config/performance.json`.

    It could look as follows:

    ```
    {
      "salt": "some salt",
      "data_sources": [
        {
          "driver": "postgresql",
          "name": "some name",
          "parameters": {
            "hostname": "localhost",
            "username": "some user",
            "database": "some database"
          },
          "tables": {
          }
        }
      ]
    }
    ```

    Running `mix gen.data_source_config performance`
    will print the data source definition to STDOUT.

    Running `mix gen.data_source_config performance output_file_path` will
    write the full data source definition to the specified path.
    """

    alias Compliance.DataSources

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    use Mix.Task

    @doc false
    def run([config_name, output_path]) do
      File.write!(output_path, build_config(config_name))
      IO.puts "Configuration for #{config_name} generated and written to #{output_path}"
    end
    def run([config_name]) do
      IO.puts build_config(config_name)
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

    defp build_config(config_name) do
      original_config = Aircloak.atomize_keys(DataSources.read_config(config_name))
      original_data_sources = original_config.data_sources

      expanded_data_sources = config_name
      |> DataSources.all_from_config()
      |> DataSources.complete_data_source_definitions()
      |> Enum.group_by(& &1.parameters)
      |> Enum.flat_map(fn({parameters, data_sources}) ->
        original_data_source = Enum.find(original_data_sources, & parameters == &1.parameters)
        Enum.map(data_sources, & Map.merge(original_data_source, Map.take(&1, [:name, :marker, :tables])))
      end)

      original_config
      |> Map.put(:data_sources, expanded_data_sources)
      |> Poison.encode!(pretty: true)
    end
  end
end
