defmodule Mix.Tasks.Gen.TestData do
  if Mix.env == :dev do
    @shortdoc "Generates an interlinked dataset for cloak testing."
    @moduledoc """
    Generates a test dataset of users and items related to these users.
    For more information, please consult the `Compliance.TableDefinitions` module.

    The command takes two parameters:
    - the name of the data source config file to be used
    - the number of users to be generated

    The config file is expected to be found in `priv/config`

    Example usage:
    `mix gen.test_data compliance 1000`

    Will generate a dataset of 1000 users based on the
    data source definitions found in the config file at
    `/priv/config/compliance.json`
    """

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    use Mix.Task

    @doc false
    def run([config_name, num_users]) do
      num_users = String.to_integer(num_users)
      IO.puts "Generating data for #{num_users} users."
      data = Compliance.Data.generate(num_users)

      config_name
      |> Compliance.DataSources.all_from_config()
      |> Compliance.DataSources.setup(data)
    end
    def run(_) do
      IO.puts """

      Please run as:
        mix gen.test_data <config-name> <num-users>
      """
    end


    # -------------------------------------------------------------------
    # Internal functions
    # -------------------------------------------------------------------
  end
end
