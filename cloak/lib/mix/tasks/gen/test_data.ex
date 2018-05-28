defmodule Mix.Tasks.Gen.TestData do
  if Mix.env() in [:test, :dev] do
    @shortdoc "Generates an interlinked dataset for cloak testing."
    @moduledoc """
    Generates a test dataset of users and items related to these users.
    For more information, please consult the `Compliance.TableDefinitions` module.

    The command takes two parameters:
    - the name of the data source config file to be used
    - the number of users to be generated

    Optionally, you can also provide the `--concurrency positive_integer` option to specify the number of concurrent
    insert processes used in each data source. The default value for this parameter is the number of online schedulers.

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

    @impl Mix.Task
    def run(args) do
      # this ensures that protocols are consolidated, which improves the overall running time
      Mix.Task.run("app.start", ["--no-start"])

      case OptionParser.parse!(args, switches: [concurrency: :integer]) do
        {options, [config_name, num_users]} ->
          concurrency = Keyword.get(options, :concurrency, System.schedulers_online())
          num_users = String.to_integer(num_users)
          IO.puts("Generating data for #{num_users} users.")

          Compliance.initialize(config_name, num_users, concurrency)

        _ ->
          IO.puts("""

          Please run as:
            mix gen.test_data <config-name> <num-users>
          """)
      end
    end
  end
end
