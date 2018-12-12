defmodule Mix.Tasks.Gen.TestData do
  if Mix.env() in [:test, :dev] do
    @shortdoc "Generates an interlinked dataset for cloak testing."
    @moduledoc """
    Generates a test dataset of users and items related to these users.
    For more information, please consult the `Compliance.TableDefinitions` module.

    The command takes two parameters:
    - the name of the data source config file to be used
    - the number of users to be generated

    Additionally you may specify the following options:

    --concurrency - specifies the number of concurrent insertion processes. Defaults to number of schedulers.
    --seed - specifies the random seed to use when generating the data. The seed used is printed for future reference.

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

      case OptionParser.parse!(args, switches: [seed: :string, concurrency: :integer]) do
        {options, [config_name, num_users]} ->
          seed =
            case Keyword.fetch(options, :seed) do
              {:ok, seed} ->
                seed

              :error ->
                :rand.seed(:exrop)
                :rand.export_seed() |> :erlang.term_to_binary() |> Base.encode64()
            end

          concurrency = Keyword.get(options, :concurrency, System.schedulers_online())
          num_users = String.to_integer(num_users)
          IO.puts("Generating data for #{num_users} users. Using seed #{inspect(seed)}.")

          seed |> Base.decode64!() |> :erlang.binary_to_term() |> :rand.seed()
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
