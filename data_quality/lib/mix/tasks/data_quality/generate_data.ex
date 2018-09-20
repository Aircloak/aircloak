defmodule Mix.Tasks.DataQuality.GenerateData do
  @shortdoc "Generates data for the data quality test"
  @usage """
    Usage:

      mix data_quality.generate_data --config <path.json>

      --config defaults to looking for a config.json in the same folder as the caller.

    When run it will (re)create a `data_quality` table and populate it with the data for the
    data quality test. No tests will be run.

    Requirements:
    - there must be a `config.json` file stored in the directory from which the command is run
    - it must configure access to a `data_quality` database for a user `data_quality`

    Consult the README.md of the data_quality project for information on the configuration.
  """

  @moduledoc "Runs data quality checks against an Aircloak system.\n\n" <> @usage

  alias DataQuality.GenerateData

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  def run(args) do
    case OptionParser.parse(args, strict: [config: :string, generate_data: :boolean]) do
      {parameters, [], []} ->
        Application.ensure_all_started(:postgrex)

        parameters
        |> load_config()
        |> GenerateData.run()

      _ ->
        cop_out()
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp load_config(parameters) do
    path = parameters[:config] || "config.json"

    case File.read(path) do
      {:ok, data} ->
        data
        |> Poison.decode!()
        |> Aircloak.atomize_keys()

      {:error, reason} ->
        cop_out("Invalid usage - could not read config: #{Aircloak.File.humanize_posix_error(reason)}")
    end
  end

  defp cop_out(reason \\ "Invalid usage") do
    IO.puts(@usage)

    Mix.raise(reason)
  end
end
