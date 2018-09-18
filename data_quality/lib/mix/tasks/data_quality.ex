defmodule Mix.Tasks.DataQuality do
  @shortdoc "Runs data quality tests"
  @usage """
    Usage:

      mix data_quality --config <path.json> [--generate-data]

      Runs a data quality test against the configured Aircloak instance.
      The configured user must have access to the data source the config lists,
      and the data source must serve the data from the database the system is
      configured with.

      --config defaults to looking for a config.json in the same folder as the caller.

      The optional --generate-data flag will recreate the database schema
      and create data for the test.
  """

  @moduledoc "Runs data quality checks against an Aircloak system.\n\n" <> @usage

  alias DataQuality.{GenerateData, Test}

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  def run(args) do
    case OptionParser.parse(args, strict: [config: :string, generate_data: :boolean]) do
      {parameters, [], []} ->
        config = load_config(parameters)

        Application.ensure_all_started(:postgrex)

        if Keyword.get(parameters, :generate_data, false) do
          generate_data(config)
        end

        run_tests(config)

      _ ->
        cop_out()
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp generate_data(config), do: GenerateData.run(config)

  defp run_tests(config), do: Test.run(config)

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
