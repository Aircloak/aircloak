defmodule DataQuality.Test.Present do
  @moduledoc "Produces and renders statistic about the query runs"

  alias Aircloak.AsciiTable
  alias DataQuality.Test.{Utility, Logger}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec mse(Test.results(), Test.global_results(), Test.config()) :: :ok
  @doc "Calculates the mean squared error for different sets of attributes and outputs the result to the screen"
  def mse(results, global_results, config) do
    Logger.banner("Data quality results")

    present_per_aggregate(results, config)
    present_mse_by_categories(global_results)
    present_mse_by_source(global_results)

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp present_per_aggregate(results, config) do
    joined_target_names = joined_target_names_from_config(config)

    for {aggregate, aggregate_results} <- results do
      Logger.header(aggregate)

      col_selectors = column_selectors_from_results(aggregate_results)

      col_headers = [
        "Distribution",
        "Dimension" | Enum.map(col_selectors, &(Utility.name(&1) <> " (mse)"))
      ]

      table_rows = Enum.reduce(aggregate_results, [], &process_distribution_results(&1, &2, col_selectors))

      IO.puts("Results are presented as: #{joined_target_names}.\n")
      IO.puts(AsciiTable.format([col_headers | table_rows]) <> "\n")
    end
  end

  defp process_distribution_results({distribution, distribution_results}, acc, col_selectors) do
    rows =
      distribution_results
      |> Enum.map(fn {dimension, aggregate_results} ->
        col_values =
          col_selectors
          |> Enum.map(fn col_selector ->
            data = aggregate_results[col_selector][:processed_data]

            data
            |> Map.keys()
            |> Enum.sort()
            |> Enum.map(&Map.get(data, &1)[:mse])
            |> Enum.join(" / ")
          end)

        [distribution, Utility.name(dimension) | col_values]
      end)

    rows ++ acc
  end

  defp joined_target_names_from_config(config),
    do:
      config
      |> Map.get(:anonymized)
      |> Enum.map(&Map.get(&1, :name))
      |> Enum.sort()
      |> Enum.join(" / ")

  defp column_selectors_from_results(results),
    do:
      results
      # This gets us the data of the first distribution
      |> Enum.at(0)
      |> elem(1)
      # This gets us the data of the first dimension (like bucket)
      |> Enum.at(0)
      |> elem(1)
      |> Map.keys()

  defp present_mse_by_categories(global_results) do
    Logger.header("MSE-values by category")

    table_rows =
      Enum.flat_map(global_results.mse_by_category, fn {grouping, grouping_values} ->
        Enum.map(grouping_values, fn {dimension, mse_values} ->
          [Utility.name(dimension), grouping | mse_values]
        end)
      end)

    col_headers = ["", "grouping" | global_results.sources]
    IO.puts(AsciiTable.format([col_headers | table_rows]) <> "\n")
  end

  defp present_mse_by_source(%{mse_by_source: mse_by_source}) do
    Logger.header("Global MSE-values")

    mse_by_source
    |> Enum.each(fn {source, mse} ->
      IO.puts(String.pad_trailing(source, 20, " ") <> to_string(mse))
    end)
  end
end
