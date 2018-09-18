defmodule DataQuality.Test.Present do
  @moduledoc "Produces and renders statistic about the query runs"

  alias Aircloak.AsciiTable
  alias DataQuality.Stats
  alias DataQuality.Test.{Utility, Logger}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec mse(Test.results(), Map.t()) :: Test.results()
  @doc "Calculates the mean squared error for different sets of attributes and outputs the result to the screen"
  def mse(results, config) do
    Logger.banner("Data quality results")
    per_aggregate(results, config)
    output_global_mse(results)

    results
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp per_aggregate(results, config) do
    joined_target_names = joined_target_names_from_config(config)

    for {aggregate, aggregate_results} <- results do
      Logger.header(aggregate)

      col_selectors = column_selectors_from_results(aggregate_results)

      col_headers = [
        "Distribution",
        "Dimension" | Enum.map(col_selectors, &(Utility.name(&1) <> " (mse)"))
      ]

      IO.puts("Results are presented as: #{joined_target_names}.\n")

      table_rows =
        aggregate_results
        |> Enum.reduce([], fn {distribution, distribution_results}, acc ->
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
        end)

      IO.puts(AsciiTable.format([col_headers | table_rows]) <> "\n")
    end
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

  defp output_global_mse(results) do
    flattened_values = Enum.flat_map(results, &extract_values_by_class/1)
    mse_by_categories([:distribution, :dimension, :class], flattened_values)
    mse_by_source(flattened_values)
  end

  def extract_values_by_class({class, class_values}),
    do:
      class_values
      |> Enum.flat_map(&extract_values_by_distribution/1)
      |> Enum.map(&Map.put(&1, :class, class))

  def extract_values_by_distribution({distribution, distribution_values}),
    do:
      distribution_values
      |> Enum.flat_map(&extract_values_by_dimension/1)
      |> Enum.map(&Map.put(&1, :distribution, distribution))

  def extract_values_by_dimension({dimension, dimension_values}),
    do:
      dimension_values
      |> Enum.flat_map(&extract_values_by_aggregate/1)
      |> Enum.map(&Map.put(&1, :dimension, dimension))

  def extract_values_by_aggregate({aggregate, aggregate_values}),
    do:
      aggregate_values[:raw_data]
      |> Enum.flat_map(fn result_row ->
        all_query_targets = aggregate_values[:raw_data] |> hd() |> Map.keys()
        test_targets = all_query_targets -- ["unanonymized"]

        Enum.map(
          test_targets,
          &%{source: &1, value: {result_row["unanonymized"], result_row[&1]}}
        )
      end)
      |> Enum.map(&Map.put(&1, :aggregate, aggregate))

  defp mse_by_categories(categories, flattened_values) do
    Logger.header("MSE-values by category")

    table_rows =
      categories
      |> Enum.flat_map(fn grouping ->
        grouped_mse(flattened_values, grouping)
        |> Enum.map(fn {group, group_values} ->
          mse_values =
            group_values
            |> Enum.sort_by(&elem(&1, 0))
            |> Enum.map(&elem(&1, 1))

          [Utility.name(group), grouping | mse_values]
        end)
      end)

    sources =
      flattened_values
      |> Enum.map(& &1[:source])
      |> Enum.uniq()
      |> Enum.sort()

    col_headers = ["", "grouping" | sources]
    IO.puts(AsciiTable.format([col_headers | table_rows]) <> "\n")
  end

  defp mse_by_source(flattened_values) do
    Logger.header("Global MSE-values")

    flattened_values
    |> mse_from_flattened_values_by_source()
    |> Enum.map(fn {source, mse} ->
      IO.puts(String.pad_trailing(source, 20, " ") <> to_string(mse))
    end)
  end

  defp grouped_mse(data, group),
    do:
      data
      |> Enum.group_by(& &1[group])
      |> Enum.map(fn {group_name, group_values} ->
        {group_name, mse_from_flattened_values_by_source(group_values)}
      end)
      |> Enum.into(%{})

  defp mse_from_flattened_values_by_source(values),
    do:
      values
      |> Enum.group_by(& &1[:source])
      |> Enum.map(fn {source, source_values} ->
        mse =
          source_values
          |> Enum.map(& &1[:value])
          |> Stats.mean_squared_error()

        {source, mse}
      end)
      |> Enum.into(%{})
end
