defmodule DataQuality.Test.Processing do
  @moduledoc "Calculating MSE-values for the all the queries that have been run"

  alias DataQuality.Stats

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec calculate_mse(Test.results()) :: Test.results()
  @doc "Calculates the mean squared error per issued query"
  def calculate_mse(results), do: map_into(results, &process_distributions/1)

  @spec calculate_global_mse(Test.results()) :: Test.global_results()
  @doc """
  Calculates the mean squared error across different dimensions in the result set.
  For example the MSE across all min or max queries, or all queries for a data source.
  """
  def calculate_global_mse(results) do
    flattened_values = Enum.flat_map(results, &extract_values_by_class/1)

    %{
      sources: sources(flattened_values),
      mse_by_category: mse_by_category([:distribution, :dimension, :class], flattened_values),
      mse_by_source: mse_by_source(flattened_values)
    }
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp process_distributions(results), do: map_into(results, &process_dimensions/1)

  defp process_dimensions(results), do: map_into(results, &process_aggregates/1)

  defp process_aggregates(aggregates), do: map_into(aggregates, &process_aggregate/1)

  defp map_into(data, function),
    do:
      data
      |> Enum.map(fn {key, values} -> {key, function.(values)} end)
      |> Enum.reject(&(&1 |> elem(1) |> is_nil()))
      |> Enum.into(%{})

  defp process_aggregate(%{raw_data: []}), do: nil

  defp process_aggregate(results) do
    raw_data = results[:raw_data]
    all_query_targets = raw_data |> hd() |> Map.keys()
    test_targets = all_query_targets -- ["unanonymized"]

    processed_results =
      for target <- test_targets, into: %{} do
        value_pairs =
          raw_data
          |> Enum.map(fn row ->
            real_value = row["unanonymized"]
            test_value = row[target]
            {real_value, test_value}
          end)

        {target, %{mse: Stats.mean_squared_error(value_pairs)}}
      end

    Map.put(results, :processed_data, processed_results)
  end

  defp sources(flattened_values),
    do:
      flattened_values
      |> Enum.map(& &1[:source])
      |> Enum.uniq()
      |> Enum.sort()

  defp mse_by_category(categories, flattened_values),
    do:
      categories
      |> Enum.map(fn grouping ->
        result =
          grouped_mse(flattened_values, grouping)
          |> Enum.map(fn {group, group_values} ->
            mse_values =
              group_values
              |> Enum.sort_by(&elem(&1, 0))
              |> Enum.map(&elem(&1, 1))

            {group, mse_values}
          end)
          |> Enum.into(%{})

        {grouping, result}
      end)
      |> Enum.into(%{})

  defp grouped_mse(data, group),
    do:
      data
      |> Enum.group_by(& &1[group])
      |> Enum.map(fn {group_name, group_values} ->
        {group_name, mse_by_source(group_values)}
      end)
      |> Enum.into(%{})

  defp mse_by_source(values),
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
end
