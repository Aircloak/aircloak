defmodule DataQuality.Test.Processing do
  @moduledoc "SQL queries to run"

  alias DataQuality.Stats

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec calculate_mse(Test.results()) :: Test.results()
  @doc "Calculates the mean squared error per issued query"
  def calculate_mse(results), do: map_into(results, &process_distributions/1)

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
end
