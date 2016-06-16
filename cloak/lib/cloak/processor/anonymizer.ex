defmodule Cloak.Processor.Anonymizer do
  @moduledoc "This module aggregates the values for a property in a query into an anonymized result."
  import Cloak.Type
  alias Cloak.SqlQuery
  alias Cloak.Processor.Noise

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Aggregates the grouped rows into anonymized buckets by applying the selected aggregation function."
  @spec aggregate(GroupedRows.t, SqlQuery.t) :: [Bucket.t]
  def aggregate(rows, query) do
    rows
    |> seed_rows()
    |> process_low_count_users(query)
    |> aggregate_rows(query)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp count_aggregator(nil, acc), do: acc
  defp count_aggregator(_value, acc), do: acc + 1

  defp sum_aggregator(nil, acc), do: acc
  defp sum_aggregator(value, nil), do: value
  defp sum_aggregator(value, acc), do: acc + value

  defp min_aggregator(nil, acc), do: acc
  defp min_aggregator(value, nil), do: value
  defp min_aggregator(value, acc), do: min(acc, value)

  defp max_aggregator(nil, acc), do: acc
  defp max_aggregator(value, nil), do: value
  defp max_aggregator(value, acc), do: max(acc, value)

  defp aggregate_values("count", _users, values) do
    Enum.reduce(values, 0, &count_aggregator/2)
  end
  defp aggregate_values("sum", _users, values) do
    Enum.reduce(values, nil, &sum_aggregator/2)
  end
  defp aggregate_values("min", _users, values) do
    Enum.reduce(values, nil, &min_aggregator/2)
  end
  defp aggregate_values("max", _users, values) do
    Enum.reduce(values, nil, &max_aggregator/2)
  end
  defp aggregate_values("avg", users, values) do
    case aggregate_values("count", users, values) do
      0 -> nil
      count -> aggregate_values("sum", users, values) / count
    end
  end
  defp aggregate_values(unknown_aggregator, _, _) do
    raise "Aggregator '#{unknown_aggregator}' is not implemented yet!"
  end

  defp seed_rows(grouped_rows) do
    for {property, users_values_map} <- grouped_rows,
      do: {property, Noise.random_seed_from_unique_users(Map.keys(users_values_map)), users_values_map}
  end

  defp low_users_count?({_property, seed, users_values_map}),
    do: not Noise.passes_filter?(Enum.count(users_values_map), seed)

  defp process_low_count_users(rows, query) do
    {low_count_rows, high_count_rows} = Enum.partition(rows, &low_users_count?/1)
    lcf_users_values_map = Enum.reduce(low_count_rows, %{},
      fn ({_property, _seed, users_values_map}, accumulator) ->
        Map.merge(accumulator, users_values_map, fn (_user, values1, values2) -> values1 ++ values2 end)
      end)
    lcf_seed = Noise.random_seed_from_unique_users(Map.keys(lcf_users_values_map))
    lcf_property = List.duplicate(:*, length(query.property))
    lcf_row = {lcf_property, lcf_seed, lcf_users_values_map}
    case low_users_count?(lcf_row) do
      false -> [lcf_row | high_count_rows]
      true -> high_count_rows
    end
  end

  defp aggregate_row({property, _seed, user_values_map}, aggregated_columns, aggregators) do
    users = Map.keys(user_values_map)
    property_values = Map.values(user_values_map)
    aggregated_values = for {:function, function, column} <- aggregators,
      column_index = Enum.find_index(aggregated_columns, &(&1 === column))
    do
      input_values = for user_values <- property_values, do: Enum.map(user_values, &Enum.at(&1, column_index))
      aggregate_values(function, users, List.flatten(input_values))
    end
    {property, aggregated_values}
  end

  defp aggregate_rows(rows, query) do
    aggregated_columns = SqlQuery.aggregated_columns(query)
    for row <- rows, do: aggregate_row(row, aggregated_columns, query.aggregators)
  end
end
