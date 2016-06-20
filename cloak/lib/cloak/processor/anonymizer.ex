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

  # Computes the average value for the margin of a collection.
  defp margin_average([], _margin_count), do: 0
  defp margin_average(values, margin_count) do
    margin = Enum.take(values, margin_count)
    Enum.sum(margin) / length(margin)
  end

  # Drops the specified numbers of outliers from a sorted collection of values.
  defp drop_outliers(values, outlier_count) do
    new_length = length(values) - 2 * outlier_count
    true = new_length > 0 # assert we have enough values to remove
    Enum.slice(values, outlier_count, new_length)
  end

  # Computes the anonymized sum of a collection of values.
  defp anonymized_sum(values, seed) do
    values = Enum.sort(values)

    Noise.set_seed(seed)

    outlier_count = Noise.config(:dropped_outliers_count)
    values = drop_outliers(values, outlier_count)

    margin_count_mean = Noise.config(:margin_count_mean)
    margin_count_sigma = Noise.config(:margin_count_sigma)
    margin_count = Noise.get(margin_count_sigma, margin_count_mean) |> round()
    top_average = margin_average(values, margin_count)
    bottom_average = margin_average(values, -margin_count)

    sum_noise_sigma = Noise.config(:sum_noise_sigma)
    noise = Noise.get(sum_noise_sigma, outlier_count)

    noise * (top_average + bottom_average) + Enum.sum(values)
  end

  # Computes the anonymized average for one of the ends of a collection of values.
  defp anonymized_margin(values, seed, margin_sign) do
    values = Enum.sort(values)

    Noise.set_seed(seed)

    outlier_count = Noise.config(:dropped_outliers_count)
    values = drop_outliers(values, outlier_count)

    margin_count_mean = Noise.config(:margin_count_mean)
    margin_count_sigma = Noise.config(:margin_count_sigma)
    margin_count = Noise.get(margin_count_sigma, margin_count_mean) |> round()

    margin_average(values, margin_sign * margin_count)
  end

  defp aggregate_values("count", seed, property_values) do
    (for user_values <- property_values, do: length(user_values))
    |> anonymized_sum(seed)
    |> round()
  end
  defp aggregate_values("sum", seed, property_values) do
    (for user_values <- property_values, do: Enum.sum(user_values))
    |> anonymized_sum(seed)
    |> Float.round(3)
  end
  defp aggregate_values("min", seed, property_values) do
    (for user_values <- property_values, do: Enum.min(user_values))
    |> anonymized_margin(seed, 1)
    |> Float.round(3)
  end
  defp aggregate_values("max", seed, property_values) do
    (for user_values <- property_values, do: Enum.max(user_values))
    |> anonymized_margin(seed, -1)
    |> Float.round(3)
  end
  defp aggregate_values("avg", seed, values) do
    count = aggregate_values("count", seed, values)
    sum = aggregate_values("sum", seed, values)
    Float.round(sum / count, 3)
  end
  defp aggregate_values(unknown_aggregator, _, _) do
    raise "Aggregator '#{unknown_aggregator}' is not implemented yet!"
  end

  defp seed_rows(grouped_rows) do
    for {property, users_values_map} <- grouped_rows,
      do: {property, Noise.make_seed(users_values_map), users_values_map}
  end

  defp low_users_count?({_property, seed, users_values_map}),
    do: low_users_count?(Enum.count(users_values_map), seed)

  defp low_users_count?(count, seed),
    do: not Noise.passes_filter?(count, seed)

  defp process_low_count_users(rows, query) do
    {low_count_rows, high_count_rows} = Enum.partition(rows, &low_users_count?/1)
    lcf_users_values_map = Enum.reduce(low_count_rows, %{},
      fn ({_property, _seed, users_values_map}, accumulator) ->
        Map.merge(accumulator, users_values_map, fn (_user, values1, values2) -> values1 ++ values2 end)
      end)
    lcf_seed = Noise.make_seed(lcf_users_values_map)
    lcf_property = List.duplicate(:*, length(query.property))
    lcf_row = {lcf_property, lcf_seed, lcf_users_values_map}
    case low_users_count?(lcf_row) do
      false -> [lcf_row | high_count_rows]
      true -> high_count_rows
    end
  end

  defp extract_user_values(user_values, index), do:
    for values <- user_values, value = Enum.at(values, index), value !== nil, do: value

  defp aggregate_row({property, seed, user_values_map}, aggregated_columns, aggregators) do
    property_values = Map.values(user_values_map)
    aggregated_values = for {:function, function, column} <- aggregators,
      column_index = Enum.find_index(aggregated_columns, &(&1 === column))
    do
      input_values = for user_values <- property_values,
        values = extract_user_values(user_values, column_index),
        values !== [], do: values
      case low_users_count?(length(input_values), seed) do
        true -> nil
        false -> aggregate_values(function, seed, input_values)
      end
    end
    {property, aggregated_values}
  end

  defp aggregate_rows(rows, query) do
    aggregated_columns = SqlQuery.aggregated_columns(query)
    for row <- rows, do: aggregate_row(row, aggregated_columns, query.aggregators)
  end
end
