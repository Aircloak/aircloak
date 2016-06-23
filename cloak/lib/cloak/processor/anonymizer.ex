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
    |> init_noise()
    |> process_low_count_users(query)
    |> aggregate_rows(query)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp aggregate_values("count", noise_generator, property_values) do
    {sum, _noise_generator} = Noise.sum(noise_generator, Enum.map(property_values, &length/1))
    round(sum)
  end
  defp aggregate_values("sum", noise_generator, property_values) do
    {sum, _noise_generator} = Noise.sum(noise_generator, Enum.map(property_values, &Enum.sum/1))
    Float.round(sum, 3)
  end
  defp aggregate_values("min", noise_generator, property_values) do
    {margin_average, _} = Noise.margin_average(noise_generator, Enum.map(property_values, &Enum.min/1), 1)
    Float.round(margin_average, 3)
  end
  defp aggregate_values("max", noise_generator, property_values) do
    {margin_average, _} = Noise.margin_average(noise_generator, Enum.map(property_values, &Enum.max/1), -1)
    Float.round(margin_average, 3)
  end
  defp aggregate_values("avg", noise_generator, values) do
    count = aggregate_values("count", noise_generator, values)
    sum = aggregate_values("sum", noise_generator, values)
    Float.round(sum / count, 3)
  end
  defp aggregate_values(unknown_aggregator, _, _) do
    raise "Aggregator '#{unknown_aggregator}' is not implemented yet!"
  end

  defp init_noise(grouped_rows) do
    for {property, users_values_map} <- grouped_rows,
      do: {property, Noise.new(users_values_map), users_values_map}
  end

  defp low_users_count?({_property, noise_generator, users_values_map}),
    do: low_users_count?(Enum.count(users_values_map), noise_generator)

  defp low_users_count?(count, noise_generator) do
    {passes_filter?, _} = Noise.passes_filter?(noise_generator, count)
    not passes_filter?
  end

  defp process_low_count_users(rows, query) do
    {low_count_rows, high_count_rows} = Enum.partition(rows, &low_users_count?/1)
    lcf_users_values_map = Enum.reduce(low_count_rows, %{},
      fn ({_property, _noise_generator, users_values_map}, accumulator) ->
        Map.merge(accumulator, users_values_map, fn (_user, values1, values2) -> values1 ++ values2 end)
      end)
    noise_generator = Noise.new(lcf_users_values_map)
    lcf_property = List.duplicate(:*, length(query.property))
    lcf_row = {lcf_property, noise_generator, lcf_users_values_map}
    case low_users_count?(lcf_row) do
      false -> [lcf_row | high_count_rows]
      true -> high_count_rows
    end
  end

  defp extract_user_values(user_values, index), do:
    for values <- user_values, value = Enum.at(values, index), value !== nil, do: value

  defp aggregate_row({property, noise_generator, user_values_map}, aggregated_columns, aggregators) do
    property_values = Map.values(user_values_map)
    aggregated_values = for {:function, function, column} <- aggregators,
      column_index = Enum.find_index(aggregated_columns, &(&1 === column))
    do
      input_values = for user_values <- property_values, do: extract_user_values(user_values, column_index)
      input_values = for values <- input_values, values !== [], do: values # drop users with no valid values
      case low_users_count?(length(input_values), noise_generator) do
        true -> nil
        false -> aggregate_values(function, noise_generator, input_values)
      end
    end
    {property, aggregated_values}
  end

  defp aggregate_rows(rows, query) do
    aggregated_columns = SqlQuery.aggregated_columns(query)
    for row <- rows, do: aggregate_row(row, aggregated_columns, query.aggregators)
  end
end
