defmodule Cloak.Processor.Aggregator do
  @moduledoc "This module aggregates the values for a property in a query into an anonymized result."
  import Cloak.Type
  alias Cloak.SqlQuery
  alias Cloak.Processor.Anonymizer

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Aggregates the grouped rows into anonymized buckets by applying the selected aggregation function."
  @spec aggregate(GroupedRows.t, SqlQuery.t) :: [Bucket.t]
  def aggregate(rows, query) do
    rows
    |> init_anonymizer()
    |> process_low_count_users(query)
    |> aggregate_rows(query)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp aggregate_values("count", anonymizer, property_values) do
    {sum, _anonymizer} = Anonymizer.sum(anonymizer, Enum.map(property_values, &length/1))
    max(round(sum), Anonymizer.count_absolute_lower_bound())
  end
  defp aggregate_values("sum", anonymizer, property_values) do
    {sum, _anonymizer} = Anonymizer.sum(anonymizer, Enum.map(property_values, &Enum.sum/1))
    Float.round(sum, 3)
  end
  defp aggregate_values("min", anonymizer, property_values) do
    {margin_average, _} = Anonymizer.top_margin_average(anonymizer, Enum.map(property_values, &Enum.min/1))
    Float.round(margin_average, 3)
  end
  defp aggregate_values("max", anonymizer, property_values) do
    {margin_average, _} = Anonymizer.bottom_margin_average(anonymizer, Enum.map(property_values, &Enum.max/1))
    Float.round(margin_average, 3)
  end
  defp aggregate_values("avg", anonymizer, values) do
    count = aggregate_values("count", anonymizer, values)
    sum = aggregate_values("sum", anonymizer, values)
    Float.round(sum / count, 3)
  end
  defp aggregate_values(unknown_aggregator, _, _) do
    raise "Aggregator '#{unknown_aggregator}' is not implemented yet!"
  end

  defp init_anonymizer(grouped_rows) do
    for {property, users_values_map} <- grouped_rows,
      do: {property, Anonymizer.new(users_values_map), users_values_map}
  end

  defp low_users_count?({_property, anonymizer, users_values_map}),
    do: low_users_count?(users_values_map, anonymizer)

  defp low_users_count?(values, anonymizer) do
    {sufficiently_large?, _} = Anonymizer.sufficiently_large?(anonymizer, values)
    not sufficiently_large?
  end

  defp process_low_count_users(rows, query) do
    {low_count_rows, high_count_rows} = Enum.partition(rows, &low_users_count?/1)
    lcf_users_values_map = Enum.reduce(low_count_rows, %{},
      fn ({_property, _anonymizer, users_values_map}, accumulator) ->
        Map.merge(accumulator, users_values_map, fn (_user, values1, values2) -> values1 ++ values2 end)
      end)
    anonymizer = Anonymizer.new(lcf_users_values_map)
    lcf_property = List.duplicate(:*, length(query.property))
    lcf_row = {lcf_property, anonymizer, lcf_users_values_map}
    case low_users_count?(lcf_row) do
      false -> [lcf_row | high_count_rows]
      true -> high_count_rows
    end
  end

  defp extract_user_values(user_values, index), do:
    for values <- user_values, value = Enum.at(values, index), value !== nil, do: value

  defp aggregate_row({property, anonymizer, user_values_map}, aggregated_columns, aggregators) do
    property_values = Map.values(user_values_map)
    aggregated_values = for {:function, function, column} <- aggregators,
      column_index = Enum.find_index(aggregated_columns, &(&1 === column))
    do
      input_values = for user_values <- property_values, do: extract_user_values(user_values, column_index)
      input_values = for values <- input_values, values !== [], do: values # drop users with no valid values
      case low_users_count?(input_values, anonymizer) do
        true -> nil
        false -> aggregate_values(function, anonymizer, input_values)
      end
    end
    {property, aggregated_values}
  end

  defp aggregate_rows(rows, query) do
    aggregated_columns = SqlQuery.aggregated_columns(query)
    for row <- rows, do: aggregate_row(row, aggregated_columns, query.aggregators)
  end
end
