defmodule Cloak.Processor.Anonymizer do
  @moduledoc "This module aggregates the values for a property in a query into an anonymized result."
  import Cloak.Type
  alias Cloak.SqlQuery

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Aggregates the grouped rows into anonymized buckets by applying the selected aggregation function."
  @spec aggregate([GroupedRow.t], SqlQuery.t) :: [Bucket.t]
  def aggregate(grouped_rows, query) do
    for {property, {users, columns}} <- grouped_rows do
      aggregated_values = for {{:function, function, _}, column} <- Enum.zip(query.aggregators, columns),
        do: aggregate_values(function, users, column)
      {property, aggregated_values}
    end
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
end
