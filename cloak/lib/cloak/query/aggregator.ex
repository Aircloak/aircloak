defmodule Cloak.Query.Aggregator do
  @moduledoc "This module aggregates the values for a property in a query into an anonymized result."
  alias Cloak.DataSource.Row
  alias Cloak.SqlQuery
  alias Cloak.Query.Anonymizer

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Applies aggregation functions and produces aggregated rows.

  The resulting rows will consist of all query properties together with
  computed anonymized aggregates (count, sum, ...). For example, in the following
  query:

  ```
  select foo, count(*), avg(bar) from baz group by foo
  ```

  Each output row will consist of columns `foo`, `count(*)`, and `avg(bar)`.
  """
  @spec aggregate([Row.t], SqlQuery.t) :: [Row.t]
  def aggregate(rows, query) do
    rows
    |> group_by_property_and_users(query)
    |> init_anonymizer()
    |> process_low_count_users(query)
    |> aggregate_rows(query)
    |> normalize(query)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp group_by_property_and_users(rows, query) do
    Enum.reduce(rows, %{}, fn(row, accumulator) ->
      property = for column <- query.property, do: Row.fetch!(row, column)
      user = user_id(row)
      Map.update(accumulator, property, %{user => [row]}, fn (user_values_map) ->
        Map.update(user_values_map, user, [row], &([row | &1]))
      end)
    end)
  end

  defp user_id(row) do
    Row.fetch!(row, hd(row.columns))
  end

  defp init_anonymizer(grouped_rows) do
    for {property, users_rows} <- grouped_rows,
      do: {property, Anonymizer.new(users_rows), users_rows}
  end

  defp low_users_count?({_property, anonymizer, users_rows}),
    do: low_users_count?(users_rows, anonymizer)

  defp low_users_count?(count, anonymizer) when is_number(count) do
    {sufficiently_large?, _} = Anonymizer.sufficiently_large?(anonymizer, count)
    not sufficiently_large?
  end
  defp low_users_count?(values, anonymizer) do
    {sufficiently_large?, _} = Anonymizer.sufficiently_large?(anonymizer, Enum.count(values))
    not sufficiently_large?
  end

  defp process_low_count_users(rows, query) do
    {low_count_rows, high_count_rows} = Enum.partition(rows, &low_users_count?/1)
    lcf_users_rows = Enum.reduce(low_count_rows, %{},
      fn ({_property, _anonymizer, users_rows}, accumulator) ->
        Map.merge(accumulator, users_rows, fn (_user, values1, values2) -> values1 ++ values2 end)
      end)
    anonymizer = Anonymizer.new(lcf_users_rows)
    lcf_property = List.duplicate(:*, length(query.property))
    lcf_row = {lcf_property, anonymizer, lcf_users_rows}
    case low_users_count?(lcf_row) do
      false -> [lcf_row | high_count_rows]
      true -> high_count_rows
    end
  end

  defp aggregate_rows(rows, query) do
    Enum.map(rows, &aggregate_row(&1, query))
  end

  defp aggregate_row({property_values, anonymizer, users_rows}, query) do
    all_users_rows = Map.values(users_rows)

    aggregated_values =
      for {:function, function, column} <- query.aggregators do
        aggregation_data = aggregation_data(all_users_rows, column)

        case low_users_count?(aggregation_data, anonymizer) do
          true -> nil
          false -> aggregate_by(function, anonymizer, aggregation_data)
        end
      end

    make_row(query, property_values, aggregated_values)
  end

  defp aggregation_data(all_users_rows, column) do
    all_users_rows
    |> Enum.map(&values_for_aggregation(&1, column))
    |> Enum.filter(&(&1 != [])) # drop users with no values for aggregation
  end

  defp values_for_aggregation(rows, column) do
    rows
    |> Enum.map(&value(&1, column))
    |> Enum.filter(&(&1 != nil)) # `nil` values do not participate in the aggregation
  end

  defp value(_row, :*), do: :*
  defp value(row, column), do: Row.fetch!(row, column)

  defp aggregate_by("count", anonymizer, aggregation_data) do
    {count, _anonymizer} = Anonymizer.count(anonymizer, aggregation_data)
    count
  end
  defp aggregate_by("distinct_count", anonymizer, aggregation_data) do
    aggregation_data
    |> user_counts_by_value()
    |> Enum.reject(fn({_value, count}) -> low_users_count?(count, anonymizer) end)
    |> Enum.count()
  end
  defp aggregate_by("sum", anonymizer, aggregation_data) do
    {sum, _anonymizer} = Anonymizer.sum(anonymizer, aggregation_data)
    Float.round(sum, 3)
  end
  defp aggregate_by("min", anonymizer, aggregation_data) do
    {margin_average, _anonymizer} = Anonymizer.min(anonymizer, aggregation_data)
    Float.round(margin_average, 3)
  end
  defp aggregate_by("max", anonymizer, aggregation_data) do
    {margin_average, _anonymizer} = Anonymizer.max(anonymizer, aggregation_data)
    Float.round(margin_average, 3)
  end
  defp aggregate_by("avg", anonymizer, aggregation_data) do
    {avg, _anonymizer} = Anonymizer.avg(anonymizer, aggregation_data)
    Float.round(avg, 3)
  end
  defp aggregate_by(unknown_aggregator, _, _) do
    raise "Aggregator '#{unknown_aggregator}' is not implemented yet!"
  end

  defp user_counts_by_value(aggregation_data) do
    Enum.reduce(aggregation_data, %{}, fn(user_values, accumulator) ->
      Enum.reduce(Enum.uniq(user_values), accumulator, fn(value, accumulator) ->
        Map.update(accumulator, value, 1, &(&1 + 1))
      end)
    end)
  end

  defp normalize([], query) do
    property_values = Enum.map(query.property, fn(_) -> nil end)
    aggregated_values = Enum.map(query.aggregators, fn
      {_, "count", _} -> 0
      _ -> nil
    end)

    [make_row(query, property_values, aggregated_values)]
  end
  defp normalize([_|_] = rows, _query), do: rows

  defp make_row(query, property_values, aggregated_values) do
    Row.new(
      query.property ++ query.aggregators,
      property_values ++ aggregated_values
    )
  end
end
