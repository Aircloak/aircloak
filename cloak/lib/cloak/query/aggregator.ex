defmodule Cloak.Query.Aggregator do
  @moduledoc "This module aggregates the values into an anonymized result. See `aggregate/2` for details."
  alias Cloak.DataSource.Row
  alias Cloak.SqlQuery
  alias Cloak.Query.Anonymizer

  @typep property_values :: [Cloak.DataSource.field | :*]
  @typep user_id :: Cloak.DataSource.field
  @typep properties :: [{property_values, Anonymizer.t, %{user_id => [Row.t]}}]


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Transforms the non-anonymized rows returned from the database into an
  anonymized result. This is done in following steps:

  1. Rows are groupped per distinct property. A property is collection of
     selected columns, as well as columns listed in the `group by` clause.
     Additionally, inside each distinct property, rows are groupped per user.

  2. Distinct properties for which there are not enough distinct users are discarded.
     A low-count substitute property is generated for all such properties to indicate
     the amount of rows which are filtered out. This property is reported, but only
     if there are enough of users which are filtered out.

  3. Aggregation functions (e.g. `sum`, `count`) are computed for each distinct property.
     The resulting values are anonymized using the `Anonymizer` module.

  Each output row will consist of all property values together with
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
    |> group_by_property(query)
    |> process_low_count_users(query)
    |> aggregate_properties(query)
    |> normalize(query)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  @spec group_by_property([Row.t], SqlQuery.t) :: properties
  defp group_by_property(rows, query) do
    Enum.reduce(rows, %{}, fn(row, accumulator) ->
      property_values = for column <- query.property, do: Row.fetch!(row, column)
      user_id = user_id(row)
      Map.update(accumulator, property_values, %{user_id => [row]}, fn (user_values_map) ->
        Map.update(user_values_map, user_id, [row], &([row | &1]))
      end)
    end)
    |> init_anonymizer()
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

  @spec process_low_count_users(properties, SqlQuery.t) :: properties
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

  @spec aggregate_properties(properties, SqlQuery.t) :: [Row.t]
  defp aggregate_properties(properties, query) do
    Enum.map(properties, &aggregate_property(&1, query))
  end

  defp aggregate_property({property_values, anonymizer, users_rows}, query) do
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

  defp aggregate_by("distinct_count", anonymizer, aggregation_data) do
    aggregation_data
    |> user_counts_by_value()
    |> Enum.reject(fn({_value, count}) -> low_users_count?(count, anonymizer) end)
    |> Enum.count()
  end
  defp aggregate_by("count", anonymizer, aggregation_data), do: Anonymizer.count(anonymizer, aggregation_data)
  defp aggregate_by("sum", anonymizer, aggregation_data), do: Anonymizer.sum(anonymizer, aggregation_data)
  defp aggregate_by("min", anonymizer, aggregation_data), do: Anonymizer.min(anonymizer, aggregation_data)
  defp aggregate_by("max", anonymizer, aggregation_data), do: Anonymizer.max(anonymizer, aggregation_data)
  defp aggregate_by("avg", anonymizer, aggregation_data), do: Anonymizer.avg(anonymizer, aggregation_data)
  defp aggregate_by("stddev", anonymizer, aggregation_data), do: Anonymizer.stddev(anonymizer, aggregation_data)
  defp aggregate_by("median", anonymizer, aggregation_data), do: Anonymizer.median(anonymizer, aggregation_data)
  defp aggregate_by(unknown_aggregator, _, _) do
    raise "Aggregator '#{unknown_aggregator}' is not implemented yet!"
  end

  @spec user_counts_by_value([[any]]) :: %{any => pos_integer}
  defp user_counts_by_value(aggregation_data) do
    Enum.reduce(aggregation_data, %{}, fn(user_values, accumulator) ->
      Enum.reduce(Enum.uniq(user_values), accumulator, fn(value, accumulator) ->
        Map.update(accumulator, value, 1, &(&1 + 1))
      end)
    end)
  end

  @spec normalize([Row.t], SqlQuery.t) :: nonempty_list(Row.t)
  defp normalize([], query) do
    # If there are no results, we'll produce one row.
    # All values will be `nil`-ed except for `count` which will have
    # the value of 0.
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
