defmodule Cloak.Query.Aggregator do
  @moduledoc "This module aggregates the values into an anonymized result. See `aggregate/2` for details."
  alias Cloak.DataSource
  alias Cloak.SqlQuery
  alias Cloak.Query.Anonymizer

  @typep property_values :: [DataSource.field | :*]
  @typep user_id :: DataSource.field
  @typep properties :: [{property_values, Anonymizer.t, %{user_id => DataSource.row}}]
  @type bucket :: %{row: [DataSource.field], occurrences: pos_integer}


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
  @spec aggregate([DataSource.row], [DataSource.column], SqlQuery.t) :: [bucket]
  def aggregate(rows, columns, query) do
    rows
    |> group_by_property(columns, query)
    |> process_low_count_users(query)
    |> aggregate_properties(columns, query)
    |> make_buckets(query)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  @spec group_by_property([DataSource.row], [DataSource.column], SqlQuery.t) :: properties
  defp group_by_property(rows, columns, query) do
    rows
    |> Enum.reduce(%{}, fn(row, accumulator) ->
      property = grouping_property(row, columns, query)
      user_id = user_id(row)
      Map.update(accumulator, property, %{user_id => [row]}, fn (user_values_map) ->
        Map.update(user_values_map, user_id, [row], fn (rows) -> [row | rows] end)
      end)
    end)
    |> init_anonymizer()
  end

  defp grouping_property(row, columns, query) do
    query_properties = Enum.map(query.property, &SqlQuery.full_column_name/1)
    Enum.map(query_properties, &DataSource.fetch_value!(row, columns, &1))
  end

  defp user_id([user_id | _rest]), do: user_id

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

  @spec aggregate_properties(properties, [DataSource.column], SqlQuery.t) :: [DataSource.row]
  defp aggregate_properties(properties, columns, query) do
    Enum.map(properties, &aggregate_property(&1, columns, query))
  end

  defp aggregate_property({property_values, anonymizer, users_rows}, columns, query) do
    all_users_rows = Map.values(users_rows)

    aggregated_values =
      for {:function, function, column} <- query.aggregators do
        values_for_aggregation = extract_values(all_users_rows, columns, column)

        case low_users_count?(values_for_aggregation, anonymizer) do
          true  -> nil
          false ->
            values_for_aggregation
            |> preprocess_for_aggregation(column)
            |> aggregate_by(function, anonymizer)
        end
      end

    property_values ++ aggregated_values
  end

  # See docs/anonymization.md for details
  defp preprocess_for_aggregation(values, {:distinct, _column}) do
    values
    |> Enum.sort_by(&Enum.count/1)
    |> Stream.with_index()
    |> Stream.flat_map(fn ({row, index}) ->
      Enum.map(row, &{index, &1})
    end)
    |> Stream.uniq(fn ({_index, value}) -> value end)
    |> Enum.reduce(%{}, fn({index, value}, accumulator) ->
      Map.update(accumulator, index, [value], fn (values) -> [value | values] end)
    end)
    |> Map.values()
  end
  defp preprocess_for_aggregation(values, _column), do: values

  defp extract_values(rows, columns, column) do
    column = SqlQuery.full_column_name(column)
    rows
    |> Enum.map(fn(user_rows) ->
      user_rows
      |> Enum.map(fn(row) -> value(row, columns, column) end)
      |> Enum.reject(&(&1 === nil))
    end)
    |> Enum.reject(&Enum.empty?/1)
  end

  defp value(_row, _columns, :*), do: :*
  defp value(row, columns, {:distinct, column}), do: value(row, columns, column)
  defp value(row, columns, column), do: DataSource.fetch_value!(row, columns, column)

  defp aggregate_by(aggregation_data, "count", anonymizer), do: Anonymizer.count(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "sum", anonymizer), do: Anonymizer.sum(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "min", anonymizer), do: Anonymizer.min(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "max", anonymizer), do: Anonymizer.max(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "avg", anonymizer), do: Anonymizer.avg(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "stddev", anonymizer), do: Anonymizer.stddev(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "median", anonymizer), do: Anonymizer.median(anonymizer, aggregation_data)
  defp aggregate_by(_, unknown_aggregator, _) do
    raise "Aggregator '#{unknown_aggregator}' is not implemented yet!"
  end

  defp make_buckets([], %{property: []} = query) do
    # If there are no results for a global aggregation, we'll produce one row.
    # All results will be `nil`-ed except for `count` which will have the value of 0.
    aggregated_values = Enum.map(query.aggregators, fn
      {_, "count", _} -> 0
      _ -> nil
    end)
    [%{row: aggregated_values, occurrences: 1}]
  end
  defp make_buckets(rows, query) do
    columns = query.property ++ query.aggregators
    Enum.map(rows, &%{row: selected_values(&1, columns, query), occurrences: occurrences(&1, columns, query)})
  end

  defp selected_values(row, columns, query), do:
    for selected_column <- query.columns, do: DataSource.fetch_value!(row, columns, selected_column)

  defp occurrences(row, columns, %{implicit_count: true}), do:
    DataSource.fetch_value!(row, columns, {:function, "count", :*})
  defp occurrences(_row, _columns, _query), do: 1
end
