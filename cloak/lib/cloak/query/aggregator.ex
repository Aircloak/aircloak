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
    |> aggregate_properties(query)
    |> make_buckets(query)
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  # We store values for aggregation in a column-oriented format for easier retrival.
  defp add_values_to_columns([], []), do: []
  defp add_values_to_columns([[] | rest_values], [prev_values | rest_prev_values]) do
    [prev_values | add_values_to_columns(rest_values, rest_prev_values)]
  end
  defp add_values_to_columns([[value] | rest_values], [prev_values | rest_prev_values]) do
    [[value | prev_values] | add_values_to_columns(rest_values, rest_prev_values)]
  end

  defp column_index(:*, _columns), do: :*
  defp column_index({:distinct, column}, columns), do: column_index(column, columns)
  defp column_index(column, columns) do
    name = SqlQuery.full_column_name(column)
    case Enum.find_index(columns, &(&1 === name)) do
      nil -> raise(Cloak.Query.Runner.RuntimeError, "Column `#{name}` doesn't exist in selected columns.")
      index -> index
    end
  end

  defp index_to_value_list(:*, _row), do: [:*]
  defp index_to_value_list(index, row) do
    case Enum.at(row, index) do
      nil -> []
      value -> [value]
    end
  end

  defp group_by_property(rows, columns, query) do
    property_indices = for column <- query.property, do: column_index(column, columns)
    aggregators_indices = for column <- SqlQuery.aggregated_columns(query), do: column_index(column, columns)
    rows
    |> Enum.reduce(%{}, fn(row, accumulator) ->
      property = for index <- property_indices, do: Enum.at(row, index)
      user_id = user_id(row)
      values = for index <- aggregators_indices, do: index_to_value_list(index, row)
      Map.update(accumulator, property, %{user_id => values}, fn (user_values_map) ->
        Map.update(user_values_map, user_id, values, &add_values_to_columns(values, &1))
      end)
    end)
    |> init_anonymizer()
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

  @spec aggregate_properties(properties, SqlQuery.t) :: [DataSource.row]
  defp aggregate_properties(properties, query) do
    Enum.map(properties, &aggregate_property(&1, query))
  end

  defp aggregate_property({property_values, anonymizer, users_rows}, query) do
    all_users_rows = Map.values(users_rows)
    aggregated_columns = SqlQuery.aggregated_columns(query)

    aggregation_results = for {:function, function, column}  <- query.aggregators do
      values_index = Enum.find_index(aggregated_columns, &column == &1)
      aggregated_values = all_users_rows |> Stream.map(&Enum.at(&1, values_index)) |> Stream.reject(&[] === &1)
      case low_users_count?(aggregated_values, anonymizer) do
        true  -> nil
        false -> aggregated_values |> preprocess_for_aggregation(column) |> aggregate_by(function, anonymizer)
      end
    end

    property_values ++ aggregation_results
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
