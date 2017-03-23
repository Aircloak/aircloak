defmodule Cloak.Query.Aggregator do
  @moduledoc "This module aggregates the values into an anonymized result. See `aggregate/2` for details."

  require Logger

  alias Cloak.DataSource
  alias Cloak.Sql.{Query, Expression}
  alias Cloak.Query.{Anonymizer, Rows, Result}
  alias Cloak.Query.Runner.Engine

  @typep property_values :: [DataSource.field | :*]
  @typep user_id :: DataSource.field
  @typep properties :: [{property_values, Anonymizer.t, %{user_id => DataSource.row}}]


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
  @spec aggregate(Enumerable.t, Query.t, Engine.state_updater) :: Result.t
  def aggregate(rows, query, state_updater) do
    rows_by_property = group_by_property(rows, query, state_updater)
    users_count = number_of_anonymized_users(rows_by_property)
    aggregated_buckets = rows_by_property
      |> process_low_count_users(query)
      |> aggregate_properties(query)
      |> make_buckets(query)

    Result.new(query, aggregated_buckets, users_count)
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

  defp group_by_property(rows, query, state_updater) do
    Logger.debug("Grouping rows ...")
    aggregated_columns = Query.aggregated_columns(query)
    rows
    |> Enum.reduce(%{}, fn(row, accumulator) ->
      if Map.size(accumulator) == 0, do: state_updater.(:ingesting_data)
      group_row(accumulator, row, query.property, aggregated_columns)
    end)
    |> fn(rows) -> state_updater.(:processing); rows end.()
    |> init_anonymizer()
  end

  defp aggregated_value_list(_row, :*), do: [:*]
  defp aggregated_value_list(row, {:distinct, column}), do: aggregated_value_list(row, column)
  defp aggregated_value_list(row, column), do: List.wrap(Expression.value(column, row))

  defp group_row(accumulator, row, property_columns, aggregated_columns) do
    property = for column <- property_columns, do: Expression.value(column, row)
    user_id = user_id(row)
    values = for column <- aggregated_columns, do: aggregated_value_list(row, column)
    Map.update(accumulator, property, %{user_id => values}, fn (user_values_map) ->
      Map.update(user_values_map, user_id, values, &add_values_to_columns(values, &1))
    end)
  end

  defp user_id([user_id | _rest]), do: user_id

  defp init_anonymizer(grouped_rows), do:
    for {property, users_rows} <- grouped_rows, do:
      {property, Anonymizer.new(users_rows), users_rows}

  defp low_users_count?({_property, anonymizer, users_rows}), do:
    low_users_count?(users_rows, anonymizer)

  defp low_users_count?(count, anonymizer) when is_integer(count) do
    {sufficiently_large?, _} = Anonymizer.sufficiently_large?(anonymizer, count)
    not sufficiently_large?
  end
  defp low_users_count?(values, anonymizer), do:
    values |> Enum.count() |> low_users_count?(anonymizer)

  @spec process_low_count_users(properties, Query.t) :: properties
  defp process_low_count_users(rows, query) do
    Logger.debug("Processing low count users ...")
    {low_count_rows, high_count_rows} = Enum.partition(rows, &low_users_count?/1)
    lcf_users_rows = Enum.reduce(low_count_rows, %{},
      fn ({_property, _anonymizer, users_rows}, accumulator) ->
        Map.merge(accumulator, users_rows, fn (_user, columns1, columns2) ->
          for {values1, values2} <- Enum.zip(columns1, columns2), do: values1 ++ values2
        end)
      end)
    anonymizer = Anonymizer.new(lcf_users_rows)
    lcf_property = List.duplicate(:*, length(query.property))
    lcf_row = {lcf_property, anonymizer, lcf_users_rows}
    case low_users_count?(lcf_row) do
      false -> [lcf_row | high_count_rows]
      true -> high_count_rows
    end
  end

  @spec aggregate_properties(properties, Query.t) :: [DataSource.row]
  defp aggregate_properties(properties, query) do
    Logger.debug("Aggregating properties ...")
    Enum.map(properties, &aggregate_property(&1, query))
  end

  defp aggregate_property({property_values, anonymizer, users_rows}, query) do
    aggregated_columns = Query.aggregated_columns(query)

    aggregation_results = for %Expression{function: function, function_args: [column]}  <- query.aggregators do
      values_index = Enum.find_index(aggregated_columns, &column == &1)
      aggregated_values =
        users_rows
        |> Stream.map(fn ({_user, values}) -> Enum.at(values, values_index) end)
        |> Enum.reject(&[] === &1)
      case low_users_count?(aggregated_values, anonymizer) do
        true  -> nil
        false ->
          aggregated_values
          |> preprocess_for_aggregation(column)
          |> aggregate_by(function, anonymizer)
          |> post_process_result(function, users_rows, values_index)
      end
    end

    property_values ++ aggregation_results
  end

  # See docs/anonymization.md for details
  defp preprocess_for_aggregation(values, {:distinct, _column}) do
    values
    |> Enum.sort_by(&(&1 |> Enum.uniq() |> Enum.count()))
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

  defp aggregate_by(aggregation_data, "count", anonymizer) do
    {count, _noise_sigma} = Anonymizer.count(anonymizer, aggregation_data)
    count
  end
  defp aggregate_by(aggregation_data, "sum", anonymizer) do
    {sum, _noise_sigma} = Anonymizer.sum(anonymizer, aggregation_data)
    sum
  end
  defp aggregate_by(aggregation_data, "avg", anonymizer) do
    {avg, _noise_sigma} = Anonymizer.avg(anonymizer, aggregation_data)
    avg
  end
  defp aggregate_by(aggregation_data, "stddev", anonymizer) do
    {stddev, _noise_sigma} = Anonymizer.stddev(anonymizer, aggregation_data)
    stddev
  end
  defp aggregate_by(aggregation_data, "count_noise", anonymizer) do
    {_count, noise_sigma} = Anonymizer.count(anonymizer, aggregation_data)
    noise_sigma
  end
  defp aggregate_by(aggregation_data, "sum_noise", anonymizer) do
    {_sum, noise_sigma} = Anonymizer.sum(anonymizer, aggregation_data)
    noise_sigma
  end
  defp aggregate_by(aggregation_data, "avg_noise", anonymizer) do
    {_avg, noise_sigma} = Anonymizer.avg(anonymizer, aggregation_data)
    noise_sigma
  end
  defp aggregate_by(aggregation_data, "stddev_noise", anonymizer) do
    {_stddev, noise_sigma} = Anonymizer.stddev(anonymizer, aggregation_data)
    noise_sigma
  end
  defp aggregate_by(aggregation_data, "min", anonymizer), do: Anonymizer.min(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "max", anonymizer), do: Anonymizer.max(anonymizer, aggregation_data)
  defp aggregate_by(aggregation_data, "median", anonymizer), do: Anonymizer.median(anonymizer, aggregation_data)
  defp aggregate_by(_, unknown_aggregator, _) do
    raise "Aggregator '#{unknown_aggregator}' is not supported!"
  end

  # For min / max aggregators, if there is a value which passes the LCF and
  # it is lower / greater than the aggregated result, we want to show that instead.
  defp post_process_result(result, "max", users_rows, values_index) do
    users_rows
    |> group_values(& &1 > result, values_index)
    |> Stream.concat([result])
    |> Enum.max()
  end
  defp post_process_result(result, "min", users_rows, values_index) do
    users_rows
    |> group_values(& &1 < result, values_index)
    |> Stream.concat([result])
    |> Enum.min()
  end
  defp post_process_result(result, _function, _users_rows, _values_index), do: result

  defp group_values(users_rows, filter_fun, values_index) do
    users_rows
    |> Enum.map(fn ({user, rows}) ->
      {user, rows |> Enum.at(values_index) |> Enum.filter(filter_fun) |> Enum.uniq()}
    end)
    |> Enum.reduce(%{}, fn ({user, values}, acc) ->
      Enum.reduce(values, acc, fn (value, acc) -> Map.update(acc, value, [user], &[user | &1]) end)
    end)
    |> Enum.reject(fn ({_value, users}) -> low_users_count?(users, Anonymizer.new(users)) end)
    |> Enum.map(fn ({value, _users}) -> value end)
  end

  defp make_buckets([], %Query{property: []} = query) do
    # If there are no results for a global aggregation, we'll produce one row.
    # All results will be `nil`-ed except for `count` which will have the value of 0.
    aggregated_values = Enum.map(query.aggregators, fn
      %Expression{function: "count"} -> 0
      %Expression{} -> nil
    end)
    [%{row: aggregated_values, occurrences: 1}]
  end
  defp make_buckets(rows, %Query{implicit_count?: false} = query) do
    Logger.debug("Making explicit buckets ...")
    rows
    |> Rows.extract_groups(query)
    |> Enum.map(&%{row: &1, occurrences: 1})
  end
  defp make_buckets(rows, %Query{implicit_count?: true} = query) do
    Logger.debug("Making implicit buckets ...")
    # We add the implicit "count" to the list of selected columns so that we can
    # retrieve it afterwards when making the bucket.
    columns_with_count = [Expression.count_star() | query.columns]
    rows
    |> Rows.extract_groups(%Query{query | columns: columns_with_count})
    |> Enum.map(fn ([count | row]) -> %{row: row, occurrences: count} end)
  end

  defp number_of_anonymized_users(data) do
    unique_user_ids = data
    |> Enum.map(fn({_result, _anonymizer, user_data}) -> user_data end)
    |> Enum.flat_map(&Map.keys/1)
    |> Enum.into(MapSet.new())
    anonymizer = Anonymizer.new(unique_user_ids)
    unique_users_count = MapSet.size(unique_user_ids)
    case Anonymizer.sufficiently_large?(anonymizer, unique_users_count) do
      {true, anonymizer} -> Anonymizer.noisy_count(anonymizer, unique_users_count)
      _ -> 0
    end
  end
end
