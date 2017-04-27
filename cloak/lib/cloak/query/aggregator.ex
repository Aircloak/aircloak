defmodule Cloak.Query.Aggregator do
  @moduledoc "This module aggregates the values into an anonymized result. See `aggregate/2` for details."

  require Logger

  alias Cloak.DataSource
  alias Cloak.Sql.{Query, Expression}
  alias Cloak.Query.{Anonymizer, Rows, Result}
  alias Cloak.Query.Runner.Engine

  @typep anonymization_group_values :: [DataSource.field | :*]
  @typep user_id :: DataSource.field
  @typep anonymization_group :: {anonymization_group_values, Anonymizer.t, %{user_id => DataSource.row}}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Transforms the non-anonymized rows returned from the database into an
  anonymized result. This is done in following steps:

  1. Rows are groupped per distinct anonymization group. Anonymization group is a
     collection of selected columns, as well as columns listed in the `group by` clause.
     Additionally, inside each distinct anonymization group, rows are groupped per user.

  2. Anonymization groups for which there are not enough distinct users are discarded.
     A low-count substitute row is generated for all such groups to indicate
     the amount of rows which are filtered out. This row is reported, but only
     if there are enough of users which are filtered out.

  3. Aggregation functions (e.g. `sum`, `count`) are computed for each distinct
     anonymization group. The resulting values are anonymized using the `Anonymizer`
     module.

  Each output row will consist of all anonymization group values together with
  computed anonymized aggregates (count, sum, ...). For example, in the following
  query:

  ```
  select foo, count(*), avg(bar) from baz group by foo
  ```

  Each output row will consist of columns `foo`, `count(*)`, and `avg(bar)`.
  """
  @spec aggregate(Enumerable.t, Query.t, Engine.state_updater) :: Result.t
  def aggregate(rows, query, state_updater) do
    anonymization_group_expressions = anonymization_group_expressions(query)
    anonymization_groups = anonymization_groups(rows, query, anonymization_group_expressions, state_updater)
    users_count = number_of_anonymized_users(anonymization_groups)
    aggregated_buckets = anonymization_groups
      |> process_low_count_users(anonymization_group_expressions)
      |> aggregate_anonymization_groups(query)
      |> make_buckets(query, anonymization_group_expressions)

    state_updater.(:post_processing)

    Result.new(query, bucket_columns(query), aggregated_buckets, users_count)
  end

  @doc "Returns the list of expressions used to form the anonymization group."
  @spec anonymization_group_expressions(Query.t) :: [Expression.t]
  def anonymization_group_expressions(%Query{group_by: [_|_] = group_by}), do:
    Expression.unique_except(group_by, &Expression.row_splitter?/1)
  def anonymization_group_expressions(%Query{group_by: [], implicit_count?: true} = query) do
    additional_expressions = bucket_columns(query) -- query.columns
    Expression.unique_except(query.columns ++ additional_expressions, &Expression.row_splitter?/1)
  end
  def anonymization_group_expressions(%Query{group_by: [], implicit_count?: false} = query), do:
    bucket_columns(query) -- query.columns

  @doc "Returns the ordered list of bucket columns."
  @spec bucket_columns(Query.t) :: [Expression.t]
  def bucket_columns(query) do
    non_selected_order_by_expressions = Query.order_by_expressions(query) -- (query.columns ++ query.group_by)
    query.columns ++ non_selected_order_by_expressions
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp aggregate_values([], [], []), do: []
  defp aggregate_values([nil | rest_values], [accumulator | rest_accumulators], [_aggregator | rest_aggregators]), do:
    [accumulator | aggregate_values(rest_values, rest_accumulators, rest_aggregators)]
  defp aggregate_values([value | rest_values], [accumulator | rest_accumulators], [aggregator | rest_aggregators]), do:
    [aggregate_value(aggregator, value, accumulator) |
      aggregate_values(rest_values, rest_accumulators, rest_aggregators)]

  defp user_id([user_id | _rest]), do: user_id

  defp per_user_aggregator(%Expression{function_args: [{:distinct, _column}]}), do: :set
  defp per_user_aggregator(%Expression{function: "count"}), do: :count
  defp per_user_aggregator(%Expression{function: "count_noise"}), do: :count
  defp per_user_aggregator(%Expression{function: "sum"}), do: :sum
  defp per_user_aggregator(%Expression{function: "sum_noise"}), do: :sum
  defp per_user_aggregator(%Expression{function: "avg"}), do: :avg
  defp per_user_aggregator(%Expression{function: "avg_noise"}), do: :avg
  defp per_user_aggregator(%Expression{function: "stddev"}), do: :stddev
  defp per_user_aggregator(%Expression{function: "stddev_noise"}), do: :stddev
  defp per_user_aggregator(%Expression{function: "min"}), do: :set
  defp per_user_aggregator(%Expression{function: "max"}), do: :set
  defp per_user_aggregator(%Expression{function: "median"}), do: :list

  defp aggregate_value(:count, _value, nil), do: 1
  defp aggregate_value(:count, _value, count), do: count + 1
  defp aggregate_value(:sum, value, nil), do: value
  defp aggregate_value(:sum, value, sum), do: sum + value
  defp aggregate_value(:avg, value, nil), do: {value, 1}
  defp aggregate_value(:avg, value, {sum, count}), do: {sum + value, count + 1}
  defp aggregate_value(:stddev, value, nil), do: {value, value * value, 1}
  defp aggregate_value(:stddev, value, {sum, sum_sqrs, count}), do: {sum + value, sum_sqrs + value * value, count + 1}
  defp aggregate_value(:set, value, nil), do: MapSet.new([value])
  defp aggregate_value(:set, value, prev_values), do: MapSet.put(prev_values, value)
  defp aggregate_value(:list, value, nil), do: [value]
  defp aggregate_value(:list, value, prev_values), do: [value | prev_values]

  # This function merges the per-user accumulated values of two different buckets.
  # Used during the creation of the low-count filtered bucket.
  @dialyzer {:nowarn_function, merge_accumulators: 1} # disable dialyzer warning because of `MapSet.union/2` call
  defp merge_accumulators({value, nil}), do: value # no values present for second bucket
  defp merge_accumulators({nil, value}), do: value # no values present for first bucket
  defp merge_accumulators({value1, value2}) when is_number(value1) and is_number(value2), do:
    value1 + value2 # sum and count accoumulators
  defp merge_accumulators({value1, value2}) when is_list(value1) and is_list(value2), do:
    value1 ++ value2 # median accumulators
  defp merge_accumulators({%MapSet{} = value1, %MapSet{} = value2}), do:
    MapSet.union(value1, value2) # min, max or distinct accumulators
  defp merge_accumulators({{value1a, value1b}, {value2a, value2b}}), do:
    {value1a + value2a, value1b + value2b} # avg accumulators
  defp merge_accumulators({{value1a, value1b, value1c}, {value2a, value2b, value2c}}), do:
    {value1a + value2a, value1b + value2b, value1c + value2c} # stddev accumulators

  defp aggregated_column(%Expression{function_args: [:*]}), do: Expression.constant(nil, :*)
  defp aggregated_column(%Expression{function_args: [{:distinct, column}]}), do: column
  defp aggregated_column(%Expression{function_args: [column]}), do: column

  defp per_user_aggregator_and_column(aggregator), do:
    {per_user_aggregator(aggregator), aggregated_column(aggregator)}

  defp anonymization_groups(rows, query, anonymization_group_expressions, state_updater) do
    Logger.debug("Grouping rows ...")
    {per_user_aggregators, aggregated_columns} =
      query.aggregators
      |> Enum.map(&per_user_aggregator_and_column/1)
      |> Enum.uniq()
      |> Enum.unzip()
    default_accumulators = List.duplicate(nil, Enum.count(aggregated_columns))
    rows
    |> Enum.reduce(%{}, fn(row, accumulator) ->
      if Map.size(accumulator) == 0, do: state_updater.(:ingesting_data)
      group_row(accumulator, row, default_accumulators, anonymization_group_expressions, per_user_aggregators,
        aggregated_columns)
    end)
    |> fn(rows) -> state_updater.(:processing); rows end.()
    |> init_anonymizer()
  end

  defp group_row(accumulator, row, default_accumulators, anonymization_group_expressions, per_user_aggregators,
    aggregated_columns
  ) do
    user_id = user_id(row)
    anonymization_group_values = Enum.map(anonymization_group_expressions, &Expression.value(&1, row))
    values = for column <- aggregated_columns, do: Expression.value(column, row)
    accumulator
    |> Map.put_new(anonymization_group_values, %{})
    |> Map.update!(anonymization_group_values, fn (user_values_map) ->
      user_values_map
      |> Map.put_new(user_id, default_accumulators)
      |> Map.update!(user_id, &aggregate_values(values, &1, per_user_aggregators))
    end)
  end

  defp init_anonymizer(anonymization_groups), do:
    for {values, users_rows} <- anonymization_groups, do:
      {values, Anonymizer.new(users_rows), users_rows}

  defp low_users_count?({_values, anonymizer, users_rows}), do:
    low_users_count?(users_rows, anonymizer)

  defp low_users_count?(count, anonymizer) when is_integer(count) do
    {sufficiently_large?, _} = Anonymizer.sufficiently_large?(anonymizer, count)
    not sufficiently_large?
  end
  defp low_users_count?(values, anonymizer), do:
    values |> Enum.count() |> low_users_count?(anonymizer)

  @spec process_low_count_users([anonymization_group], [Expression.t]) :: [anonymization_group]
  defp process_low_count_users(rows, anonymization_group_expressions) do
    Logger.debug("Processing low count users ...")
    {low_count_rows, high_count_rows} = Enum.partition(rows, &low_users_count?/1)
    lcf_users_rows = Enum.reduce(low_count_rows, %{},
      fn ({_values, _anonymizer, users_rows}, accumulator) ->
        Map.merge(accumulator, users_rows, fn (_user, columns1, columns2) ->
          Enum.zip(columns1, columns2) |> Enum.map(&merge_accumulators/1)
        end)
      end)
    anonymizer = Anonymizer.new(lcf_users_rows)
    lcf_values = List.duplicate(:*, length(anonymization_group_expressions))
    lcf_row = {lcf_values, anonymizer, lcf_users_rows}
    case low_users_count?(lcf_row) do
      false -> [lcf_row | high_count_rows]
      true -> high_count_rows
    end
  end

  @spec aggregate_anonymization_groups([anonymization_group], Query.t) :: [DataSource.row]
  defp aggregate_anonymization_groups(anonymization_groups, query) do
    Logger.debug("Aggregating anonymization groups ...")
    # Only unique per-user aggregators are computed, so wee need to compute the index
    # of the aggregator into the per-user aggregated value list.
    per_user_aggregators_and_columns =
      query.aggregators
      |> Enum.map(&per_user_aggregator_and_column/1)
      |> Enum.uniq()
    indexed_aggregators =
      Enum.map(query.aggregators, fn (aggregator) ->
        per_user_aggregator_and_column = per_user_aggregator_and_column(aggregator)
        values_index = Enum.find_index(per_user_aggregators_and_columns, & &1 == per_user_aggregator_and_column)
        {values_index, aggregator}
      end)
    Enum.map(anonymization_groups, &aggregate_anonymization_group(&1, indexed_aggregators))
  end

  defp aggregate_anonymization_group({values, anonymizer, users_rows}, indexed_aggregators) do
    aggregation_results = Enum.map(indexed_aggregators, fn ({values_index, aggregator}) ->
      aggregated_values =
        users_rows
        |> Stream.map(fn ({_user, row_values}) -> Enum.at(row_values, values_index) end)
        |> Enum.reject(&is_nil/1)
      case low_users_count?(aggregated_values, anonymizer) do
        true  ->
          if aggregator.function == "count", do: 0, else: nil
        false ->
          aggregated_values
          |> preprocess_for_aggregation(aggregator)
          |> aggregate_by(aggregator.function, aggregator.type, anonymizer)
          |> post_process_result(aggregator.function, users_rows, values_index)
      end
    end)

    users_count = Anonymizer.noisy_count(anonymizer, Enum.count(users_rows))
    {users_count, values ++ aggregation_results}
  end

  # See docs/anonymization.md for details
  defp preprocess_for_aggregation(values, %Expression{function_args: [{:distinct, column}]} = aggregator) do
    per_user_aggregator = per_user_aggregator(%Expression{aggregator | function_args: [column]})
    values
    |> Enum.sort_by(&Enum.count/1)
    |> Stream.with_index()
    |> Stream.flat_map(fn ({row, index}) ->
      Enum.map(row, &{index, &1})
    end)
    |> Stream.uniq(fn ({_index, value}) -> value end)
    |> Enum.reduce(%{}, fn({index, value}, accumulator) ->
      accumulator
      |> Map.put_new(index, nil)
      |> Map.update!(index, &aggregate_value(per_user_aggregator, value, &1))
    end)
    |> Map.values()
  end
  defp preprocess_for_aggregation(values, _aggregator), do: values

  defp aggregate_by(aggregation_data, "count", _type, anonymizer) do
    {count, _noise_sigma} = Anonymizer.count(anonymizer, aggregation_data)
    count
  end
  defp aggregate_by(aggregation_data, "sum", type, anonymizer) do
    {sum, _noise_sigma} = Anonymizer.sum(anonymizer, aggregation_data)
    float_to_type(sum, type)
  end
  defp aggregate_by(aggregation_data, "avg", _type, anonymizer) do
    {avg, _noise_sigma} = Anonymizer.avg(anonymizer, aggregation_data)
    avg
  end
  defp aggregate_by(aggregation_data, "stddev", _type, anonymizer) do
    {stddev, _noise_sigma} = Anonymizer.stddev(anonymizer, aggregation_data)
    stddev
  end
  defp aggregate_by(aggregation_data, "count_noise", _type, anonymizer) do
    {_count, noise_sigma} = Anonymizer.count(anonymizer, aggregation_data)
    noise_sigma
  end
  defp aggregate_by(aggregation_data, "sum_noise", type, anonymizer) do
    {_sum, noise_sigma} = Anonymizer.sum(anonymizer, aggregation_data)
    float_to_type(noise_sigma, type)
  end
  defp aggregate_by(aggregation_data, "avg_noise", _type, anonymizer) do
    {_avg, noise_sigma} = Anonymizer.avg(anonymizer, aggregation_data)
    noise_sigma
  end
  defp aggregate_by(aggregation_data, "stddev_noise", _type, anonymizer) do
    {_stddev, noise_sigma} = Anonymizer.stddev(anonymizer, aggregation_data)
    noise_sigma
  end
  defp aggregate_by(aggregation_data, "min", type, anonymizer), do:
    Anonymizer.min(anonymizer, aggregation_data) |> float_to_type(type)
  defp aggregate_by(aggregation_data, "max", type, anonymizer), do:
    Anonymizer.max(anonymizer, aggregation_data) |> float_to_type(type)
  defp aggregate_by(aggregation_data, "median", type, anonymizer), do:
    Anonymizer.median(anonymizer, aggregation_data) |> float_to_type(type)
  defp aggregate_by(_, unknown_aggregator, _type, _) do
    raise "Aggregator '#{unknown_aggregator}' is not supported!"
  end

  defp float_to_type(nil, _type), do: nil
  defp float_to_type(value, :integer), do: round(value)
  defp float_to_type(value, :real), do: value

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
    |> Stream.map(fn ({user, rows}) ->
      filtered_rows =
        case Enum.at(rows, values_index) do
          nil -> []
          set -> Enum.filter(set, filter_fun)
        end
      {user, filtered_rows}
    end)
    |> Enum.reduce(%{}, fn ({user, values}, acc) ->
      Enum.reduce(values, acc, fn (value, acc) -> Map.update(acc, value, [user], &[user | &1]) end)
    end)
    |> Enum.reject(fn ({_value, users}) -> low_users_count?(users, Anonymizer.new(users)) end)
    |> Enum.map(fn ({value, _users}) -> value end)
  end

  defp make_buckets([], query, []) do
    # If there are no results for a global aggregation, we'll produce one row.
    # All results will be `nil`-ed except for `count` which will have the value of 0.
    aggregated_values = Enum.map(query.aggregators, fn
      %Expression{function: "count"} -> 0
      %Expression{} -> nil
    end)
    [%{row: aggregated_values, occurrences: 1, users_count: 0}]
  end
  defp make_buckets(rows, %Query{implicit_count?: false} = query, anonymization_group_expressions) do
    Logger.debug("Making explicit buckets ...")
    rows
    |> Stream.map(fn ({_users_count, row}) -> row end)
    |> Rows.extract_groups(anonymization_group_expressions, bucket_columns(query), query)
    |> Stream.zip(Stream.map(rows, fn ({users_count, _row}) -> users_count end))
    |> Enum.map(fn ({row, users_count}) ->
      %{row: row, occurrences: 1, users_count: users_count}
    end)
  end
  defp make_buckets(rows, %Query{implicit_count?: true} = query, anonymization_group_expressions) do
    Logger.debug("Making implicit buckets ...")
    rows
    |> Stream.map(fn ({_users_count, row}) -> row end)
    |> Rows.extract_groups(anonymization_group_expressions, [Expression.count_star() | bucket_columns(query)],
      query)
    |> Stream.zip(Stream.map(rows, fn ({users_count, _row}) -> users_count end))
    |> Enum.map(fn ({[count | row], users_count}) ->
      %{row: row, occurrences: count, users_count: users_count}
    end)
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
