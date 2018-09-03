defmodule Cloak.Query.Aggregator do
  @moduledoc "This module aggregates the values into an anonymized result. See `aggregate/2` for details."

  require Logger

  alias Cloak.DataSource
  alias Cloak.Sql.{Query, Expression, NoiseLayer}
  alias Cloak.Query.{Anonymizer, Rows, Result}

  @typep group_values :: [DataSource.field() | :*]
  @typep user_id :: DataSource.field()
  @typep group :: {group_values, Anonymizer.t(), %{user_id => DataSource.row()}}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
    Transforms the previously grouped database rows into anonymized buckets. This is done in following steps:

    1. Groups for which there are not enough distinct users are discarded.
       A low-count substitute row is generated for all such groups to indicate
       the amount of rows which are filtered out. This row is reported, but only
       if there are enough of users which are filtered out.

    2. Aggregation functions (e.g. `sum`, `count`) are computed for each distinct
       group. The resulting values are anonymized using the `Anonymizer`
       module.

    Each output row will consist of all anonymization group values together with
    computed anonymized aggregates (count, sum, ...). For example, in the following
    query:

    ```
    select foo, count(*), avg(bar) from baz group by foo
    ```

    Each output row will consist of columns `foo`, `count(*)`, and `avg(bar)`.
  """
  @spec aggregate(Rows.groups(), Query.t()) :: [Result.bucket()]
  def aggregate(groups, query),
    do:
      groups
      |> init_anonymizer()
      |> process_low_count_users(query)
      |> aggregate_groups(query)
      |> make_buckets(query)
      |> finalize_buckets(query)

  @doc """
    Rows are grouped per query specification. See `Cloak.Query.Rows.group_expressions/1` for details.
    Additionally, inside each distinct group, rows are groupped per user.
  """
  @spec group(Enumerable.t(), Query.t()) :: Rows.groups()
  def group(rows, query) do
    {per_user_aggregators, aggregated_columns} =
      query.aggregators
      |> Enum.map(&per_user_aggregator_and_column/1)
      |> Enum.uniq()
      |> Enum.unzip()

    default_accumulators = List.duplicate(nil, Enum.count(aggregated_columns))
    default_noise_layers = NoiseLayer.new_accumulator(query.noise_layers)

    merging_fun = group_updater(per_user_aggregators, aggregated_columns, default_accumulators, query)

    Rows.group(rows, query, {%{}, default_noise_layers}, merging_fun)
  end

  @doc """
    Returns the union of two sets of groups.
    Merging is needed when grouping over multiple processes, before the start of the aggregation step.
  """
  @spec merge_groups(Rows.groups(), Rows.groups()) :: Rows.groups()
  def merge_groups(groups1, groups2),
    do:
      Map.merge(groups1, groups2, fn _key, {user_values1, noise_layers1}, {user_values2, noise_layers2} ->
        {merge_user_values(user_values1, user_values2), NoiseLayer.merge_accumulators(noise_layers1, noise_layers2)}
      end)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp aggregate_values([], [], []), do: []

  defp aggregate_values([nil | rest_values], [accumulator | rest_accumulators], [
         _aggregator | rest_aggregators
       ]),
       do: [accumulator | aggregate_values(rest_values, rest_accumulators, rest_aggregators)]

  defp aggregate_values([value | rest_values], [accumulator | rest_accumulators], [
         aggregator | rest_aggregators
       ]),
       do: [
         aggregate_value(aggregator, value, accumulator)
         | aggregate_values(rest_values, rest_accumulators, rest_aggregators)
       ]

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
  defp per_user_aggregator(%Expression{function: "min"}), do: :min
  defp per_user_aggregator(%Expression{function: "max"}), do: :max
  defp per_user_aggregator(%Expression{function: "median"}), do: :list

  defp aggregate_value(:count, _value, nil), do: 1
  defp aggregate_value(:count, _value, count), do: count + 1
  defp aggregate_value(:sum, value, nil), do: value
  defp aggregate_value(:sum, value, sum), do: sum + value
  defp aggregate_value(:avg, value, nil), do: {:avg, value, 1}
  defp aggregate_value(:avg, value, {:avg, sum, count}), do: {:avg, sum + value, count + 1}
  defp aggregate_value(:stddev, value, nil), do: {:stddev, value, value * value, 1}

  defp aggregate_value(:stddev, value, {:stddev, sum, sum_sqrs, count}),
    do: {:stddev, sum + value, sum_sqrs + value * value, count + 1}

  defp aggregate_value(:set, value, nil), do: MapSet.new([value])
  defp aggregate_value(:set, value, prev_values), do: MapSet.put(prev_values, value)
  defp aggregate_value(:list, value, nil), do: [value]
  defp aggregate_value(:list, value, prev_values), do: [value | prev_values]
  defp aggregate_value(:min, value, nil), do: {:min, value}
  defp aggregate_value(:min, value, {:min, prev_value}), do: {:min, min(value, prev_value)}
  defp aggregate_value(:max, value, nil), do: {:max, value}
  defp aggregate_value(:max, value, {:max, prev_value}), do: {:max, max(value, prev_value)}

  # This function merges the per-user accumulated values of two different buckets.
  # Used during the creation of the low-count filtered bucket.
  # disable dialyzer warning because of `MapSet.union/2` call
  @dialyzer {:nowarn_function, merge_accumulators: 1}
  # no values present for second bucket
  defp merge_accumulators({value, nil}), do: value
  # no values present for first bucket
  defp merge_accumulators({nil, value}), do: value

  defp merge_accumulators({value1, value2}) when is_number(value1) and is_number(value2),
    # sum and count accoumulators
    do: value1 + value2

  defp merge_accumulators({value1, value2}) when is_list(value1) and is_list(value2),
    # median accumulators
    do: value1 ++ value2

  defp merge_accumulators({%MapSet{} = value1, %MapSet{} = value2}),
    # distinct accumulators
    do: MapSet.union(value1, value2)

  defp merge_accumulators({{:avg, value1a, value1b}, {:avg, value2a, value2b}}),
    do: {:avg, value1a + value2a, value1b + value2b}

  defp merge_accumulators({{:stddev, value1a, value1b, value1c}, {:stddev, value2a, value2b, value2c}}),
    do: {:stddev, value1a + value2a, value1b + value2b, value1c + value2c}

  defp merge_accumulators({{:min, value1}, {:min, value2}}), do: {:min, min(value1, value2)}
  defp merge_accumulators({{:max, value1}, {:max, value2}}), do: {:max, max(value1, value2)}

  defp aggregated_column(%Expression{function_args: [:*]}), do: Expression.constant(nil, :*)
  defp aggregated_column(%Expression{function_args: [{:distinct, column}]}), do: column
  defp aggregated_column(%Expression{function_args: [column]}), do: column

  defp per_user_aggregator_and_column(aggregator), do: {per_user_aggregator(aggregator), aggregated_column(aggregator)}

  defp group_updater(per_user_aggregators, aggregated_columns, default_accumulators, query) do
    processed_noise_layers = NoiseLayer.pre_process_layers(query.noise_layers)

    fn {user_rows, noise_accumulator}, row ->
      user_id = user_id(row)
      values = Enum.map(aggregated_columns, &Expression.value(&1, row))

      user_rows =
        user_rows
        |> Map.put_new(user_id, default_accumulators)
        |> Map.update!(user_id, &aggregate_values(values, &1, per_user_aggregators))

      noise_accumulator = NoiseLayer.accumulate(processed_noise_layers, noise_accumulator, row)

      {user_rows, noise_accumulator}
    end
  end

  defp init_anonymizer(grouped_rows) do
    Logger.debug("Initializing anonymizer ...")

    Enum.map(grouped_rows, fn {property, {users_rows, noise_layers}} ->
      {property, Anonymizer.new(noise_layers), users_rows}
    end)
  end

  defp low_users_count?({_values, anonymizer, users_rows}), do: low_users_count?(users_rows, anonymizer)

  defp low_users_count?(count, anonymizer) when is_integer(count),
    do: not Anonymizer.sufficiently_large?(anonymizer, count)

  defp low_users_count?(values, anonymizer), do: values |> Enum.count() |> low_users_count?(anonymizer)

  @spec process_low_count_users([group], Query.t()) :: [group]
  defp process_low_count_users(rows, query) do
    Logger.debug("Processing low count users ...")
    bucket_size = query |> Rows.group_expressions() |> length()
    lcf_aggregation_limit = query.data_source |> lcf_buckets_aggregation_limit() |> min(bucket_size)

    # We first partition the buckets into low-count and high-count buckets.
    # Then, starting from right to left, we censor each bucket value sequentially and merge corresponding buckets.
    # We then split the merged buckets again. We keep the merged buckets that pass the low-count filter and
    # repeat the process for the next column and the new set of low-count buckets.
    # When we run out of bucket values, we drop the final low-count bucket, if any.

    {_low_count_rows, high_count_rows} =
      rows |> Enum.split_with(&low_users_count?/1) |> aggregate_lcf_buckets(lcf_aggregation_limit, bucket_size)

    high_count_rows
  end

  defp aggregate_lcf_buckets(splitted_rows, 0, _bucket_size), do: splitted_rows

  defp aggregate_lcf_buckets(splitted_rows, bucket_size, bucket_size),
    do: Enum.reduce((bucket_size - 1)..0, splitted_rows, &group_low_count_rows/2)

  defp aggregate_lcf_buckets({low_count_rows, high_count_rows}, lcf_aggregation_limit, bucket_size)
       when lcf_aggregation_limit < bucket_size do
    # censor values from lcf partial aggregation limit until max label count
    low_count_rows =
      Enum.map(low_count_rows, fn {values, anonymizer, users_rows} ->
        values = Enum.reduce(lcf_aggregation_limit..bucket_size, values, &List.replace_at(&2, &1, :*))
        {values, anonymizer, users_rows}
      end)

    Enum.reduce((lcf_aggregation_limit - 1)..0, {low_count_rows, high_count_rows}, &group_low_count_rows/2)
  end

  defp group_low_count_rows(column_index, {low_count_rows, high_count_rows}) do
    {low_count_grouped_rows, high_count_grouped_rows} =
      low_count_rows
      |> Enum.group_by(fn {values, _anonymizer, _users_rows} ->
        List.replace_at(values, column_index, :*)
      end)
      |> Enum.map(&collapse_grouped_rows/1)
      |> Enum.split_with(&low_users_count?/1)

    {low_count_grouped_rows, high_count_grouped_rows ++ high_count_rows}
  end

  defp collapse_grouped_rows({values, grouped_rows}) do
    user_rows =
      grouped_rows
      |> Stream.map(fn {_values, _anonymizer, users_rows} -> users_rows end)
      |> Enum.reduce(fn users_rows1, users_rows2 ->
        Map.merge(users_rows1, users_rows2, fn _user, columns1, columns2 ->
          Enum.zip(columns1, columns2) |> Enum.map(&merge_accumulators/1)
        end)
      end)

    anonymizer =
      grouped_rows
      |> Enum.map(fn {_values, anonymizer, _users_rows} -> anonymizer.noise_layers end)
      |> Enum.reduce(&NoiseLayer.merge_accumulators/2)
      |> Anonymizer.new()

    {values, anonymizer, user_rows}
  end

  @spec aggregate_groups([group], Query.t()) :: [DataSource.row()]
  defp aggregate_groups(groups, query) do
    Logger.debug("Aggregating groups ...")
    # Only unique per-user aggregators are computed, so wee need to compute the index
    # of the aggregator into the per-user aggregated value list.
    per_user_aggregators_and_columns =
      query.aggregators
      |> Enum.map(&per_user_aggregator_and_column/1)
      |> Enum.uniq()

    indexed_aggregators =
      Enum.map(query.aggregators, fn aggregator ->
        per_user_aggregator_and_column = per_user_aggregator_and_column(aggregator)

        values_index =
          Enum.find_index(
            per_user_aggregators_and_columns,
            &(&1 == per_user_aggregator_and_column)
          )

        {values_index, aggregator}
      end)

    Enum.map(groups, &aggregate_group(&1, indexed_aggregators))
  end

  defp aggregate_group({values, anonymizer, users_rows}, indexed_aggregators) do
    aggregation_results =
      Enum.map(indexed_aggregators, fn {values_index, aggregator} ->
        users_rows
        |> Stream.map(fn {_user, row_values} -> Enum.at(row_values, values_index) end)
        |> Enum.reject(&is_nil/1)
        |> preprocess_for_aggregation(aggregator)
        |> aggregate_by(aggregator.function, aggregator.type, anonymizer)
      end)

    users_count = Anonymizer.noisy_count(anonymizer, Enum.count(users_rows))
    {users_count, values ++ aggregation_results}
  end

  # See docs/anonymization.md for details
  defp preprocess_for_aggregation(
         values,
         %Expression{function_args: [{:distinct, column}]} = aggregator
       ) do
    per_user_aggregator = per_user_aggregator(%Expression{aggregator | function_args: [column]})

    values
    |> Enum.sort_by(&Enum.count/1)
    |> Stream.with_index()
    |> Stream.flat_map(fn {row, index} ->
      Enum.map(row, &{index, &1})
    end)
    |> Stream.uniq_by(fn {_index, value} -> value end)
    |> Enum.reduce(%{}, fn {index, value}, accumulator ->
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

  defp aggregate_by(aggregation_data, "sum_noise", _type, anonymizer) do
    {_sum, noise_sigma} = Anonymizer.sum(anonymizer, aggregation_data)
    noise_sigma
  end

  defp aggregate_by(aggregation_data, "avg_noise", _type, anonymizer) do
    {_avg, noise_sigma} = Anonymizer.avg(anonymizer, aggregation_data)
    noise_sigma
  end

  defp aggregate_by(aggregation_data, "stddev_noise", _type, anonymizer) do
    {_stddev, noise_sigma} = Anonymizer.stddev(anonymizer, aggregation_data)
    noise_sigma
  end

  defp aggregate_by(aggregation_data, aggregator, type, anonymizer)
       when type in [:datetime, :date, :time] and aggregator in ["min", "max", "median"] do
    aggregation_data
    |> Stream.map(fn
      {:min, value} -> {:min, Cloak.Time.to_integer(value)}
      {:max, value} -> {:max, Cloak.Time.to_integer(value)}
      values when is_list(values) -> Enum.map(values, &Cloak.Time.to_integer/1)
    end)
    |> aggregate_by(aggregator, :integer, anonymizer)
    |> Cloak.Time.from_integer(type)
  end

  defp aggregate_by(aggregation_data, "min", type, anonymizer),
    do: Anonymizer.min(anonymizer, aggregation_data) |> float_to_type(type)

  defp aggregate_by(aggregation_data, "max", type, anonymizer),
    do: Anonymizer.max(anonymizer, aggregation_data) |> float_to_type(type)

  defp aggregate_by(aggregation_data, "median", type, anonymizer),
    do: Anonymizer.median(anonymizer, aggregation_data) |> float_to_type(type)

  defp aggregate_by(_, unknown_aggregator, _type, _) do
    raise "Aggregator '#{unknown_aggregator}' is not supported!"
  end

  defp float_to_type(nil, _type), do: nil
  defp float_to_type(value, :integer), do: round(value)
  defp float_to_type(value, :real), do: value

  defp make_buckets(rows, query) do
    if rows == [] && Rows.group_expressions(query) == [] do
      # If there are no results for a global aggregation, we'll produce one row.
      # All results will be `nil`-ed except for `count` which will have the value of 0.
      aggregated_values =
        Enum.map(query.aggregators, fn
          %Expression{function: "count"} -> 0
          %Expression{} -> nil
        end)

      [%{row: aggregated_values, occurrences: 1, unreliable: true}]
    else
      make_non_empty_buckets(rows, query)
    end
  end

  defp make_non_empty_buckets(rows, %Query{implicit_count?: false} = query) do
    Logger.debug("Making explicit buckets ...")

    rows
    |> Stream.map(fn {_users_count, row} -> row end)
    |> Rows.extract_groups(Query.bucket_columns(query), query)
    |> Stream.zip(Stream.map(rows, fn {users_count, _row} -> users_count end))
    |> Enum.map(fn {row, users_count} ->
      %{row: row, occurrences: 1, unreliable: unreliable_bucket?(users_count)}
    end)
  end

  defp make_non_empty_buckets(rows, %Query{implicit_count?: true} = query) do
    Logger.debug("Making implicit buckets ...")

    rows
    |> Stream.map(fn {_users_count, row} -> row end)
    |> Rows.extract_groups([Expression.count_star() | Query.bucket_columns(query)], query)
    |> Stream.zip(Stream.map(rows, fn {users_count, _row} -> users_count end))
    |> Enum.map(fn {[count | row], users_count} ->
      %{row: row, occurrences: count, unreliable: unreliable_bucket?(users_count)}
    end)
  end

  @users_count_reliability_threshold 15
  defp unreliable_bucket?(users_count), do: users_count < @users_count_reliability_threshold

  defp merge_user_values(user_values1, user_values2),
    do:
      Map.merge(user_values1, user_values2, fn _user, columns1, columns2 ->
        columns1 |> Enum.zip(columns2) |> Enum.map(&merge_accumulators/1)
      end)

  defp finalize_buckets(buckets, query) do
    bucket_columns = Query.bucket_columns(query)

    buckets
    |> Cloak.Query.Sorter.order_rows(bucket_columns, query.order_by, & &1.row)
    |> distinct(query.distinct?)
    |> offset(query.offset)
    |> limit(query.limit)
    |> drop_non_selected_columns(bucket_columns, query.columns)
  end

  defp distinct(buckets, true), do: Enum.map(buckets, &Map.put(&1, :occurrences, 1))
  defp distinct(buckets, false), do: buckets

  defp offset(buckets, 0), do: buckets
  defp offset([], _amount), do: []

  defp offset([%{occurrences: occurrences} | rest], amount) when occurrences <= amount,
    do: offset(rest, amount - occurrences)

  defp offset([%{occurrences: occurrences} = bucket | rest], amount),
    do: [%{bucket | occurrences: occurrences - amount} | rest]

  defp limit(buckets, nil), do: buckets

  defp limit(buckets, amount),
    do:
      buckets
      |> take(amount, [])
      |> Enum.reverse()

  defp take([], _amount, acc), do: acc

  defp take([%{occurrences: occurrences} = bucket | rest], amount, acc) when occurrences < amount,
    do: take(rest, amount - occurrences, [bucket | acc])

  defp take([%{} = bucket | _rest], amount, acc), do: [%{bucket | occurrences: amount} | acc]

  def drop_non_selected_columns(buckets, selected_columns, selected_columns),
    # Optimization of the frequent case where selected columns are equal to bucket columns
    do: buckets

  def drop_non_selected_columns(buckets, bucket_columns, selected_columns) do
    selected_columns_indices =
      Enum.map(selected_columns, fn selected_column ->
        index = Enum.find_index(bucket_columns, &(&1 == selected_column))
        true = index != nil
        index
      end)

    Enum.map(
      buckets,
      &%{&1 | row: Enum.map(selected_columns_indices, fn index -> Enum.at(&1.row, index) end)}
    )
  end

  defp lcf_buckets_aggregation_limit(data_source),
    do: data_source.lcf_buckets_aggregation_limit || Application.get_env(:cloak, :lcf_buckets_aggregation_limit, 3)
end
