defmodule Cloak.Query.Aggregator do
  @moduledoc "This module aggregates the values into an anonymized result. See `aggregate/2` for details."

  require Logger

  alias __MODULE__.{UserId, Statistics}
  alias Cloak.DataSource
  alias Cloak.Query.{Anonymizer, Result, Rows}
  alias Cloak.Sql.{Expression, NoiseLayer, Query}

  @typep group_values :: [DataSource.field() | :*]
  @typep group :: {group_values, %{any => Anonymizer.t()}, UserId.t() | Statistics.t()}

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
  def aggregate(groups, query) do
    query = aggregation_sub_module(query).pre_process(query)

    groups
    |> init_anonymizers(query)
    |> process_low_count_users(query)
    |> aggregate_groups(query)
    |> make_buckets(query)
    |> finalize_buckets(query)
  end

  @doc """
    Rows are grouped per query specification. See `Cloak.Query.Rows.group_expressions/1` for details.
    Additionally, inside each distinct group, rows are groupped per user.
  """
  @spec group(Enumerable.t(), Query.t()) :: Rows.groups()
  def group(rows, query) do
    grouping_sets_count = grouping_sets_count(query.grouping_sets)

    default_noise_layers =
      for grouping_set_index <- 0..(grouping_sets_count - 1), into: %{} do
        default_noise_layers =
          query.noise_layers
          |> NoiseLayer.filter_layers_for_grouping_set(grouping_set_index)
          |> NoiseLayer.new_accumulator()

        {grouping_set_index, default_noise_layers}
      end

    processed_noise_layers =
      for grouping_set_index <- 0..(grouping_sets_count - 1), into: %{} do
        processed_noise_layers =
          query.noise_layers
          |> NoiseLayer.filter_layers_for_grouping_set(grouping_set_index)
          |> NoiseLayer.pre_process_layers()

        {grouping_set_index, processed_noise_layers}
      end

    group_updater = aggregation_sub_module(query).group_updater(processed_noise_layers, query)
    group_initializer = &{%{}, Map.fetch!(default_noise_layers, &1)}

    Rows.group(rows, query, group_initializer, group_updater)
  end

  @doc """
    Returns the function that performans the union of two sets of groups.
    Merging is needed when grouping over multiple processes, before the start of the aggregation step.
  """
  @spec group_merger(Query.t()) :: (Rows.groups(), Rows.groups() -> Rows.groups())
  def group_merger(query) do
    aggregation_sub_module = aggregation_sub_module(query)

    fn groups1, groups2 ->
      Map.merge(groups1, groups2, fn _key, {aggregation_data1, noise_layers1}, {aggregation_data2, noise_layers2} ->
        aggregation_data = aggregation_sub_module.merge_aggregation_data(aggregation_data1, aggregation_data2)
        noise_layers = NoiseLayer.merge_accumulators(noise_layers1, noise_layers2)
        {aggregation_data, noise_layers}
      end)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # We have two anonymizing aggregation algorithms: one that uses user ids and one that uses statistics (no-uid).
  # This function returns the sub-module responsible for implementing the algorithm-specific pipeline steps.
  defp aggregation_sub_module(%Query{anonymization_type: :statistics}), do: Statistics
  defp aggregation_sub_module(_query), do: UserId

  defp init_anonymizers(grouped_rows, query) do
    Logger.debug("Initializing anonymizers ...")

    Enum.map(grouped_rows, fn {property, {aggregation_data, noise_layers}} ->
      anonymizers = init_bucket_anonymizers(noise_layers, aggregation_data, query)
      {property, anonymizers, aggregation_data}
    end)
  end

  defp init_bucket_anonymizers(accumulated_noise_layers, aggregation_data, query) do
    default_anonymizer =
      accumulated_noise_layers
      |> NoiseLayer.filter_accumulator_for_aggregator(nil)
      |> case do
        [] ->
          # We don't have any noise layers, so we generate a generic layer composed of the user ids in the bucket.
          user_id_values = aggregation_sub_module(query).user_id_values(aggregation_data)
          NoiseLayer.accumulator_from_values(user_id_values)

        accumulated_noise_layers ->
          accumulated_noise_layers
      end
      |> Anonymizer.new()

    query.aggregators
    |> Enum.with_index()
    |> Enum.map(fn {_aggregator, index} ->
      anonymizer =
        if NoiseLayer.accumulator_references_aggregator?(accumulated_noise_layers, index),
          do: accumulated_noise_layers |> NoiseLayer.filter_accumulator_for_aggregator(index) |> Anonymizer.new(),
          else: default_anonymizer

      {index, anonymizer}
    end)
    |> Enum.into(%{default: default_anonymizer})
  end

  defp low_users_count?({_property, anonymizers, aggregation_data}, aggregator_sub_module),
    do: not Anonymizer.sufficiently_large?(anonymizers.default, aggregator_sub_module.users_count(aggregation_data))

  @spec process_low_count_users([group], Query.t()) :: [group]
  defp process_low_count_users(rows, query) do
    Logger.debug("Processing low count users ...")
    bucket_size = query |> Rows.group_expressions() |> length()
    lcf_aggregation_limit = query |> Query.lcf_buckets_aggregation_limit() |> min(bucket_size)
    aggregator_sub_module = aggregation_sub_module(query)

    # We first partition the buckets into low-count and high-count buckets.
    # Then, starting from right to left, we censor each bucket value sequentially and merge corresponding buckets.
    # We then split the merged buckets again. We keep the merged buckets that pass the low-count filter and
    # repeat the process for the next column and the new set of low-count buckets.
    # When we run out of bucket values, we drop the final low-count bucket, if any.

    {_low_count_rows, high_count_rows} =
      rows
      |> Enum.split_with(&low_users_count?(&1, aggregator_sub_module))
      |> aggregate_lcf_buckets(lcf_aggregation_limit, bucket_size, aggregator_sub_module)

    high_count_rows
  end

  defp aggregate_lcf_buckets(splitted_rows, 0, _bucket_size, _aggregator_sub_module), do: splitted_rows

  defp aggregate_lcf_buckets(splitted_rows, bucket_size, bucket_size, aggregator_sub_module),
    do: Enum.reduce((bucket_size - 1)..0, splitted_rows, &group_low_count_rows(&1, &2, aggregator_sub_module))

  defp aggregate_lcf_buckets(
         {low_count_rows, high_count_rows},
         lcf_aggregation_limit,
         bucket_size,
         aggregator_sub_module
       )
       when lcf_aggregation_limit < bucket_size do
    # censor property values from lcf partial aggregation limit until max label count
    low_count_rows =
      Enum.map(low_count_rows, fn {[group_index | property], anonymizers, users_rows} ->
        property = Enum.reduce(lcf_aggregation_limit..bucket_size, property, &List.replace_at(&2, &1, :*))
        {[group_index | property], anonymizers, users_rows}
      end)

    Enum.reduce(
      (lcf_aggregation_limit - 1)..0,
      {low_count_rows, high_count_rows},
      &group_low_count_rows(&1, &2, aggregator_sub_module)
    )
  end

  defp group_low_count_rows(column_index, {low_count_rows, high_count_rows}, aggregator_sub_module) do
    {low_count_grouped_rows, high_count_grouped_rows} =
      low_count_rows
      |> Enum.group_by(fn {[group_index | property], _anonymizers, _users_rows} ->
        [group_index | List.replace_at(property, column_index, :*)]
      end)
      |> Enum.map(&collapse_grouped_rows(&1, aggregator_sub_module))
      |> Enum.split_with(&low_users_count?(&1, aggregator_sub_module))

    {low_count_grouped_rows, high_count_grouped_rows ++ high_count_rows}
  end

  defp collapse_grouped_rows({group, grouped_rows}, aggregator_sub_module) do
    user_rows =
      grouped_rows
      |> Enum.map(fn {_group, _anonymizers, aggregation_data} -> aggregation_data end)
      |> collapse_aggregation_data(&aggregator_sub_module.merge_aggregation_data/2)

    anonymizers =
      grouped_rows
      |> Enum.map(fn {_group, anonymizers, _users_rows} -> anonymizers end)
      |> Enum.reduce(fn anonymizers1, anonymizers2 ->
        Enum.zip(anonymizers1, anonymizers2)
        |> Enum.map(fn {{tag, anonymizer1}, {tag, anonymizer2}} ->
          merged_noise_layers = NoiseLayer.merge_accumulators(anonymizer1.noise_layers, anonymizer2.noise_layers)
          {tag, Anonymizer.new(merged_noise_layers)}
        end)
        |> Enum.into(%{})
      end)

    {group, anonymizers, user_rows}
  end

  defp collapse_aggregation_data([data], _data_merger), do: data
  defp collapse_aggregation_data([data1, data2], data_merger), do: data_merger.(data1, data2)

  defp collapse_aggregation_data(list, data_merger) when is_list(list) do
    list
    |> Enum.chunk_every(2)
    |> Enum.map(&collapse_aggregation_data(&1, data_merger))
    |> collapse_aggregation_data(data_merger)
  end

  defp aggregate_groups(groups, query) do
    Logger.debug("Aggregating groups ...")
    Enum.map(groups, aggregation_sub_module(query).group_aggregator(query.aggregators))
  end

  defp make_buckets(rows, query) do
    if rows == [] && Rows.group_expressions(query) == [] do
      # If there are no results for a global aggregation, we'll produce one row.
      # All results will be `nil`-ed except for `count` which will have the value of 0.
      aggregated_values =
        Enum.map(query.aggregators, fn
          %Expression{name: "count"} -> 0
          %Expression{alias: "count", name: "sum"} -> 0
          %Expression{} -> nil
        end)

      [%{row: [_group_index = 0 | aggregated_values], occurrences: 1, unreliable: true}]
    else
      make_non_empty_buckets(rows, query)
    end
    |> Rows.extract_groups(Query.bucket_columns(query), query)
  end

  defp make_non_empty_buckets(rows, %Query{implicit_count?: false}) do
    Logger.debug("Making explicit buckets ...")

    Enum.map(rows, fn {users_count, row} ->
      %{row: row, occurrences: 1, unreliable: unreliable_bucket?(users_count)}
    end)
  end

  defp make_non_empty_buckets(rows, %Query{implicit_count?: true} = query) do
    Logger.debug("Making implicit buckets ...")

    count_index = (query |> Rows.group_expressions() |> length()) + 1

    Enum.map(rows, fn {users_count, row} ->
      %{row: row, occurrences: Enum.at(row, count_index), unreliable: unreliable_bucket?(users_count)}
    end)
  end

  @users_count_reliability_threshold 15
  defp unreliable_bucket?(users_count), do: users_count < @users_count_reliability_threshold

  defp finalize_buckets(buckets, query) do
    bucket_columns = Query.bucket_columns(query)

    buckets
    |> Cloak.Query.Sorter.order_rows(bucket_columns, query.order_by)
    |> offset(query.offset)
    |> limit(query.limit)
    |> drop_non_selected_columns(bucket_columns, query.columns)
  end

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

  defp grouping_sets_count([]), do: 1
  defp grouping_sets_count(grouping_sets), do: Enum.count(grouping_sets)
end
