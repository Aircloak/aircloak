defmodule Cloak.Query.Aggregator.Statistics do
  @moduledoc "Inner module for implementing statistics-based data aggregation."

  alias Cloak.DataSource
  alias Cloak.Sql.{Query, Expression, NoiseLayer}
  alias Cloak.Query.{Anonymizer, Rows}

  @type user_id :: DataSource.field()
  @type bucket_statistics :: [pos_integer() | MapSet.t()]
  @type aggregation_statistics :: [number | nil]
  @type t :: [bucket_statistics | aggregation_statistics]

  @typep values :: [DataSource.field() | :*]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Processes the query before the start of the aggregation pipeline."
  @spec pre_process(Query.t()) :: Query.t()
  def pre_process(query) do
    [%Expression{name: "__ac_min_uid"}, %Expression{name: "__ac_max_uid"} | aggregators] = query.aggregators
    %Query{query | aggregators: aggregators}
  end

  @doc "Returns the number of distinct users in the aggregation data."
  @spec users_count(t) :: pos_integer()
  def users_count(aggregation_data) when is_list(aggregation_data) do
    [bucket_statistics | _] = aggregation_data
    [count_duid | _] = bucket_statistics
    count_duid
  end

  @doc "Returns the function for digesting the data for a group."
  @spec group_updater(NoiseLayer.processed(), Query.t()) :: Rows.group_updater()
  def group_updater(processed_noise_layers, query) do
    [min_uid_column, max_uid_column | per_user_aggregators] = query.aggregators
    [count_duid_column | _] = query.db_columns

    bucket_statistics_columns = [count_duid_column, min_uid_column, max_uid_column]
    aggregation_statistics_columns = Enum.map(per_user_aggregators, &extract_args(&1.function_args))
    statistics_columns = [bucket_statistics_columns | aggregation_statistics_columns]

    fn {accumulated_statistics, default_noise_layers}, row ->
      noise_accumulator = NoiseLayer.accumulate(processed_noise_layers, default_noise_layers, row)

      [[count_duid, min_uid, max_uid] | aggregation_statistics] =
        Enum.map(statistics_columns, fn columns_group ->
          Enum.map(columns_group, &Expression.value(&1, row))
        end)

      statistics = [[count_duid, MapSet.new([min_uid, max_uid])] | aggregation_statistics]
      {merge_aggregation_data(accumulated_statistics, statistics), noise_accumulator}
    end
  end

  @doc "Called to merge two aggregation accumulators for the same group into one."
  @spec merge_aggregation_data(t, t) :: t
  def merge_aggregation_data(%{}, statistics) when is_list(statistics), do: statistics

  def merge_aggregation_data(statistics1, statistics2) when is_list(statistics1) and is_list(statistics2) do
    [[count_duid1, uids1] | aggregation_statistics1] = statistics1
    [[count_duid2, uids2] | aggregation_statistics2] = statistics2

    uids = MapSet.union(uids1, uids2)
    {min_uid1, max_uid1} = Enum.min_max(uids1)
    {min_uid2, max_uid2} = Enum.min_max(uids2)

    count_adder = fn count1, count2 ->
      cond do
        max_uid1 < min_uid2 or max_uid2 < min_uid1 -> count1 + count2
        max_uid1 == min_uid2 or max_uid2 == min_uid1 -> count1 + count2 - 1
        # Estimate the combined count as maximum count plus a quarter of
        # minimum count (since some collisions can occur).
        true -> max(max(count1, count2) + div(min(count1, count2), 4), Enum.count(uids))
      end
    end

    count_duid = count_adder.(count_duid1, count_duid2)
    bucket_statistics = [count_duid, uids]

    aggregation_statistics =
      Enum.zip(aggregation_statistics1, aggregation_statistics2)
      |> Enum.map(fn {column_statistics1, column_statistics2} ->
        merge_aggregation_statistics(count_adder, column_statistics1, column_statistics2)
      end)

    [bucket_statistics | aggregation_statistics]
  end

  @doc "Returns the anonymizing aggregator for a group."
  @spec group_aggregator([Expression.t()]) :: ({values, Anonymizer.t(), t} -> {pos_integer, values})
  def group_aggregator(aggregators), do: &aggregate_group(&1, aggregators)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp merge_aggregation_statistics(count_adder, [count1], [count2]), do: [count_adder.(count1, count2)]

  defp merge_aggregation_statistics(_count_adder, [0, nil, nil, nil, nil], statistics2), do: statistics2
  defp merge_aggregation_statistics(_count_adder, statistics1, [0, nil, nil, nil, nil]), do: statistics1

  defp merge_aggregation_statistics(count_adder, statistics1, statistics2) do
    [count1, sum1, min1, max1, stddev1] = statistics1
    [count2, sum2, min2, max2, stddev2] = statistics2

    count = count_adder.(count1, count2)

    avg1 = sum1 / count1
    avg2 = sum2 / count2
    avg = (sum1 + sum2) / count

    stddev1 = stddev1 || 0
    stddev2 = stddev2 || 0
    # use the formula `sd(v) = sqrt(sum(v^2)/count - avg(v)^2)` to extract sum of squares
    sum_sqrs1 = (stddev1 * stddev1 + avg1 * avg1) * count1
    sum_sqrs2 = (stddev2 * stddev2 + avg2 * avg2) * count2
    sum_sqrs = sum_sqrs1 + sum_sqrs2

    min = min(min1, min2)
    max = max(max1, max2)
    sum = sum1 + sum2
    # use the formula `sd(v) = sqrt(sum(v^2)/count - avg(v)^2)` to combine standard deviations
    stddev = (sum_sqrs / count - avg * avg) |> Kernel.max(0) |> :math.sqrt() |> Kernel.min((max - min) / 2)

    [count, sum, min, max, stddev]
  end

  defp aggregate_group({property, anonymizer, statistics}, aggregators) do
    [bucket_statistics | aggregation_statistics] = statistics
    [count_duid | _] = bucket_statistics

    users_count = Anonymizer.noisy_count(anonymizer, count_duid)

    aggregation_results =
      Enum.zip(aggregators, aggregation_statistics)
      |> Enum.map(fn
        {%Expression{function: "count", function_args: [{:distinct, %Expression{user_id?: true}}]}, [^count_duid]} ->
          users_count

        {%Expression{function: "count_noise", function_args: [{:distinct, %Expression{user_id?: true}}]}, [^count_duid]} ->
          1

        {_aggregator, [0, nil, nil, nil, nil]} ->
          nil

        {aggregator, [count, sum, min, max, stddev]} ->
          avg = sum / count
          statistics = {count, sum, min, max, avg, stddev}

          {noisy_sum, noisy_min, noisy_max, noisy_sum_sigma} = Anonymizer.noisy_statistics(anonymizer, statistics)

          case aggregator.alias do
            "count" -> (noisy_sum || 0) |> round() |> Kernel.max(Anonymizer.config(:low_count_absolute_lower_bound))
            "sum" -> float_to_type(noisy_sum, aggregator.type)
            "min" -> float_to_type(noisy_min, aggregator.type)
            "max" -> float_to_type(noisy_max, aggregator.type)
            "count_noise" -> noisy_sum_sigma
            "sum_noise" -> noisy_sum_sigma
          end
      end)

    {users_count, property ++ aggregation_results}
  end

  defp float_to_type(nil, _), do: nil
  defp float_to_type(value, :integer), do: round(value)
  defp float_to_type(value, :real), do: value

  defp extract_args(args), do: Enum.map(args, &extract_arg/1)

  defp extract_arg({:distinct, arg}), do: arg
  defp extract_arg(arg), do: arg
end
