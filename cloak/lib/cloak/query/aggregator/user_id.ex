defmodule Cloak.Query.Aggregator.UserId do
  @moduledoc "Inner module for implementing user_id-based data aggregation."

  alias Cloak.DataSource
  alias Cloak.Sql.{Query, Expression, NoiseLayer}
  alias Cloak.Query.{Anonymizer, Rows}

  @type user_id :: DataSource.field()
  @type t :: %{user_id => DataSource.row()}

  @typep values :: [DataSource.field() | :*]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Processes the query before the start of the aggregation pipeline."
  @spec pre_process(Query.t()) :: Query.t()
  def pre_process(query), do: query

  @doc "Returns the number of distinct users in the aggregation data."
  @spec users_count(t) :: pos_integer()
  def users_count(aggregation_data) when is_map(aggregation_data), do: Enum.count(aggregation_data)

  @doc "Returns the function for digesting the data for a group."
  @spec group_updater(NoiseLayers.processed(), Query.t()) :: Rows.group_updater()
  def group_updater(processed_noise_layers, query) do
    {per_user_aggregators, aggregated_columns} =
      query.aggregators
      |> Enum.map(&per_user_aggregator_and_column/1)
      |> Enum.uniq()
      |> Enum.unzip()

    default_accumulators = List.duplicate(nil, Enum.count(aggregated_columns))

    fn {user_rows, noise_accumulator}, row ->
      [user_id | _rest] = row
      values = Enum.map(aggregated_columns, &Expression.value(&1, row))

      user_rows =
        user_rows
        |> Map.put_new(user_id, default_accumulators)
        |> Map.update!(user_id, &aggregate_values(values, &1, per_user_aggregators))

      noise_accumulator = NoiseLayer.accumulate(processed_noise_layers, noise_accumulator, row)

      {user_rows, noise_accumulator}
    end
  end

  @doc "Called to merge two aggregation accumulators for the same group into one."
  @spec merge_aggregation_data(t, t) :: t
  def merge_aggregation_data(users_rows1, users_rows2) when is_map(users_rows1) and is_map(users_rows2),
    do:
      Map.merge(users_rows1, users_rows2, fn _user, columns1, columns2 ->
        columns1 |> Enum.zip(columns2) |> Enum.map(&merge_accumulators/1)
      end)

  @doc "Returns the anonymizing aggregator for a group."
  @spec group_aggregator([Expression.t()]) :: ({values, Anonymizer.t(), t} -> {pos_integer, values})
  def group_aggregator(aggregators) do
    # Only unique per-user aggregators are computed, so we need to compute the index
    # of the aggregator into the per-user aggregated value list.
    per_user_aggregators_and_columns =
      aggregators
      |> Enum.map(&per_user_aggregator_and_column/1)
      |> Enum.uniq()

    indexed_aggregators =
      Enum.map(aggregators, fn aggregator ->
        per_user_aggregator_and_column = per_user_aggregator_and_column(aggregator)

        values_index =
          Enum.find_index(
            per_user_aggregators_and_columns,
            &(&1 == per_user_aggregator_and_column)
          )

        {values_index, aggregator}
      end)

    &aggregate_group(&1, indexed_aggregators)
  end

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

  defp per_user_aggregator(%Expression{function_args: [{:distinct, _column}]}), do: :set
  defp per_user_aggregator(%Expression{function: "count"}), do: :count
  defp per_user_aggregator(%Expression{function: "count_noise"}), do: :count
  defp per_user_aggregator(%Expression{function: "sum"}), do: :sum
  defp per_user_aggregator(%Expression{function: "sum_noise"}), do: :sum
  defp per_user_aggregator(%Expression{function: "avg"}), do: :avg
  defp per_user_aggregator(%Expression{function: "avg_noise"}), do: :avg
  defp per_user_aggregator(%Expression{function: "variance"}), do: :variance
  defp per_user_aggregator(%Expression{function: "variance_noise"}), do: :variance
  defp per_user_aggregator(%Expression{function: "min"}), do: :min
  defp per_user_aggregator(%Expression{function: "max"}), do: :max
  defp per_user_aggregator(%Expression{function: "median"}), do: :list

  defp aggregate_value(:count, _value, nil), do: 1
  defp aggregate_value(:count, _value, count), do: count + 1
  defp aggregate_value(:sum, value, nil), do: value
  defp aggregate_value(:sum, value, sum), do: sum + value
  defp aggregate_value(:avg, value, nil), do: {:avg, value, 1}
  defp aggregate_value(:avg, value, {:avg, sum, count}), do: {:avg, sum + value, count + 1}
  defp aggregate_value(:variance, value, nil), do: {:variance, value, value * value, 1}

  defp aggregate_value(:variance, value, {:variance, sum, sum_sqrs, count}),
    do: {:variance, sum + value, sum_sqrs + value * value, count + 1}

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

  defp merge_accumulators({%{__struct__: MapSet} = value1, %{__struct__: MapSet} = value2}),
    # distinct accumulators
    # using MapSet.new for each value to satisfy dialyzer
    do: MapSet.union(MapSet.new(value1), MapSet.new(value2))

  defp merge_accumulators({{:avg, value1a, value1b}, {:avg, value2a, value2b}}),
    do: {:avg, value1a + value2a, value1b + value2b}

  defp merge_accumulators({{:variance, value1a, value1b, value1c}, {:variance, value2a, value2b, value2c}}),
    do: {:variance, value1a + value2a, value1b + value2b, value1c + value2c}

  defp merge_accumulators({{:min, value1}, {:min, value2}}), do: {:min, min(value1, value2)}
  defp merge_accumulators({{:max, value1}, {:max, value2}}), do: {:max, max(value1, value2)}

  defp aggregated_column(%Expression{function_args: [:*]}), do: Expression.constant(nil, :*)
  defp aggregated_column(%Expression{function_args: [{:distinct, column}]}), do: column
  defp aggregated_column(%Expression{function_args: [column]}), do: column

  defp per_user_aggregator_and_column(aggregator), do: {per_user_aggregator(aggregator), aggregated_column(aggregator)}

  defp aggregate_group({property, anonymizer, users_rows}, indexed_aggregators) do
    users_count = Anonymizer.noisy_count(anonymizer, Enum.count(users_rows))

    aggregation_results =
      Enum.map(indexed_aggregators, fn
        {_values_index, %Expression{function: "count", function_args: [{:distinct, %Expression{user_id?: true}}]}} ->
          users_count

        {_values_index, %Expression{function: "count_noise", function_args: [{:distinct, %Expression{user_id?: true}}]}} ->
          Anonymizer.noise_amount(1, anonymizer)

        {values_index, aggregator} ->
          users_rows
          |> Stream.map(fn {_user, row_values} -> Enum.at(row_values, values_index) end)
          |> Enum.reject(&is_nil/1)
          |> preprocess_for_aggregation(aggregator)
          |> aggregate_by(aggregator.alias || aggregator.function, aggregator.type, anonymizer)
      end)

    {users_count, property ++ aggregation_results}
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

  defp aggregate_by(aggregation_data, "variance", _type, anonymizer) do
    {variance, _noise_sigma} = Anonymizer.variance(anonymizer, aggregation_data)
    variance
  end

  defp aggregate_by(aggregation_data, "variance_noise", _type, anonymizer) do
    {_variance, noise_sigma} = Anonymizer.variance(anonymizer, aggregation_data)
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
end
