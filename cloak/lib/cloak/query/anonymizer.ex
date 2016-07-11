defmodule Cloak.Query.Anonymizer do
  @moduledoc """
  Module for anonymized aggregation of data.

  This module can be used to produce aggregated values, such as counts, sums or averages,
  in a privacy-preserving form. The produced results are approximations of the real values,
  with constant noise added and removal of outliers. The anonymization parameters can
  be configured through the `:anonymizer` section of OTP application environment.

  The generated noise is deterministic for the same set of users. For example,
  calling `Anonymizer.new(users) |> Anonymizer.sum(values)` will always give the
  same result for the same set of users and values, while it may differ for another
  set of users even if the values are the same.

  All functions need the state of the noise generator. This state is used to produce
  the next noisy number. Consequently, calling the same function with the same input
  but a different state may produce a different value.

  For example, let's say we have the following code:

  ```
  initial_anonymizer = Anonymizer.new(users)
  sum = Anonymizer.sum(initial_anonymizer, values)
  ```

  At this point, calling `Anonymizer.sum(new_anonymizer, values)` might return a
  different sum. However, calling `Anonymizer.sum(initial_anonymizer, values)`
  would always return the same value.

  For a description of the way the aggregation is performed, see `docs/anonymization.md`.
  """

  @opaque t :: %{
    rng: :rand.state
  }

  import Kernel, except: [max: 2]


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Creates a noise generator from a collection of unique user ids.

  This function takes either a `MapSet` containing user ids, or a map where
  keys are user ids. Such types ensure that user ids are unique.
  """
  @spec new(MapSet.t | %{String.t => any}) :: t
  def new(%MapSet{} = users) do
    new_instance(users)
  end
  def new(%{} = users_map) do
    new_instance(Map.keys(users_map))
  end

  @doc """
  Returns a `{boolean, anonymizer}` tuple, where the boolean value is
  true if the passed bucket size is sufficiently large to be reported.

  Sufficiently large means:

  1. Greater than count_absolute_lower_bound
  2. A noised version of the count is greater than count_soft_lower_bound

  See config/config.exs for the parameters of the distribution used. The PRNG is seeded based
  on the user list provided, giving the same answer every time for the given list of users.
  """
  @spec sufficiently_large?(t, non_neg_integer) :: {boolean, t}
  def sufficiently_large?(anonymizer, count) do
    {noisy_lower_bound, anonymizer} = add_noise(anonymizer, config(:count_soft_lower_bound))
    noisy_lower_bound = Kernel.max(round(noisy_lower_bound), config(:count_absolute_lower_bound))
    {count > noisy_lower_bound, anonymizer}
  end

  @doc "Computes the noisy count of all values in rows, where each row is an enumerable."
  @spec count(t, Enumerable.t) :: non_neg_integer
  def count(anonymizer, rows) do
    {count, _anonymizer} = sum_positives(anonymizer, rows, &Enum.count(&1))
    count |> round() |> Kernel.max(config(:count_absolute_lower_bound))
  end

  @doc "Computes the noisy sum of all values in rows, where each row is an enumerable of numbers."
  @spec sum(t, Enumerable.t) :: number
  def sum(anonymizer, rows) do
    {positives_sum, anonymizer} = sum_positives(anonymizer, rows, &Enum.sum(&1))
    {negatives_sum, _anonymizer} = sum_positives(anonymizer, rows, &-Enum.sum(&1))
    first_value = rows |> Enum.at(0) |> Enum.sum()
    maybe_round_result(positives_sum - negatives_sum, first_value)
  end

  @doc "Computes the noisy minimum value of all values in rows, where each row is an enumerable of numbers."
  @spec min(t, Enumerable.t) :: number | nil
  def min(anonymizer, rows) do
    # we use the fact that min([value]) = -max([-value])
    -get_max(anonymizer, rows, &(-Enum.min(&1)))
  end

  @doc "Computes the noisy maximum value of all values in rows, where each row is an enumerable of numbers."
  @spec max(t, Enumerable.t) :: number | nil
  def max(anonymizer, rows) do
    get_max(anonymizer, rows, &Enum.max/1)
  end

  @doc "Computes the average value of all values in rows, where each row is an enumerable of numbers."
  @spec avg(t, Enumerable.t) :: float
  def avg(anonymizer, rows), do: sum(anonymizer, rows) / count(anonymizer, rows)

  @doc "Computes the standard deviation of all values in rows, where each row is an enumerable of numbers."
  @spec stddev(t, Enumerable.t) :: float
  def stddev(anonymizer, rows) do
    real_sum = rows |> Stream.map(&Enum.sum(&1)) |> Enum.sum()
    real_count = rows |> Stream.map(&Enum.count(&1)) |> Enum.sum()
    real_avg = real_sum / real_count

    get_variance = fn (value) -> (real_avg - value) * (real_avg - value) end
    variances = Stream.map(rows, &Stream.map(&1, get_variance))

    :math.sqrt(avg(anonymizer, variances))
  end

  @doc "Computes the median value of all values in rows, where each row is an enumerable of numbers."
  @spec median(t, Enumerable.t) :: number | nil
  def median(anonymizer, rows) do
    values =
      rows
      |> Stream.with_index()
      |> Stream.flat_map(fn ({row, user_index}) -> Stream.map(row, &{user_index, &1}) end)
      |> Enum.sort_by(fn ({_user_index, value}) -> value end)

    top_count = config(:top_count)
    {noisy_above_count, anonymizer} = add_noise(anonymizer, top_count)
    noisy_above_count = round(noisy_above_count)
    {noisy_below_count, _anonymizer} = add_noise(anonymizer, top_count)
    noisy_below_count = round(noisy_below_count)

    middle = round((Enum.count(values) - 1) / 2)
    {bottom_values, [{_middle_user_index, middle_value} | top_values]} = Enum.split(values, middle - 1)
    above_values = top_values |> take_values_from_distinct_users(noisy_above_count)
    below_values = bottom_values |> Enum.reverse() |> take_values_from_distinct_users(noisy_below_count)
    middle_values = below_values ++ [middle_value] ++ above_values

    middle_values_count = Enum.count(middle_values)
    case  noisy_below_count + noisy_above_count + 1 do
      ^middle_values_count ->
        median = Enum.sum(middle_values) / middle_values_count
        maybe_round_result(median, Enum.at(middle_values, 0))
      _ -> nil
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp config(name), do: :cloak_conf.get_val(:anonymizer, name)

  defp new_instance(unique_users) do
    %{rng: :rand.seed(:exsplus, seed(unique_users))}
  end

  defp seed(unique_users) do
    unique_users
    |> Enum.reduce(compute_hash(""), fn (user, accumulator) ->
      user
      |> to_string()
      |> compute_hash()
      # since the list is not sorted, using `xor` (which is commutative) will get us consistent results
      |> :crypto.exor(accumulator)
    end)
    |> binary_to_seed()
  end

  defp compute_hash(binary), do: :crypto.hash(:md4, binary)

  defp binary_to_seed(binary) do
    <<a::32, b::32, c::64>> = binary
    {a, b, c}
  end

  # Produces a gaussian distributed random number with given mean and standard deviation.
  defp add_noise(%{rng: rng} = anonymizer, {mean, sd}) do
    {rand1, rng} = :rand.uniform_s(rng)
    {rand2, rng} = :rand.uniform_s(rng)
    noise = sd * gauss(rand1, rand2)
    {mean + noise, %{anonymizer | rng: rng}}
  end

  # Generates a gaussian distributed random number from two
  # uniform distributed numbers by the Box-Muller method.
  defp gauss(rand1, rand2) when rand1 > 0 do
    r1 = -2.0 * :math.log(rand1)
    r2 = 2.0 * :math.pi() * rand2
    :math.sqrt(r1) * :math.cos(r2)
  end

  # Computes the noisy sum of a collection of positive numbers.
  defp sum_positives(anonymizer, rows, row_accumulator) do
    outlier_count = config(:dropped_outliers_count)
    {noisy_top_count, anonymizer} = add_noise(anonymizer, config(:top_count))
    noisy_top_count = round(noisy_top_count)

    {sum, top_average} = get_positives_sum_and_top_average(rows, outlier_count, noisy_top_count, row_accumulator)

    {noisy_outlier_count, anonymizer} = add_noise(anonymizer, {outlier_count, config(:sum_noise_sigma)})
    sum = sum + noisy_outlier_count * top_average
    {sum, anonymizer}
  end

  # Round the final result of an aggregator depending on the type of aggregated values.
  defp maybe_round_result(result, value) when is_integer(value), do: round(result)
  defp maybe_round_result(result, value) when is_float(value), do: result

  defp take_values_from_distinct_users(user_values, amount) do
    user_values
    |> Stream.dedup_by(fn({user, _value}) -> user end)
    |> Stream.map(fn({_user, value}) -> value end)
    |> Enum.take(amount)
  end

  # Given a list of rows and a row accumulator functor, this method will drop the
  # biggest outliers and rows with negative values, and, for the remaining rows,
  # will return the sum and average value of the top rows.
  defp get_positives_sum_and_top_average(rows, outliers_count, top_amount, row_accumulator) do
    {sum, top_length, top_values} = Enum.reduce(rows, {0, 0, []}, fn
      (row, {sum, top_length, top}) when top_length <= outliers_count + top_amount ->
        row_value = row_accumulator.(row)
        case row_value >= 0 do
          true -> {sum + row_value, top_length + 1, Enum.sort([row_value | top])}
          false -> {sum, top_length, top}
        end
      (row, {sum, top_length, [top_smallest | top_rest] = top}) ->
        row_value = Kernel.max(row_accumulator.(row), 0)
        case row_value > top_smallest do
          true -> {sum + row_value, top_length, Enum.sort([row_value | top_rest])}
          false -> {sum + row_value, top_length, top}
        end
    end)
    {outliers, top_values} = top_values |> Enum.reverse() |> Enum.split(outliers_count) # drop outliers
    top_length = top_length - outliers_count
    top_average = case top_length do
      0 -> 0
      _ -> Enum.sum(top_values) / top_length
    end
    {sum - Enum.sum(outliers), top_average}
  end

  # Given a list of rows and a row accumulator functor, this method will drop the biggest outliers and
  # will return the average value of the top remaining rows, if enough rows are available.
  defp get_max(anonymizer, rows, row_accumulator) do
    {noisy_top_count, _anonymizer} = add_noise(anonymizer, config(:top_count))
    noisy_top_count = round(noisy_top_count)
    outliers_count = config(:dropped_outliers_count)

    {top_length, top_values} = Enum.reduce(rows, {0, []}, fn
      (row, {top_length, top}) when top_length <= outliers_count + noisy_top_count ->
        row_value = row_accumulator.(row)
        {top_length + 1, Enum.sort([row_value | top])}
      (row, {top_length, [top_smallest | top_rest] = top}) ->
        row_value = row_accumulator.(row)
        case row_value > top_smallest do
          true -> {top_length, Enum.sort([row_value | top_rest])}
          false -> {top_length, top}
        end
    end)

    case top_length < noisy_top_count + outliers_count do
      true -> nil # there weren't enough values in the input to anonymize the result
      false ->
        {_outliers, top_values} = top_values |> Enum.reverse() |> Enum.split(outliers_count) # drop outliers
        top_length = top_length - outliers_count
        top_average = Enum.sum(top_values) / top_length
        maybe_round_result(top_average, Enum.at(top_values, 0))
    end
  end
end
