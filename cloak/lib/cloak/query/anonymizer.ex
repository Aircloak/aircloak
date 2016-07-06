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

  For a description of the way the aggregation is performed, see `docs/anonymizer.md`.
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
    values = Stream.map(rows, &Enum.count(&1))
    {count, _anonymizer} = sum_positives(anonymizer, values)
    Kernel.max(round(count), config(:count_absolute_lower_bound))
  end

  @doc "Computes the noisy sum of all values in rows, where each row is an enumerable of numbers."
  @spec sum(t, Enumerable.t) :: number
  def sum(anonymizer, rows) do
    values = Stream.map(rows, &Enum.sum/1)
    positives = Stream.filter(values, &(&1 >= 0))
    negatives = Stream.filter_map(values, &(&1 < 0), &-/1)

    {positives_sum, anonymizer} = sum_positives(anonymizer, positives)
    {negatives_sum, _anonymizer} = sum_positives(anonymizer, negatives)

    positives_sum - negatives_sum
  end

  @doc "Computes the noisy minimum value of all values in rows, where each row is an enumerable of numbers."
  @spec min(t, Enumerable.t) :: number | nil
  def min(anonymizer, rows) do
    values = rows |> Stream.map(&Enum.min/1) |> Enum.sort(&(&1 < &2))
    {_outliers, values} = Enum.split(values, config(:dropped_outliers_count)) # drop outliers
    anonymizer |> top_average(values) |> maybe_round_result(values)
  end

  @doc "Computes the noisy maximum value of all values in rows, where each row is an enumerable of numbers."
  @spec max(t, Enumerable.t) :: number | nil
  def max(anonymizer, rows) do
    values = rows |> Stream.map(&Enum.max/1) |> Enum.sort(&(&1 > &2))
    {_outliers, values} = Enum.split(values, config(:dropped_outliers_count)) # drop outliers
    anonymizer |> top_average(values) |> maybe_round_result(values)
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
        maybe_round_result(median, middle_values)
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

  # Computes the noisy average of the top of the collection.
  defp top_average(anonymizer, values) do
    {noisy_top_count, _anonymizer} = add_noise(anonymizer, config(:top_count))
    noisy_top_count = round(noisy_top_count)
    top = Enum.take(values, noisy_top_count)
    case Enum.count(top) do
      ^noisy_top_count -> Enum.sum(top) / noisy_top_count
      _ -> nil # there weren't enough values in the input to anonymize the result
    end
  end

  # Computes the noisy sum of a collection of positive numbers.
  defp sum_positives(anonymizer, values) do
    values = Enum.sort(values, &(&1 > &2))

    outlier_count = config(:dropped_outliers_count)
    {_outliers, values} = Enum.split(values, outlier_count) # drop outliers

    {noisy_top_count, anonymizer} = add_noise(anonymizer, config(:top_count))
    noisy_top_count = round(noisy_top_count)
    top_average = case Enum.take(values, noisy_top_count) do
      [] -> 0
      top -> Enum.sum(top) / Enum.count(top)
    end

    {noisy_outlier_count, anonymizer} = add_noise(anonymizer, {outlier_count, config(:sum_noise_sigma)})
    sum = noisy_outlier_count * top_average + Enum.sum(values)
    {maybe_round_result(sum, values), anonymizer}
  end

  # Round the final result of an aggregator depending on the type of aggregated values.
  defp maybe_round_result(nil, _values), do: nil
  defp maybe_round_result(result, []), do: round(result)
  defp maybe_round_result(result, [value | _rest]) when is_integer(value), do: round(result)
  defp maybe_round_result(result, [value | _rest]) when is_float(value), do: result

  defp take_values_from_distinct_users(user_values, amount) do
    user_values
    |> Stream.dedup_by(fn({user, _value}) -> user end)
    |> Stream.map(fn({_user, value}) -> value end)
    |> Enum.take(amount)
  end
end
