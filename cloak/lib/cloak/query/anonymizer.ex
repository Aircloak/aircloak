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
    rngs: [:rand.state],
  }

  import Kernel, except: [max: 2]


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Creates a noise generator from a collection of sets of values representing noise layers.

  Each noise layer must be either a `MapSet`, or a map (in which case the keys are used).
  Such types ensure that user ids are unique.
  """
  @spec new([MapSet.t | %{String.t => any}]) :: t
  def new(layers), do:
    %{rngs: Enum.map(layers, &:rand.seed(:exsplus, seed(&1)))}


  @doc """
  Returns a `{boolean, anonymizer}` tuple, where the boolean value is
  true if the passed bucket size is sufficiently large to be reported.

  Sufficiently large means the bucket size is greater or equal to the:

  1. low_count_absolute_lower_bound
  2. noisy low_count_soft_lower_bound

  See config/config.exs for the parameters of the distribution used. The PRNG is seeded based
  on the user list provided, giving the same answer every time for the given list of users.
  """
  @spec sufficiently_large?(t, non_neg_integer) :: {boolean, t}
  def sufficiently_large?(anonymizer, count) do
    {noisy_lower_bound, anonymizer} = noisy_lower_bound(anonymizer)
    {count >= noisy_lower_bound, anonymizer}
  end

  @doc """
  Returns a lower bound distributed as described in `sufficently_large?`.
  """
  @spec noisy_lower_bound(t) :: {non_neg_integer, t}
  def noisy_lower_bound(anonymizer) do
    {noisy_lower_bound, anonymizer} = add_noise(anonymizer, config(:low_count_soft_lower_bound))
    noisy_lower_bound = Kernel.max(round(noisy_lower_bound), config(:low_count_absolute_lower_bound))
    {noisy_lower_bound, anonymizer}
  end

  @doc "Computes the noisy count and noise sigma of all values in rows, where each row is an enumerable."
  @spec count(t, Enumerable.t) :: {non_neg_integer, non_neg_integer}
  def count(anonymizer, rows) do
    {count, noise_sigma, _anonymizer} = sum_positives(anonymizer, rows)
    count = count |> round() |> Kernel.max(config(:low_count_absolute_lower_bound))
    {_noise_mean_lower_bound, noise_sigma_lower_bound} = config(:outliers_count)
    noise_sigma = noise_sigma |> round() |> Kernel.max(noise_sigma_lower_bound)
    {count, noise_sigma}
  end

  @doc "Computes the noisy sum and noise sigma of all values in rows, where each row is an enumerable of numbers."
  @spec sum(t, Enumerable.t) :: {number, number}
  def sum(anonymizer, rows) do
    {positives_sum, positives_noise_sigma, anonymizer} = sum_positives(anonymizer, rows)
    {negatives_sum, negatives_noise_sigma, _anonymizer} = sum_positives(anonymizer, Stream.map(rows, &-/1))
    noise_sigma = sum_noise_sigmas(positives_noise_sigma, negatives_noise_sigma)
    sum = positives_sum - negatives_sum
    {sum, noise_sigma}
  end

  @doc "Computes the noisy minimum value of all values in rows, where each row is an enumerable of numbers."
  @spec min(t, Enumerable.t) :: number | nil
  def min(anonymizer, rows) do
    # we use the fact that min([value]) = -max([-value])
    case get_max(anonymizer, rows, & -Enum.min(&1)) do
      nil -> nil
      value -> -value
    end
  end

  @doc "Computes the noisy maximum value of all values in rows, where each row is an enumerable of numbers."
  @spec max(t, Enumerable.t) :: number | nil
  def max(anonymizer, rows) do
    get_max(anonymizer, rows, &Enum.max/1)
  end

  @doc """
    Computes the noisy average value and noise sigma of all values in rows,
    where each row is an enumerable of numbers.
  """
  @spec avg(t, Enumerable.t) :: {float, float}
  def avg(anonymizer, rows) do
    {sum, sum_noise_sigma} = sum(anonymizer, Stream.map(rows, fn ({sum, _count}) -> sum end))
    {count, _count_noise_sigma} = count(anonymizer, Stream.map(rows, fn ({_sum, count}) -> count end))
    {sum / count, sum_noise_sigma / count}
  end

  @doc """
    Computes the noisy standard deviation and noise sigma of all values in rows,
    where each row is an enumerable of numbers.
  """
  @spec stddev(t, Enumerable.t) :: {float, float}
  def stddev(anonymizer, rows) do
    {sum, count} = Enum.reduce(rows, {0, 0}, fn ({sum, _sum_sqrs, count}, {acc_sum, acc_count}) ->
      {acc_sum + sum, acc_count + count}
    end)
    mean = sum / count
    variances = Stream.map(rows, fn ({sum, sum_sqrs, count}) ->
      {sum_sqrs + mean * (count * mean - 2 * sum), count}
    end)
    {avg_variance, noise_sigma_variance} = avg(anonymizer, variances)
    {:math.sqrt(abs(avg_variance)), :math.sqrt(noise_sigma_variance)}
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
    {noisy_below_count, anonymizer} = add_noise(anonymizer, top_count)
    noisy_below_count = round(noisy_below_count)

    middle = round((Enum.count(values) - 1) / 2)
    {bottom_values, [{_middle_user_index, middle_value} | top_values]} = Enum.split(values, middle - 1)
    above_values = top_values |> take_values_from_distinct_users(noisy_above_count)
    below_values = bottom_values |> Enum.reverse() |> take_values_from_distinct_users(noisy_below_count)
    middle_values = below_values ++ [middle_value] ++ above_values

    middle_values_count = Enum.count(middle_values)
    case noisy_below_count + noisy_above_count + 1 do
      ^middle_values_count ->
        {noised_median, _anonymizer} = noisy_average(middle_values, anonymizer)
        noised_median
      _ -> nil
    end
  end

  @doc """
    Returns a noisy version of the value passed as the parameter.
    This anonymization function is only to be used when each user
    is only represented at most once in the value.

    A good example of a valid use would be to get a noisy count of
    distinct users in a result set.

    No low count check is done, and should be separately performed
    using `sufficiently_large?/1`.
  """
  @spec noisy_count(t, integer) :: integer
  def noisy_count(anonymizer, count) do
    sigma = config(:sum_noise_sigma)
    {noisy_count, _anonymizer} = add_noise(anonymizer, {count, sigma})
    Kernel.max(round(noisy_count), config(:low_count_absolute_lower_bound))
  end

  @doc "Returns the configuration value for an anonymizer parameter."
  @spec config(atom) :: term
  def config(name), do: Application.get_env(:cloak, :anonymizer) |> Keyword.fetch!(name)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp seed(%MapSet{} = values), do: do_seed(values)
  defp seed(%{} = values), do: values |> Map.keys() |> do_seed()

  defp do_seed(unique_values) do
    unique_values
    |> Enum.reduce(compute_hash(config(:salt)), fn (user, accumulator) ->
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
  defp add_noise(%{rngs: rngs} = anonymizer, {mean, sd}) do
    {noise, rngs} = Enum.reduce(rngs, {0, []}, fn(rng, {noise, rngs}) ->
      {sample, rng} = gauss(rng)
      {noise + sample, [rng | rngs]}
    end)

    {mean + sd * noise, %{anonymizer | rngs: Enum.reverse(rngs)}}
  end

  defp gauss(rng) do
    {rand1, rng} = :rand.uniform_s(rng)
    {rand2, rng} = :rand.uniform_s(rng)
    {gauss(rand1, rand2), rng}
  end

  # Generates a gaussian distributed random number from two
  # uniform distributed numbers by the Box-Muller method.
  defp gauss(rand1, rand2) when rand1 > 0 do
    r1 = -2.0 * :math.log(rand1)
    r2 = 2.0 * :math.pi() * rand2
    :math.sqrt(r1) * :math.cos(r2)
  end

  # Computes the noisy sum of a collection of positive numbers.
  defp sum_positives(anonymizer, rows) do
    {outliers_count, anonymizer} = add_noise(anonymizer, config(:outliers_count))
    outliers_count = outliers_count |> round() |> Kernel.max(config(:min_outliers_count))
    {top_count, anonymizer} = add_noise(anonymizer, config(:top_count))
    top_count = round(top_count)
    {sum, noise_sigma_scale} = sum_positives(rows, outliers_count, top_count)
    noise_sigma = config(:sum_noise_sigma) * noise_sigma_scale
    {noisy_sum, anonymizer} = add_noise(anonymizer, {sum, noise_sigma})
    {noisy_sum, round_noise_sigma(noise_sigma), anonymizer}
  end

  defp take_values_from_distinct_users(user_values, amount) do
    user_values
    |> Stream.dedup_by(fn({user, _value}) -> user end)
    |> Stream.map(fn({_user, value}) -> value end)
    |> Enum.take(amount)
  end

  # This is a micro-optimization for inserting a value into a sorted list, by avoiding a call to Enum.sort/1.
  # Although the input for this should be very small (less than 10 items), it helps to reduce the amount
  # of garbage generated during aggregation.
  defp insert_sorted([], new_value), do: [new_value]
  defp insert_sorted([head | _tail] = list, new_value) when new_value <= head, do: [new_value | list]
  defp insert_sorted([head | tail], new_value) when new_value > head, do: [head | insert_sorted(tail, new_value)]

  # Given a list of positives values and the anonymization parameters,
  # this method will drop the rows with negative values, and, for the remaining rows,
  # will return the anonymized sum plus the required scale for the noise standard deviation.
  defp sum_positives(rows, outliers_count, top_count) do
    # The following part is written in a more convoluted way in order to do a single pass through the data.
    # It improves performance, but also reduces the amount of garbage generated, making the cloak more memory-stable.
    # The code is roughly equivalent to:
    # rows
    #   |> Enum.filter(& &1 >= 0)
    #   |> Enum.sort(& &1 > &2)
    #   |> Enum.split(outliers_count + top_count)
    {sum, count, top_length, top_values} = Enum.reduce(rows, {0, 0, 0, []}, fn
      (value, {sum, count, top_length, top}) when value < 0 ->
        {sum, count, top_length, top}
      (value, {sum, count, top_length, top}) when top_length <= outliers_count + top_count ->
        # This is the case in which the `top_values` list is not full yet and we need to add the current value to it.
        {sum, count, top_length + 1, insert_sorted(top, value)}
      (value, {sum, count, top_length, [top_smallest | top_rest] = top}) ->
        # This is the case in which our `top_values` list is full and we need to compare the
        # current `value` with the head of the list.
        if value > top_smallest do
          {sum + top_smallest, count + 1, top_length, insert_sorted(top_rest, value)}
        else
          {sum + value, count + 1, top_length, top}
        end
    end)

    if top_length > outliers_count do
      top_length  = top_length - outliers_count
      top_values_sum = top_values |> Enum.take(top_length) |> Enum.sum()
      top_average = top_values_sum / top_length
      sum = sum + top_values_sum
      count = count + top_length
      average = sum / count
      {sum + outliers_count * top_average, Kernel.max(2 * average, top_average)}
    else
      {0, 0} # We don't have enough values to return a result.
    end
  end

  # Given a list of rows and a row accumulator functor, this method will drop the biggest outliers and
  # will return the average value of the top remaining rows, if enough rows are available.
  defp get_max(anonymizer, rows, row_accumulator) do
    {outliers_count, anonymizer} = add_noise(anonymizer, config(:outliers_count))
    outliers_count = outliers_count |> round() |> Kernel.max(config(:min_outliers_count))
    {top_count, anonymizer} = add_noise(anonymizer, config(:top_count))
    top_count = round(top_count)

    {top_length, top_values} = Enum.reduce(rows, {0, []}, fn
      (row, {top_length, top}) when top_length <= outliers_count + top_count ->
        row_value = row_accumulator.(row)
        {top_length + 1, insert_sorted(top, row_value)}
      (row, {top_length, [top_smallest | top_rest] = top}) ->
        row_value = row_accumulator.(row)
        if row_value > top_smallest do
          {top_length, insert_sorted(top_rest, row_value)}
        else
          {top_length, top}
        end
    end)

    if top_length < top_count + outliers_count do
      nil # there weren't enough values in the input to anonymize the result
    else
      top_values = Enum.take(top_values, top_count)
      {noised_top_average, _anonymizer} = noisy_average(top_values, anonymizer)
      noised_top_average
    end
  end

  # Returns the average of a set of values + noise with SD of the quarter of the SD of the input values
  defp noisy_average(values, anonymizer) do
    value_count = Enum.count(values)
    average = Enum.sum(values) / value_count
    variance = (values |> Enum.map(&(&1 - average) * (&1 - average)) |> Enum.sum()) / value_count
    quarter_stddev = :math.sqrt(variance) / 4
    add_noise(anonymizer, {average, quarter_stddev})
  end

  # Rounds a value to money style increments (1, 2, 5, 10, 20, 50, 100, ...).
  defp money_round(value) when value >= 0.0 and value < 0.0001, do: 0.0
  defp money_round(value) when value >= 1.0 and value < 1.5, do: 1.0
  defp money_round(value) when value >= 1.5 and value < 3.5, do: 2.0
  defp money_round(value) when value >= 3.5 and value < 7.5, do: 5.0
  defp money_round(value) when value >= 7.5 and value < 10.0, do: 10.0
  defp money_round(value) when value >= 0.0001 and value < 1.0, do: 0.1 * money_round(10.0 * value)
  defp money_round(value) when value >= 10.0, do: 10.0 * money_round(0.1 * value)

  # Rounds the noise sigma to a value that can be provided back to the analyst.
  # For more info, see this: https://github.com/Aircloak/aircloak/issues/267.
  defp round_noise_sigma(sigma) do
    case money_round(sigma * 0.05) do
      0.0 -> 0.0
      round_step -> Float.round(sigma / round_step) * round_step
    end
  end

  defp sum_noise_sigmas(sigma1, sigma2), do: :math.sqrt(sigma1 * sigma1 + sigma2 * sigma2)
end
