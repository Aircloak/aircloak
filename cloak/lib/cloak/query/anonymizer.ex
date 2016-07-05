defmodule Cloak.Query.Anonymizer do
  @moduledoc """
  Utility module for stateful deterministic anonymization based on collection
  of unique users.

  This module can be used to produce noisy values, such as counts, sums or averages.
  The values are approximations of the real values, with some constant noise
  added. The amount of noise can be configured through the `:anonymizer` section
  of OTP application environment.

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
  def sufficiently_large?(anonymizer, real_count) do
    count_soft_lower_bound = config(:count_soft_lower_bound)
    count_soft_lower_bound_sigma = config(:count_soft_lower_bound_sigma)
    count_absolute_lower_bound = config(:count_absolute_lower_bound)
    {noisy_count, anonymizer} = add_noise(anonymizer, real_count, count_soft_lower_bound_sigma)
    {
      real_count > count_absolute_lower_bound and round(noisy_count) > count_soft_lower_bound,
      anonymizer
    }
  end

  @doc "Computes the noisy count of all values in rows, where each row is an enumerable."
  @spec count(t, Enumerable.t) :: non_neg_integer
  def count(anonymizer, rows) do
    values = Stream.map(rows, &Enum.count(&1))
    {count, _anonymizer} = sum_positives(anonymizer, values)
    Kernel.max(round(count), config(:count_absolute_lower_bound))
  end

  @doc "Computes the noisy sum of all values in rows, where each row is an enumerable of numbers."
  @spec sum(t, Enumerable.t) :: float
  def sum(anonymizer, rows) do
    values = Stream.map(rows, &Enum.sum/1)
    positives = Stream.filter(values, &(&1 >= 0))
    negatives = Stream.filter_map(values, &(&1 < 0), &-/1)

    {positives_sum, anonymizer} = sum_positives(anonymizer, positives)
    {negatives_sum, _anonymizer} = sum_positives(anonymizer, negatives)

    positives_sum - negatives_sum
  end

  @doc "Computes the noisy minimum value of all values in rows, where each row is an enumerable of numbers."
  @spec min(t, Enumerable.t) :: float
  def min(anonymizer, rows) do
    values = rows |> Stream.map(&Enum.min/1) |> Enum.sort(&(&1 < &2))
    {_outliers, values} = Enum.split(values, config(:dropped_outliers_count)) # drop outliers
    {min, _anonymizer} = top_average(anonymizer, values)
    maybe_round_result(min, values)
  end

  @doc "Computes the noisy maximum value of all values in rows, where each row is an enumerable of numbers."
  @spec max(t, Enumerable.t) :: float
  def max(anonymizer, rows) do
    values = rows |> Stream.map(&Enum.max/1) |> Enum.sort(&(&1 > &2))
    {_outliers, values} = Enum.split(values, config(:dropped_outliers_count)) # drop outliers
    {max, _anonymizer} = top_average(anonymizer, values)
    maybe_round_result(max, values)
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

  # Produces a gaussian distributed random integer with given mean and standard deviation.
  defp add_noise(%{rng: rng} = anonymizer, mu, sigma) do
    {rand1, rng} = :rand.uniform_s(rng)
    {rand2, rng} = :rand.uniform_s(rng)
    {gauss(mu, sigma, rand1, rand2), %{anonymizer | rng: rng}}
  end

  # Generates a gaussian distributed random number from two
  # uniform distributed numbers by the Box-Muller method.
  defp gauss(mu, sigma, rand1, rand2) when rand1 > 0 do
    r1 = -2.0 * :math.log(rand1)
    r2 = 2.0 * :math.pi() * rand2
    preval = :math.sqrt(r1) * :math.cos(r2)
    mu + sigma * preval
  end

  # Computes the noisy average of the top of the collection.
  defp top_average(anonymizer, []), do: {0, anonymizer}
  defp top_average(anonymizer, values) do
    top_count_mean = config(:top_count_mean)
    top_count_sigma = config(:top_count_sigma)
    {noisy_top_count, anonymizer} = add_noise(anonymizer, top_count_mean, top_count_sigma)

    top = Enum.take(values, round(noisy_top_count))
    average = Enum.sum(top) / Enum.count(top)
    {average, anonymizer}
  end

  # Computes the noisy sum of a collection of positive numbers.
  defp sum_positives(anonymizer, values) do
    values = Enum.sort(values, &(&1 > &2))

    outlier_count = config(:dropped_outliers_count)
    {_outliers, values} = Enum.split(values, outlier_count) # drop outliers

    {top_average, anonymizer} = top_average(anonymizer, values)

    {noisy_outlier_count, anonymizer} = add_noise(anonymizer, outlier_count, config(:sum_noise_sigma))
    sum = noisy_outlier_count * top_average + Enum.sum(values)
    {maybe_round_result(sum, values), anonymizer}
  end

  # Round the final result of an aggregator depending on the type of aggregated values.
  defp maybe_round_result(result, []), do: round(result)
  defp maybe_round_result(result, [value | _rest]) when is_integer(value), do: round(result)
  defp maybe_round_result(result, [value | _rest]) when is_float(value), do: result
end
