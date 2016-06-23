defmodule Cloak.Processor.Noise do
  @moduledoc "Utility module for noisy processing of buckets."

  @opaque t :: %{
    rng: :rand.state
  }

  import Cloak.Type


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Creates a noise generator from a collection of unique user ids.

  This function takes either a `MapSet` containing user ids, or a map where
  keys are user ids. Such types ensure that user ids are deduplicated.

  It then creates a noise generator which can be used to create deterministic
  noise based on the input list.

  Internally, noise generator uses a `rand` based random number generator. This
  generator is relying on explicit passing of state, rather than on process
  dictionary, which forces client to explicitly pass the state around, and
  thus decide whether they want to produce noise sequences, or start from the
  beginning.
  """
  @spec new(MapSet.t | %{String.t => any}) :: t
  def new(%MapSet{} = users) do
    new_instance(users)
  end
  def new(%{} = users_map) do
    new_instance(Map.keys(users_map))
  end

  @doc """
  Returns a `{passes_filter?, noise_generator}` tuple, where `passes_filter?` is
  true if count is sufficiently large to be reported.

  Sufficiently large means:

  1. Greater than absolute_lower_bound
  2. A noised version of the count is greater than soft_lower_bound

  See config/config.exs for the parameters of the distribution used. The PRNG is seeded based
  on the user list provided, giving the same answer every time for the given list of users.
  """
  @spec passes_filter?(t, non_neg_integer) :: {boolean, t}
  def passes_filter?(noise_generator, count) do
    absolute_lower_bound = config(:absolute_lower_bound)
    soft_lower_bound = config(:soft_lower_bound)
    sigma_soft_lower_bound = config(:sigma_soft_lower_bound)
    {noisy_count, noise_generator} = get(noise_generator, sigma_soft_lower_bound, count)
    {
      count > absolute_lower_bound and round(noisy_count) > soft_lower_bound,
      noise_generator
    }
  end

  @doc """
  Generates a gaussian distributed random integer with given standard deviation and mean.

  Returns the integer together with the new generator state.
  """
  @spec get(t, integer | float, integer | float) :: {float, t}
  def get(%{rng: rng} = noise_generator, sigma, mu) do
    {rand1, rng} = :rand.uniform_s(rng)
    {rand2, rng} = :rand.uniform_s(rng)
    {gauss(sigma, mu, rand1, rand2), %{noise_generator | rng: rng}}
  end

  @doc """
  Computes the anonymized sum of a collection of values.

  The returned sum is an approximation of the real value. Refer to the
  implementation for precise details of the noise algorithm.
  """
  @spec sum(t, [number]) :: {float, t}
  def sum(noise_generator, values) do
    values = Enum.sort(values)

    outlier_count = config(:dropped_outliers_count)
    values = drop_outliers(values, outlier_count)

    margin_count_mean = config(:margin_count_mean)
    margin_count_sigma = config(:margin_count_sigma)
    {margin_count, noise_generator} = __MODULE__.get(noise_generator, margin_count_sigma, margin_count_mean)
    rounded_margin_count = round(margin_count)
    top_average = real_margin_average(values, rounded_margin_count)
    bottom_average = real_margin_average(values, -rounded_margin_count)

    sum_noise_sigma = config(:sum_noise_sigma)
    {noise, noise_generator} = __MODULE__.get(noise_generator, sum_noise_sigma, outlier_count)

    {
      noise * (top_average + bottom_average) + Enum.sum(values),
      noise_generator
    }
  end

  @doc """
  Sorts the values and computes the anonymized average of either beginning or
  the end of the sorted collection.

  The function first sorts the values, and then computes the anonymized average
  of either the beginning of the list (if `margin_sign` == 1), or its end
  (if `margin_sign` == -1). Refer to the implementation for precise details of
  the noise algorithm.
  """
  @spec margin_average(t, [number], 1 | -1) :: {float, t}
  def margin_average(noise_generator, values, margin_sign) do
    values = Enum.sort(values)

    outlier_count = config(:dropped_outliers_count)
    values = drop_outliers(values, outlier_count)

    margin_count_mean = config(:margin_count_mean)
    margin_count_sigma = config(:margin_count_sigma)
    {margin_count, noise_generator} = __MODULE__.get(noise_generator, margin_count_sigma, margin_count_mean)

    {real_margin_average(values, margin_sign * round(margin_count)), noise_generator}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp config(name), do: :cloak_conf.get_val(:noise, name)

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

  # Generates a gaussian distributed random number from two
  # uniform distributed numbers by the Box-Muller method.
  defp gauss(sigma, mu, rand1, rand2) when rand1 > 0 do
    r1 = -2.0 * :math.log(rand1)
    r2 = 2.0 * :math.pi() * rand2
    preval = :math.sqrt(r1) * :math.cos(r2)
    mu + sigma * preval
  end

  # Drops the specified numbers of outliers from a sorted collection of values.
  defp drop_outliers(values, outlier_count) do
    new_length = length(values) - 2 * outlier_count
    true = new_length > 0 # assert we have enough values to remove
    Enum.slice(values, outlier_count, new_length)
  end

  # Computes the average value for the margin of a collection.
  defp real_margin_average([], _margin_count), do: 0
  defp real_margin_average(values, margin_count) do
    margin = Enum.take(values, margin_count)
    Enum.sum(margin) / length(margin)
  end
end
