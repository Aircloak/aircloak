defmodule Cloak.Processor.Noise do
  @moduledoc """
  Utility module for generating deterministic noise from a collection of unique users.

  This module can be used to produce noisy values, such as sums or averages.
  The values are approximations of the real values, with some random noise
  added. The amount of noise can be configured through the `:noise` section of OTP
  application environment.

  The generated noise is deterministic for the same set of users. For example,
  calling `Noise.new(users) |> Noise.sum(values)` will always give the same result
  for the same set of users and values, while it may differ for another set of
  users even if the values are the same.

  All functions return the anonymized value as well as the next state of the
  noise generator. This state is used to produce the next random number.
  Consequently, calling the same function with the same input but a different
  state may produce a different value.

  For example, let's say we have the following code:

  ```
  initial_noise_generator = Noise.new(users)
  {sum, new_noise_generator} = Noise.sum(initial_noise_generator, values)
  ```

  At this point, calling `Noise.sum(new_noise_generator, values)` might return a
  different sum. However, calling `Noise.sum(initial_noise_generator, values)`
  would always return the same value.
  """

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
  Returns a `{passes_filter?, noise_generator}` tuple, where `passes_filter?` is
  true if count is sufficiently large to be reported.

  Sufficiently large means:

  1. Greater than absolute_lower_bound
  2. A noised version of the count is greater than soft_lower_bound

  See config/config.exs for the parameters of the distribution used. The PRNG is seeded based
  on the user list provided, giving the same answer every time for the given list of users.
  """
  @spec passes_filter?(t, Enumerable.t) :: {boolean, t}
  def passes_filter?(noise_generator, values) do
    soft_lower_bound = config(:soft_lower_bound)
    sigma_soft_lower_bound = config(:sigma_soft_lower_bound)
    real_count = Enum.count(values)
    {noisy_count, noise_generator} = add_noise(noise_generator, real_count, sigma_soft_lower_bound)
    {
      real_count > count_lower_bound() and round(noisy_count) > soft_lower_bound,
      noise_generator
    }
  end

  @doc "Returns the size below which buckets are always considered too small to include."
  @spec count_lower_bound :: non_neg_integer
  def count_lower_bound, do: config(:absolute_lower_bound)

  @doc """
  Computes the anonymized sum of a collection of values.

  The returned sum is an approximation of the real value. Refer to the
  implementation for precise details of the noise algorithm.
  """
  @spec sum(t, Enumerable.t) :: {float, t}
  def sum(noise_generator, values) do
    values = Enum.sort(values)

    outlier_count = config(:dropped_outliers_count)
    values = drop_outliers(values, outlier_count)

    margin_count_mean = config(:margin_count_mean)
    margin_count_sigma = config(:margin_count_sigma)
    {noisy_margin_count, noise_generator} = add_noise(noise_generator, margin_count_mean, margin_count_sigma)
    rounded_noisy_margin_count = round(noisy_margin_count)
    top_average = real_margin_average(values, rounded_noisy_margin_count, :top)
    bottom_average = real_margin_average(values, rounded_noisy_margin_count, :bottom)

    sum_noise_sigma = config(:sum_noise_sigma)
    {noisy_outlier_count, noise_generator} = add_noise(noise_generator, outlier_count, sum_noise_sigma)

    {
      noisy_outlier_count * (top_average + bottom_average) + Enum.sum(values),
      noise_generator
    }
  end

  @doc """
  Sorts the values and computes the anonymized average of the top of the sorted
  collection. Refer to the implementation for precise details of the noise algorithm.
  """
  @spec top_margin_average(t, Enumerable.t) :: {float, t}
  def top_margin_average(noise_generator, values) do
    margin_average(noise_generator, values, :top)
  end

  @doc """
  Sorts the values and computes the anonymized average of the bottom of the sorted
  collection. Refer to the implementation for precise details of the noise algorithm.
  """
  @spec bottom_margin_average(t, Enumerable.t) :: {float, t}
  def bottom_margin_average(noise_generator, values) do
    margin_average(noise_generator, values, :bottom)
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

  if Mix.env != :test do
    # Produces a gaussian distributed random integer with given mean and standard deviation.
    defp add_noise(%{rng: rng} = noise_generator, mu, sigma) do
      {rand1, rng} = :rand.uniform_s(rng)
      {rand2, rng} = :rand.uniform_s(rng)
      {gauss(mu, sigma, rand1, rand2), %{noise_generator | rng: rng}}
    end

    # Generates a gaussian distributed random number from two
    # uniform distributed numbers by the Box-Muller method.
    defp gauss(mu, sigma, rand1, rand2) when rand1 > 0 do
      r1 = -2.0 * :math.log(rand1)
      r2 = 2.0 * :math.pi() * rand2
      preval = :math.sqrt(r1) * :math.cos(r2)
      mu + sigma * preval
    end
  else
    # No noise in unit tests
    defp add_noise(noise_generator, mu, _sigma), do: {mu, noise_generator}
  end

  defp drop_outliers(values, outlier_count) do
    new_length = length(values) - 2 * outlier_count
    true = new_length > 0 # assert we have enough values to remove
    Enum.slice(values, outlier_count, new_length)
  end

  # Computes the margin average of the top or bottom of the collection.
  defp margin_average(noise_generator, values, top_or_bottom) do
    values = Enum.sort(values)

    outlier_count = config(:dropped_outliers_count)
    values = drop_outliers(values, outlier_count)

    margin_count_mean = config(:margin_count_mean)
    margin_count_sigma = config(:margin_count_sigma)
    {noisy_margin_count, noise_generator} = add_noise(noise_generator, margin_count_mean, margin_count_sigma)

    {real_margin_average(values, round(noisy_margin_count), top_or_bottom), noise_generator}
  end

  # Computes the average value for the margin of a collection.
  defp real_margin_average([], _margin_count, _top_or_bottom), do: 0
  defp real_margin_average(values, margin_count, top_or_bottom) do
    count_to_take = case top_or_bottom do
      :top -> margin_count
      :bottom -> -margin_count
    end
    margin = Enum.take(values, count_to_take)
    Enum.sum(margin) / length(margin)
  end
end
