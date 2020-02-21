defmodule Cloak.Query.Anonymizer do
  @moduledoc """
  Module for anonymized aggregation of data.

  This module can be used to produce aggregated values, such as counts, sums or averages,
  in a privacy-preserving form. The produced results are approximations of the real values,
  with constant noise added and removal of outliers. The anonymization parameters can
  be configured through the `:anonymizer` section of OTP application environment.

  The generated noise is deterministic for the same set of noise layers. For example,
  calling `Anonymizer.new([users]) |> Anonymizer.sum(values)` will always give the
  same result for the same set of users and values, while it may differ for another
  set of users even if the values are the same.

  All functions need the state of the noise generator. This state is used to produce
  the next noisy number. Consequently, calling the same function with the same input
  but a different state may produce a different value.

  For example, let's say we have the following code:

  ```
  initial_anonymizer = Anonymizer.new([users | other_noise_layers])
  sum = Anonymizer.sum(initial_anonymizer, values)
  ```

  At this point, calling `Anonymizer.sum(new_anonymizer, values)` might return a
  different sum. However, calling `Anonymizer.sum(initial_anonymizer, values)`
  would always return the same value.

  For a description of the way the aggregation is performed, see `docs/anonymization.md`.
  """

  alias Cloak.Sql.NoiseLayer

  @opaque rng_state :: [:rand.state()]

  @type t :: %{
          rngs: rng_state,
          noise_layers: NoiseLayer.accumulator()
        }

  import Kernel, except: [max: 2]

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Creates a noise generator from a collection of sets of values representing noise layers.

  Each noise layer must be either a `MapSet`, or a map (in which case the keys are used).
  Such types ensure that the values in the noise layers are unique without the need to check
  that again if the calling code already keeps the values in such a structure.
  """
  @spec new(NoiseLayer.accumulator()) :: t
  def new([_ | _] = noise_layers), do: %{rngs: build_rngs(noise_layers), noise_layers: noise_layers}

  @doc """
  Returns true if the passed bucket size is sufficiently large to be reported.

  Sufficiently large means the bucket size is greater or equal to the:

  1. low_count_absolute_lower_bound
  2. noisy soft_lower_bound

  See config/config.exs for the parameters of the distribution used. The PRNG is seeded based
  on the user list provided, giving the same answer every time for the given list of users.
  """
  @spec sufficiently_large?(t, non_neg_integer, atom) :: boolean
  def sufficiently_large?(anonymizer, count, soft_lower_bound \\ :low_count_soft_lower_bound) do
    absolute_lower_bound = config(:low_count_absolute_lower_bound)

    if count < absolute_lower_bound do
      false
    else
      {noisy_lower_bound, _anonymizer} = add_noise(anonymizer, config(soft_lower_bound))

      count >= round(noisy_lower_bound)
    end
  end

  @doc """
  Computes the noisy count and noise sigma of all values in rows, where each row is an enumerable.

  When the number of users is too low to produce a count:
  - the count uses the lower threshold for reportable values (to remain compatible with standard SQL)
  - the count_noise is set to nil
  """
  @spec count(t, Enumerable.t()) :: {non_neg_integer, non_neg_integer | nil}
  def count(anonymizer, rows) do
    case sum_positives(anonymizer, rows) do
      {{0, nil}, _anonymizer} ->
        {config(:low_count_absolute_lower_bound), nil}

      {{count, noise_sigma}, _anonymizer} ->
        count = count |> round() |> Kernel.max(config(:low_count_absolute_lower_bound))
        {_noise_lower_bound, _noise_upper_bound, noise_sigma_lower_bound} = config(:outliers_count)
        {count, Kernel.max(noise_sigma, noise_sigma_lower_bound)}
    end
  end

  @doc """
  Computes the noisy sum and noise sigma of all values in rows, where each row is an enumerable of numbers.

  Returns nil when the number of users is too low to produce an anonymized sum.
  """
  @spec sum(t, Enumerable.t()) :: {number, number} | {nil, nil}
  def sum(anonymizer, rows) do
    {positive_result, anonymizer} = sum_positives(anonymizer, rows)
    {negative_result, _anonymizer} = sum_positives(anonymizer, Stream.map(rows, &-/1))

    case {positive_result, negative_result} do
      {{0, nil}, {0, nil}} ->
        {nil, nil}

      {{positives_sum, positives_noise_sigma}, {negatives_sum, negatives_noise_sigma}} ->
        noise_sigma = sum_noise_sigmas(positives_noise_sigma, negatives_noise_sigma)
        sum = positives_sum - negatives_sum
        {sum, noise_sigma}
    end
  end

  @doc "Computes the noisy minimum value from the given enumerable of numbers."
  @spec min(t, Enumerable.t()) :: number | nil
  def min(anonymizer, rows) do
    {max_count, min_count, top_count, anonymizer} = min_max_groups(anonymizer)
    top_length = max_count + top_count + min_count
    # we use the fact that min([value]) = -max([-value])
    case get_top(rows, fn {:min, value} -> -value end, top_length) do
      # there weren't enough values in the input to anonymize the result
      nil ->
        nil

      top_values ->
        case top_values |> Enum.drop(max_count) |> Enum.take(top_count) |> noisy_average(anonymizer) do
          nil -> nil
          value -> -value
        end
    end
  end

  @doc "Computes the noisy maximum value from the given enumerable of numbers."
  @spec max(t, Enumerable.t()) :: number | nil
  def max(anonymizer, rows) do
    {max_count, min_count, top_count, anonymizer} = min_max_groups(anonymizer)
    top_length = max_count + top_count + min_count

    case get_top(rows, fn {:max, value} -> value end, top_length) do
      # there weren't enough values in the input to anonymize the result
      nil ->
        nil

      top_values ->
        top_values |> Enum.drop(min_count) |> Enum.take(top_count) |> noisy_average(anonymizer)
    end
  end

  @doc """
  Computes the noisy average value and noise sigma of all values in rows,
  where each row is an enumerable of numbers.

  Returns nil when the number of users is too low to produce an anonymized average.
  """
  @spec avg(t, Enumerable.t()) :: {float, float} | {nil, nil}
  def avg(anonymizer, rows) do
    case sum(anonymizer, Stream.map(rows, fn {:avg, sum, _count} -> sum end)) do
      {nil, nil} ->
        {nil, nil}

      {sum, sum_noise_sigma} ->
        {count, _count_noise_sigma} = count(anonymizer, Stream.map(rows, fn {:avg, _sum, count} -> count end))

        {sum / count, sum_noise_sigma / count}
    end
  end

  @doc """
  Computes the noisy variance and noise sigma of all values in rows,
  where each row is an enumerable of numbers.
  """
  @spec variance(t, Enumerable.t()) :: {float, float} | {nil, nil}
  def variance(anonymizer, rows) do
    {sum, count} =
      Enum.reduce(rows, {0, 0}, fn {:variance, sum, _sum_sqrs, count}, {acc_sum, acc_count} ->
        {acc_sum + sum, acc_count + count}
      end)

    mean = sum / Kernel.max(count, 1)

    variances =
      Stream.map(rows, fn {:variance, sum, sum_sqrs, count} ->
        {:avg, sum_sqrs + mean * (count * mean - 2 * sum), count}
      end)

    avg(anonymizer, variances)
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
    sigma = config(:sum_noise_sigma) * noise_sigma_scale_factor(1, 1)
    {noisy_count, _anonymizer} = add_noise(anonymizer, {count, sigma})
    Kernel.max(round(noisy_count), config(:low_count_absolute_lower_bound))
  end

  @doc "Returns the configuration value for an anonymizer parameter."
  @spec config(atom) :: term
  def config(name), do: Application.get_env(:cloak, :anonymizer) |> Keyword.fetch!(name)

  @factor 4
  @avg_scale 1
  @top_scale 0.5

  @doc "Computes the noisy sum, min and max aggregates from the no-uid statistics for a bucket."
  @spec noisy_statistics(t, {non_neg_integer, number, number, number, float, float}) ::
          {float | nil, float | nil, float | nil, float | nil}
  def noisy_statistics(anonymizer, {count, sum, min, max, avg, stddev} = _statistics) do
    {std_above, std_below} =
      if min == max do
        # In this case we want edge_{above,below} to become the avg
        {0, 0}
      else
        range = max - min
        std_above = stddev * (max - avg) / range
        std_below = stddev * (avg - min) / range
        {std_above, std_below}
      end

    edge_above = avg + @factor * std_above
    edge_below = avg - @factor * std_below

    flatten = max - edge_above + min - edge_below

    adj_avg = avg - if flatten > 0, do: flatten / count, else: 0

    sum_sigma = Enum.max([abs(@avg_scale * adj_avg), abs(@top_scale * edge_above), abs(@top_scale * edge_below)])

    {noise, anonymizer} = add_noise(anonymizer, {0, config(:sum_noise_sigma)})

    noisy_sum = sum + noise * sum_sigma - flatten
    noisy_min = edge_below + noise * edge_sigma(edge_below, avg, count) * @top_scale
    noisy_max = edge_above + noise * edge_sigma(edge_above, avg, count) * @top_scale
    noise_amount = noise_amount(sum_sigma, anonymizer)

    cond do
      not sufficiently_large?(anonymizer, count) -> {nil, nil, nil, nil}
      min >= 0 -> {Kernel.max(noisy_sum, 0.0), Kernel.max(noisy_min, 0.0), Kernel.max(noisy_max, 0.0), noise_amount}
      max <= 0 -> {Kernel.min(noisy_sum, 0.0), Kernel.min(noisy_min, 0.0), Kernel.min(noisy_max, 0.0), noise_amount}
      true -> {noisy_sum, noisy_min, noisy_max, noise_amount}
    end
  end

  @doc "Returns the actual noise amount from a dynamic anonymizer and a static noise SD."
  @spec noise_amount(number, t) :: float
  def noise_amount(sigma, anonymizer), do: sigma |> scale_sigma_by_noise_layers(anonymizer) |> round_noise_sigma()

  @doc "Computes the noisy count of distinct values from the no-uid statistics for a bucket."
  @spec noisy_distinct_count(t, {non_neg_integer | nil, non_neg_integer | nil}) :: {float | nil, float | nil}
  def noisy_distinct_count(_anonymizer, {count, nil} = _statistics), do: {count, 0}

  def noisy_distinct_count(anonymizer, {count, noise_factor} = _statistics) do
    {noise, _anonymizer} = add_noise(anonymizer, {0, config(:sum_noise_sigma)})
    count_sigma = noise_factor * 0.5
    noise_amount = noise_amount(count_sigma, anonymizer)
    noisy_count = count + noise * count_sigma
    {noisy_count |> round() |> Kernel.max(0), noise_amount}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp build_rng(hash) do
    <<a::16-unsigned, b::16-unsigned, c::32-unsigned>> = <<hash::64-unsigned>>
    :rand.seed(:exsplus, {a, b, c})
  end

  defp build_rngs(noise_layers),
    do:
      noise_layers
      |> Enum.map(&NoiseLayer.sum_hashes(&1, config(:salt)))
      |> Enum.uniq()
      |> Enum.map(&build_rng/1)

  # Produces random number with given mean. The number is a sum of the mean and a gaussian-distributed 0-mean number
  # with the given standard deviation _per noise layer_.
  defp add_noise(%{rngs: rngs} = anonymizer, {mean, sd_scale}) do
    {noise, rngs} =
      Enum.reduce(rngs, {0, []}, fn rng, {noise, rngs} ->
        {sample, rng} = Cloak.RNG.gauss(rng)
        {noise + sample, [rng | rngs]}
      end)

    {mean + noise * sd_scale, %{anonymizer | rngs: Enum.reverse(rngs)}}
  end

  # Computes the noisy sum of a collection of positive numbers.
  defp sum_positives(anonymizer, rows) do
    noise_sigma = config(:sum_noise_sigma)
    {noise, anonymizer} = add_noise(anonymizer, {0, noise_sigma})
    {outliers_count, anonymizer} = get_group_count(anonymizer, config(:outliers_count))
    {top_count, anonymizer} = get_group_count(anonymizer, config(:top_count))

    case sum_positives(rows, outliers_count, top_count) do
      {0, nil} ->
        {{0, nil}, anonymizer}

      {sum, noise_sigma_scale} ->
        noise_sigma = noise_sigma * noise_sigma_scale
        noisy_sum = Kernel.max(sum + noise * noise_sigma_scale, 0.0)
        {{noisy_sum, noise_amount(noise_sigma, anonymizer)}, anonymizer}
    end
  end

  # The standard deviation used grows with the square root of the number of noise layers.
  defp scale_sigma_by_noise_layers(sigma, %{rngs: rngs} = _anonymizer), do: :math.sqrt(length(rngs)) * sigma

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
    {sum, count, top_length, top_values} =
      Enum.reduce(rows, {0, 0, 0, []}, fn
        value, {sum, count, top_length, top} when value < 0 ->
          {sum, count, top_length, top}

        value, {sum, count, top_length, top} when top_length <= outliers_count + top_count ->
          # This is the case in which the `top_values` list is not full yet and we need to add the current value to it.
          {sum, count, top_length + 1, insert_sorted(top, value)}

        value, {sum, count, top_length, [top_smallest | top_rest] = top} ->
          # This is the case in which our `top_values` list is full and we need to compare the
          # current `value` with the head of the list.
          if value > top_smallest do
            {sum + top_smallest, count + 1, top_length, insert_sorted(top_rest, value)}
          else
            {sum + value, count + 1, top_length, top}
          end
      end)

    if top_length > outliers_count do
      top_length = top_length - outliers_count
      top_values_sum = top_values |> Enum.take(top_length) |> Enum.sum()
      top_average = top_values_sum / top_length
      sum = sum + top_values_sum
      count = count + top_length
      average = sum / count
      {sum + outliers_count * top_average, noise_sigma_scale_factor(average, top_average)}
    else
      # We don't have enough values to return a result.
      {0, nil}
    end
  rescue
    _ -> {0, nil}
  end

  defp noise_sigma_scale_factor(average, top_average) do
    {average_factor, top_average_factor} = config(:sum_noise_sigma_scale_params)
    Kernel.max(average_factor * average, top_average_factor * top_average)
  end

  defp min_max_groups(anonymizer) do
    {largest_outliers_count, anonymizer} = get_group_count(anonymizer, config(:outliers_count))
    {smallest_outliers_count, anonymizer} = get_group_count(anonymizer, config(:outliers_count))
    {top_count, anonymizer} = get_group_count(anonymizer, config(:top_count))
    {largest_outliers_count, smallest_outliers_count, top_count, anonymizer}
  end

  # Given a list of rows and a row accumulator functor, this function will return
  # the top values, if enough rows are available.
  defp get_top(rows, row_accumulator, min_length) do
    {top_length, top_values} =
      Enum.reduce(rows, {0, []}, fn
        row, {top_length, top} when top_length <= min_length ->
          row_value = row_accumulator.(row)
          {top_length + 1, insert_sorted(top, row_value)}

        row, {top_length, [top_smallest | top_rest] = top} ->
          row_value = row_accumulator.(row)

          if row_value > top_smallest do
            {top_length, insert_sorted(top_rest, row_value)}
          else
            {top_length, top}
          end
      end)

    if top_length < min_length, do: nil, else: top_values
  end

  # Returns the average of a set of values + noise with SD of the quarter of the SD of the input values
  defp noisy_average(values, anonymizer) do
    value_count = values |> Enum.count() |> Kernel.max(1)
    average = Enum.sum(values) / value_count

    variance = (values |> Enum.map(&((&1 - average) * (&1 - average))) |> Enum.sum()) / value_count

    quarter_stddev = :math.sqrt(variance) / 4
    {noisy_average, _anonymizer} = add_noise(anonymizer, {average, quarter_stddev})
    noisy_average
  rescue
    _ -> nil
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

  defp sum_noise_sigmas(sigma1, nil), do: sigma1
  defp sum_noise_sigmas(nil, sigma2), do: sigma2
  defp sum_noise_sigmas(sigma1, sigma2), do: :math.sqrt(sigma1 * sigma1 + sigma2 * sigma2)

  defp get_group_count(anonymizer, range_sigma) do
    {min_count, max_count, sigma} = range_sigma
    mean_count = (min_count + max_count) * 0.5
    {count, anonymizer} = add_noise(anonymizer, {mean_count, sigma})
    count = count |> round() |> Kernel.max(min_count) |> Kernel.min(max_count)
    {count, anonymizer}
  end

  defp edge_sigma(edge, avg, count) do
    new_avg = (avg * count - edge) / Kernel.max(count - 1, 1)
    abs(avg - new_avg)
  end
end
