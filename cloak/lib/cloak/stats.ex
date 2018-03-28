defmodule Cloak.Stats do
  @moduledoc """
    Provides functions for efficient computation of statistics.

    The functions in this module expect the input values to be provided in a list.
    In order to take maximum advantage of compiling this module to native code,
    external calls are (and should be) avoided as much as possible.
  """

  @compile [:native, {:hipe, [:o3]}]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the mean(average) of the input list of values."
  @spec mean([number]) :: number
  def mean([]), do: nil

  def mean(values) do
    {count, sum, _sum_squares} = count_and_sums(values, 0, 0, 0)
    sum / count
  end

  @doc "Returns the standard deviation of the input list of values. Single pass through the values."
  @spec stddev([number]) :: number
  def stddev([]), do: nil
  def stddev([_value]), do: nil

  def stddev(values) do
    {count, sum, sum_squares} = count_and_sums(values, 0, 0, 0)
    variance = (count * sum_squares - sum * sum) / (count * (count - 1))
    :math.sqrt(variance)
  end

  @doc "Returns the sum of the input list of values. Should be 3-4 times faster than Enum.sum/1."
  @spec sum([number]) :: number
  def sum([]), do: nil
  def sum(values), do: sum(values, 0)

  @doc "Returns the mdian of the input list of values. Optimized verstion that minimizes GC pressure."
  @spec median([number]) :: number
  @bins_count 1000
  def median([]), do: nil

  def median(values) when length(values) < 2 * @bins_count,
    do:
      values
      |> Enum.sort()
      |> Enum.at(values |> length() |> div(2))

  def median(values) do
    # Uses binmedian algorithm from http://www.stat.cmu.edu/~ryantibs/papers/median.pdf
    # TL,DR:
    #     - median is guaranteed to be in interval [mean - sigma, mean + sigma]
    #     - we split that interval into bins and count the number of values in each bin
    #     - we find the bin holding the median and rank of the median value in that bin
    #     - original algorithm is recursive and re-splits the bin holding the median
    #     - here, instead, we select and order all the values in the median bin and then return the median
    {count, sum, sum_squares} = count_and_sums(values, 0, 0, 0)
    mean = sum / count
    variance = (count * sum_squares - sum * sum) / (count * (count - 1))
    sigma = :math.sqrt(variance)
    # initialize and fill the bins
    bins = :ets.new(:bins, [:private, :set, read_concurrency: false, write_concurrency: false])

    try do
      :ets.insert(bins, {:left_count, 0})
      for i <- 0..@bins_count, do: :ets.insert(bins, {i, 0})
      fill_bins(bins, values, mean, sigma)
      # identify the bin holding the median and the rank of the median in that bin
      left_count = :ets.lookup_element(bins, :left_count, 2)
      position = (length(values) |> div(2)) - left_count
      {median_bin, median_position_in_bin} = find_median_position(bins, 0, position)
      # retrieve the values in the bin containing the median, sort them and return the median
      bin_min = mean - sigma + median_bin * (2 * sigma) / @bins_count
      bin_max = bin_min + 2 * sigma / @bins_count

      values
      |> find_values_in_range(bin_min, bin_max, [])
      |> Enum.sort()
      |> Enum.at(median_position_in_bin)
    after
      :ets.delete(bins)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp count_and_sums([], count, sum, sum_squares), do: {count, sum, sum_squares}

  defp count_and_sums([value | rest], count, sum, sum_squares),
    do: count_and_sums(rest, count + 1, sum + value, sum_squares + value * value)

  defp sum([], sum), do: sum
  defp sum([value | rest], sum), do: sum(rest, sum + value)

  defp fill_bins(_bins, [], _mean, _sigma), do: :ok

  defp fill_bins(bins, [value | rest], mean, sigma) do
    cond do
      value < mean - sigma ->
        :ets.update_counter(bins, :left_count, {2, 1})

      value <= mean + sigma ->
        index = ((value - (mean - sigma)) * @bins_count / (2 * sigma)) |> trunc()
        :ets.update_counter(bins, index, {2, 1})

      true ->
        :ok
    end

    fill_bins(bins, rest, mean, sigma)
  end

  defp find_median_position(bins, index, position) when position >= 0 and index <= @bins_count do
    count = :ets.lookup_element(bins, index, 2)

    if position >= count or count == 0 do
      find_median_position(bins, index + 1, position - count)
    else
      {index, position}
    end
  end

  defp find_values_in_range([], _min, _max, acc), do: acc

  defp find_values_in_range([value | rest], min, max, acc) when value < min or value > max,
    do: find_values_in_range(rest, min, max, acc)

  defp find_values_in_range([value | rest], min, max, acc), do: find_values_in_range(rest, min, max, [value | acc])
end
