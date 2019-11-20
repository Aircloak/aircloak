defmodule Cloak.Stats do
  @moduledoc """
    Provides functions for efficient computation of statistics.

    The functions in this module expect the input values to be provided in a list.
    In order to take maximum advantage of compiling this module to native code,
    external calls are (and should be) avoided as much as possible.
  """

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
    with variance when not is_nil(variance) <- variance(values), do: :math.sqrt(variance)
  end

  @doc "Returns the variance of the input list of values. Single pass through the values."
  @spec variance([number]) :: number
  def variance([]), do: nil
  def variance([_value]), do: nil

  def variance(values) do
    {count, sum, sum_squares} = count_and_sums(values, 0, 0, 0)
    max((count * sum_squares - sum * sum) / (count * (count - 1)), 0.0)
  end

  @doc "Returns the sum of the input list of values. Should be 3-4 times faster than Enum.sum/1."
  @spec sum([number]) :: number
  def sum([]), do: nil
  def sum(values), do: sum(values, 0)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp count_and_sums([], count, sum, sum_squares), do: {count, sum, sum_squares}

  defp count_and_sums([value | rest], count, sum, sum_squares),
    do: count_and_sums(rest, count + 1, sum + value, sum_squares + value * value)

  defp sum([], sum), do: sum
  defp sum([value | rest], sum), do: sum(rest, sum + value)
end
