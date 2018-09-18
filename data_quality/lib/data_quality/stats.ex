defmodule DataQuality.Stats do
  @moduledoc "Various stats"

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec mean_squared_error([{integer, integer | nil}]) :: float
  @doc """
  Will return the mean squared error given a list of pairs of integer values.
  The expected input value is [{value1a, value1b}, {value2a, value2b}, ...]
  where the first value in the pair always must be present, whereas the second
  value in the pair might be `nil`. This is useful when calculating the
  mean squared error for query results that originate from the `cloak` where
  no value was returned for a given bucket.
  """
  def mean_squared_error(value_pairs) do
    squared_errors = squared_errors(value_pairs)

    case Enum.count(squared_errors) do
      0 ->
        0.0

      mse_rows_count ->
        Float.round(Enum.sum(squared_errors) / mse_rows_count, 2)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp squared_errors(value_pairs),
    do:
      value_pairs
      # For min/max/sum/avg we return null where we can't generate an aggregate.
      # There is no good replacement value we could use to generate an MSE, so
      # we remove these values.
      |> Enum.reject(fn {_, anon_value} -> is_nil(anon_value) end)
      |> Enum.map(fn {raw, anon} -> (raw - anon) * (raw - anon) end)
end
