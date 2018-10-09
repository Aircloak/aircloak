defmodule DataQuality.Test.Utility do
  @moduledoc "Miscelaneous utility functions needed by other test modules"

  alias DataQuality.Test

  @timeout :timer.minutes(10)

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec name(Test.dimension() | Test.aggregate() | :atom | String.t()) :: String.t()
  @doc "Produces a human friendly name for dimensions and aggregates"
  def name({:dimension, name}), do: name
  def name({:bucket, param}), do: "bucket by #{param}"
  def name({:count, count}), do: count
  def name(name) when is_atom(name), do: to_string(name)
  def name(name) when is_binary(name), do: name
  def name(other), do: to_string(other)

  @spec partition(
          [Map.t()],
          [atom]
        ) :: %{Map.t() => [Map.t()]}
  @doc """
  Partition operates like `Enum.group_by` but instead of a key function it accepts a list of
  keys by which the list of maps should be partitioned.
  """
  def partition(values, partition_keys),
    do:
      values
      |> partition(partition_keys, %{})
      |> Enum.into(%{})

  @spec partition_and_process(
          [Test.result()],
          [atom],
          ([Test.result()], %{atom => String.t()} -> any)
        ) :: [any]
  @doc """
  Slices the query results along a set of given dimensions, and passes each group
  to the callback function in turn. The list of return values are returned to the caller.
  """
  def partition_and_process(values, partition_keys, callback) do
    values
    |> partition(partition_keys)
    |> Task.async_stream(
      fn {partition_parameters, values} -> callback.(values, partition_parameters) end,
      timeout: @timeout
    )
    |> Enum.map(fn {:ok, val} -> val end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp partition(values, [], partition_parameters), do: [{partition_parameters, values}]

  defp partition(values, [partition_key | partition_keys], partition_parameters) do
    values
    |> Enum.group_by(& &1[partition_key])
    |> Enum.flat_map(fn {partition_name, partition_values} ->
      partition(
        partition_values,
        partition_keys,
        Map.put(partition_parameters, partition_key, partition_name)
      )
    end)
  end
end
