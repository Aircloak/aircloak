defmodule DataQuality.Test.Utility do
  @moduledoc "Miscelaneous utility functions needed by other test modules"

  alias DataQuality.Test

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
  Slices the query results along a set of given dimensions. Effectively an `Enum.group_by`
  allowing for passing a set of map keys that should be partitioned by.
  """
  def partition(values, partition_keys), do: Enum.group_by(values, &Map.take(&1, partition_keys))

  def maybe_to_number(value) when is_binary(value) do
    case Integer.parse(value) do
      :error ->
        value

      {int, ""} ->
        int

      _ ->
        case Float.parse(value) do
          :error -> value
          {float, ""} -> float
          _ -> value
        end
    end
  end

  def maybe_to_number(value), do: value
end
