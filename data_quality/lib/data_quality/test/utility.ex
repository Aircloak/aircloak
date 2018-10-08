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

  @spec process_across_dimensions(
          [Test.result()],
          %{atom => String.t()},
          [atom],
          ([Test.result()], %{atom => String.t()} -> any)
        ) :: any
  @doc """
  Slices the query results along a set of given dimensions, and passes each group
  to the callback function in turn. The list of return values are returned to the caller.
  """
  def process_across_dimensions(values, path, [], callback), do: [callback.(values, path)]

  def process_across_dimensions(values, path, [dimension | dimensions], callback) do
    values
    |> Enum.group_by(& &1[dimension])
    |> Enum.flat_map(fn {dimension_name, dimension_values} ->
      updated_path = Map.put(path, dimension, dimension_name)
      process_across_dimensions(dimension_values, updated_path, dimensions, callback)
    end)
  end
end
