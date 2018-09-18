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
end
