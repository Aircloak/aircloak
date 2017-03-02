defmodule Cloak.Stream do
  @moduledoc "Utilities for working with streams."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a transformed stream that invokes the given function when the first element is evaluated."
  @spec side_effect_after_first(Stream.t, (() -> any)) :: Stream.t
  def side_effect_after_first(stream, fun) do
    stream
    |> Stream.with_index()
    |> Stream.map(fn {item, index} ->
      if index == 0, do: fun.()
      item
    end)
  end

  @doc "Returns a transformed stream that invokes the given function after the last element is evaluated."
  @spec side_effect_after_last(Stream.t, (() -> any)) :: Stream.t
  def side_effect_after_last(stream, fun) do
    Stream.concat(stream, empty_side_effect_stream(fun))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp empty_side_effect_stream(fun) do
    Stream.resource(fun, fn(_) -> {:halt, nil} end, fn(_) -> :ok end)
  end
end
