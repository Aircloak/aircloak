defmodule Cloak.Stream do
  @moduledoc "Utilities for working with streams."

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
end
