defmodule Cloak.StreamTest do
  use ExUnit.Case, async: true

  test "rich stream" do
    assert [:b, :c, :c, 3] ==
      [:a, :b, :c]
      |> Cloak.Stream.transform(
            0,
            fn(el, count) -> {output(el), count + 1} end,
            fn(count) -> [count] end
          )
      |> Enum.to_list()
  end

  test "rich stream works with a lazy input" do
    assert [:b, :c, :c, 3] ==
      [:a, :b, :c]
      |> Stream.map(&(&1))
      |> Cloak.Stream.transform(
            0,
            fn(el, count) -> {output(el), count + 1} end,
            fn(count) -> [count] end
          )
      |> Enum.to_list()
  end

  defp output(:a), do: []
  defp output(:b), do: [:b]
  defp output(:c), do: [:c, :c]
end
