defmodule Cloak.RichStreamTest do
  use ExUnit.Case, async: true

  alias Cloak.RichStream

  test "rich stream" do
    assert [:b, :c, :c, 3] ==
      [:a, :b, :c]
      |> RichStream.new(
            0,
            fn(count, el) -> {output(el), count + 1} end,
            fn(count) -> [count] end
          )
      |> Enum.to_list()
  end

  test "rich stream works with a lazy input" do
    assert [:b, :c, :c, 3] ==
      [:a, :b, :c]
      |> Stream.map(&(&1))
      |> RichStream.new(
            0,
            fn(count, el) -> {output(el), count + 1} end,
            fn(count) -> [count] end
          )
      |> Enum.to_list()
  end

  defp output(:a), do: []
  defp output(:b), do: [:b]
  defp output(:c), do: [:c, :c]
end
