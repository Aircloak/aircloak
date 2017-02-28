defmodule Cloak.Stream.Test do
  use ExUnit.Case, async: true

  describe "side_effect_after_first" do
    test "the side effect doesn't happen immediately" do
      this = self()
      Stream.cycle([nil])
      |> Cloak.Stream.side_effect_after_first(fn() -> send(this, :message) end)

      refute_receive :message
    end

    test "the side effect happens when the first element is evaluated" do
      this = self()
      stream =
        Stream.cycle([nil])
        |> Cloak.Stream.side_effect_after_first(fn() -> send(this, :message) end)
      Enum.take(stream, 1)

      assert_receive :message
    end

    test "doesn't change the content of the stream" do
      stream1 = Stream.cycle([1, 2, 3])
      stream2 = Cloak.Stream.side_effect_after_first(stream1, fn() -> nil end)

      assert Enum.take(stream1, 10) == Enum.take(stream2, 10)
    end
  end
end
