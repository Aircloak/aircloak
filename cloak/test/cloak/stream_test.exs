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

  describe "side_effect_after_last" do
    test "the side effect doesn't if all elements aren't evaluted" do
      this = self()
      Stream.cycle([nil])
      |> Stream.take(10)
      |> Cloak.Stream.side_effect_after_last(fn() -> send(this, :message) end)
      |> Enum.take(9)

      refute_receive :message
    end

    test "the side effect happens when the whole stream is evaluated" do
      this = self()
      Stream.cycle([nil])
      |> Stream.take(10)
      |> Cloak.Stream.side_effect_after_last(fn() -> send(this, :message) end)
      |> Stream.run()

      assert_receive :message
    end

    test "doesn't change the content of the stream" do
      stream1 = Stream.cycle([1, 2, 3]) |> Stream.take(10)
      stream2 = Cloak.Stream.side_effect_after_last(stream1, fn() -> nil end)

      assert Enum.to_list(stream1) == Enum.to_list(stream2)
    end
  end
end
