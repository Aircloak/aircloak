defmodule Cloak.DataSource.Isolators.Queue.Test do
  use ExUnit.Case, async: true
  alias Cloak.DataSource.Isolators.Queue

  test "fetching from an empty queue" do
    assert Queue.next_column(Queue.new([])) == :error
  end

  test "first in, first out" do
    queue = Queue.new(columns(1..3))
    assert pop_all(queue) == [:col_1, :col_2, :col_3]
  end

  test "prioritizing" do
    queue = Queue.set_high_priority(Queue.new(columns(1..5)), :col_3)
    assert pop_all(queue) == [:col_3, :col_1, :col_2, :col_4, :col_5]
  end

  test "prioritizing is idempotent" do
    queue = Enum.reduce([:col_2, :col_3, :col_4, :col_3], Queue.new(columns(1..5)), &Queue.set_high_priority(&2, &1))
    assert pop_all(queue) == [:col_2, :col_3, :col_4, :col_1, :col_5]
  end

  test "update adds new columns to the regular queue" do
    queue = Queue.new(columns(1..3)) |> Queue.set_high_priority(:col_2) |> Queue.update_known_columns(columns(1..4))
    assert pop_all(queue) == [:col_2, :col_1, :col_3, :col_4]
  end

  test "update doesn't affect processed columns" do
    queue = Queue.new(columns(1..3)) |> Queue.set_high_priority(:col_2)
    {_, queue} = Queue.next_column(queue)
    {_, queue} = Queue.next_column(queue)
    queue = Queue.update_known_columns(queue, columns(1..3))
    assert pop_all(queue) == [:col_3]
  end

  test "update removes obsolete columns" do
    queue = Queue.new(columns(1..4)) |> Queue.set_high_priority(:col_2) |> Queue.update_known_columns(columns(3..4))
    assert pop_all(queue) == [:col_3, :col_4]
  end

  defp columns(indices), do: Enum.map(indices, &:"col_#{&1}")

  defp pop_all(queue) do
    queue
    |> Stream.unfold(&with(:error <- Queue.next_column(&1), do: nil))
    |> Enum.to_list()
  end
end
