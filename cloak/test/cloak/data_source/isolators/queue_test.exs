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

  test "updating" do
    queue =
      Queue.new(columns(1..5))
      |> process(:col_4)
      |> Queue.set_high_priority(:col_2)
      |> Queue.set_high_priority(:col_5)
      |> Queue.update_known_columns(columns(3..6))

    assert pop_all(queue) == [:col_5, :col_3, :col_6]
  end

  defp columns(indices), do: Enum.map(indices, &:"col_#{&1}")

  defp pop_all(queue) do
    queue
    |> Stream.unfold(&with(:error <- Queue.next_column(&1), do: nil))
    |> Enum.to_list()
  end

  defp process(queue, column) do
    queue = Queue.set_high_priority(queue, column)
    {^column, queue} = Queue.next_column(queue)
    queue
  end
end
