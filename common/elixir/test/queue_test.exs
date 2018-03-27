defmodule Aircloak.QueueTest do
  use ExUnit.Case, async: true

  alias Aircloak.Queue

  test "size of an empty queue", do: assert(Queue.new().size == 0)

  test "pushing of a single item" do
    q = Queue.new() |> Queue.push(:foo)
    assert q.size == 1
    assert {{:value, :foo}, q} = Queue.pop(q)
    assert q.size == 0
  end

  test "first come first served" do
    q = Queue.new() |> Queue.push(1) |> Queue.push(2) |> Queue.push(3)
    assert {{:value, 1}, q} = Queue.pop(q)
    assert {{:value, 2}, q} = Queue.pop(q)
    assert {{:value, 3}, _} = Queue.pop(q)
  end

  test "popping from an empty queue",
    do: assert(Queue.new() |> Queue.pop() == {:empty, Queue.new()})

  test "peek", do: assert(Queue.new() |> Queue.push(:foo) |> Queue.peek() == {:value, :foo})

  test "peeking from an empty queue", do: assert(Queue.new() |> Queue.peek() == :empty)

  test "dropping an item" do
    q = Queue.new() |> Queue.push(1) |> Queue.push(2) |> Queue.push(3) |> Queue.drop()
    assert q.size == 2
    assert Queue.peek(q) == {:value, 2}
  end

  test "dropping from an empty queue", do: assert(Queue.new() |> Queue.drop() == Queue.new())

  test "drop_if satisfied" do
    q =
      Queue.new() |> Queue.push(1) |> Queue.push(2) |> Queue.push(3) |> Queue.drop_if(&(&1 == 1))

    assert q.size == 2
    assert Queue.peek(q) == {:value, 2}
  end

  test "drop_if not satisfied" do
    q =
      Queue.new() |> Queue.push(1) |> Queue.push(2) |> Queue.push(3) |> Queue.drop_if(&(&1 == 2))

    assert q.size == 3
    assert Queue.peek(q) == {:value, 1}
  end

  test "drop_if from an empty queue", do: assert(Queue.new() |> Queue.drop() == Queue.new())

  test "drop_while" do
    q =
      Queue.new()
      |> Queue.push(1)
      |> Queue.push(2)
      |> Queue.push(3)
      |> Queue.drop_while(&(&1.size > 1))

    assert q.size == 1
    assert Queue.peek(q) == {:value, 3}
  end

  test "drop_while stops on empty queue" do
    q =
      Queue.new()
      |> Queue.push(1)
      |> Queue.push(2)
      |> Queue.push(3)
      |> Queue.drop_while(&(&1.size > -1))

    assert q.size == 0
  end
end
