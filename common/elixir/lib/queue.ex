defmodule Aircloak.Queue do
  @moduledoc """
  Implements a purely functional queue structure.

  This is a thin wrapper around `:queue` module with some additional features:

    - Normalized to Elixir's "subject as the first argument" convention.
    - Maintains queue size, so it can be obtained in constant time.
    - Supports additional operations, such as `drop_if/2`, and `drop_while/2`
  """

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @type t() :: %__MODULE__{items: :queue.queue(), size: non_neg_integer}
  defstruct items: :queue.new(), size: 0

  @doc "Creates a new queue instance."
  @spec new() :: t
  def new(), do: %__MODULE__{}

  @doc "Pushes the given item to the back of the queue."
  @spec push(t, any) :: t
  def push(queue, item),
    do: %__MODULE__{queue | items: :queue.in(item, queue.items), size: queue.size + 1}

  @doc "Pops the item from the front of the queue."
  @spec pop(t) :: {:empty, t} | {{:value, any}, t}
  def pop(queue) do
    case :queue.out(queue.items) do
      {:empty, _} ->
        {:empty, queue}

      {{:value, _} = value, items} ->
        {value, %__MODULE__{queue | items: items, size: queue.size - 1}}
    end
  end

  @doc "Returns the front item without removing it."
  @spec peek(t) :: :empty | {:value, any}
  def peek(queue), do: :queue.peek(queue.items)

  @doc "Drops the front item. Does nothing if the queue is empty."
  @spec drop(t) :: t
  def drop(%__MODULE__{size: 0} = queue), do: queue

  def drop(queue) do
    {_, queue} = pop(queue)
    queue
  end

  @doc "Conditionally drops the front item."
  @spec drop_if(t, (any -> boolean)) :: t
  def drop_if(queue, predicate) do
    case peek(queue) do
      :empty ->
        queue

      {:value, item} ->
        if predicate.(item),
          do: drop(queue),
          else: queue
    end
  end

  @doc "Drops front items until the predicate returns true."
  @spec drop_while(t, (t -> boolean)) :: t
  def drop_while(%__MODULE__{size: 0} = queue, _predicate), do: queue

  def drop_while(queue, predicate) do
    if predicate.(queue),
      do: queue |> drop() |> drop_while(predicate),
      else: queue
  end
end
