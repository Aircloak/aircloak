defmodule Cloak.DataSource.Isolators.Queue do
  @moduledoc "Queue of the columns which must be processed by the isolator cache."

  @opaque t :: %{processed_columns: MapSet.t(), regular_queue: :queue.t(), priority_queue: :queue.t()}
  @type column :: {datasource_name :: String.t(), table_name :: String.t(), column_name :: String.t()}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates a new queue instance from the given collection of columns."
  @spec new(Enumerable.t()) :: t
  def new(known_columns) do
    %{
      processed_columns: MapSet.new(),
      regular_queue: :queue.new(),
      priority_queue: :queue.new()
    }
    |> update_known_columns(known_columns)
  end

  @doc """
  Retrieves the next column to be processed.

  The function first tries to pull from the high priority queue. If that queue is empty, the data is pulled from the
  regular queue. If both queues are empty, the function returns `:error`. See also `set_high_priority/2`.
  """
  @spec next_column(t) :: {column, t} | :error
  def next_column(queue) do
    with {column, queue} <- pop(queue), do: {column, update_in(queue.processed_columns, &MapSet.put(&1, column))}
  end

  @doc """
  Updates the collection of known columns.

  This function takes an enumerable of all known columns, and updates the queue structure:

    - Pending columns which are not known anymore are removed.
    - New columns which have not yet been processed are added to the queue.
  """
  @spec update_known_columns(t, Enumerable.t()) :: t
  def update_known_columns(queue, known_columns) do
    known_columns = MapSet.new(known_columns)

    new_unprocessed_columns =
      known_columns
      |> MapSet.difference(known_columns(queue))
      |> Enum.to_list()
      |> :queue.from_list()

    %{
      queue
      | processed_columns: MapSet.intersection(queue.processed_columns, known_columns),
        regular_queue:
          queue.regular_queue |> delete_unknown_columns(known_columns) |> :queue.join(new_unprocessed_columns),
        priority_queue: delete_unknown_columns(queue.priority_queue, known_columns)
    }
  end

  @doc """
  Moves the desired column to the high priority queue.

  This function can be used to affect the order of columns pulled with `next_column/1`.
  """
  @spec set_high_priority(t, column) :: t
  def set_high_priority(queue, column) do
    false = MapSet.member?(queue.processed_columns, column)

    if :queue.member(column, queue.priority_queue) do
      queue
    else
      %{
        queue
        | regular_queue: :queue.filter(&(&1 != column), queue.regular_queue),
          priority_queue: :queue.in(column, queue.priority_queue)
      }
    end
  end

  @doc "Moves all processed items to the end of the regular queue, leaving the priority queue intact."
  @spec reset(t) :: t
  def reset(queue) do
    %{
      queue
      | regular_queue: Enum.reduce(queue.processed_columns, queue.regular_queue, &:queue.in/2),
        processed_columns: MapSet.new()
    }
  end

  @doc "Returns true if the column has been processed."
  @spec processed?(t, column) :: boolean
  def processed?(queue, column), do: MapSet.member?(queue.processed_columns, column)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pop(queue) do
    case :queue.out(queue.priority_queue) do
      {{:value, column}, priority_queue} ->
        {column, %{queue | priority_queue: priority_queue}}

      {:empty, _} ->
        case :queue.out(queue.regular_queue) do
          {{:value, column}, regular_queue} -> {column, %{queue | regular_queue: regular_queue}}
          {:empty, _} -> :error
        end
    end
  end

  defp known_columns(queue) do
    [
      :queue.to_list(queue.regular_queue),
      :queue.to_list(queue.priority_queue),
      queue.processed_columns
    ]
    |> Enum.concat()
    |> MapSet.new()
  end

  defp delete_unknown_columns(queue, known_columns) do
    queue
    |> :queue.to_list()
    |> Stream.filter(&MapSet.member?(known_columns, &1))
    |> Enum.to_list()
    |> :queue.from_list()
  end
end
