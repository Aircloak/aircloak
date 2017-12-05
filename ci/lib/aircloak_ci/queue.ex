defmodule AircloakCI.Queue do
  @moduledoc "Queueing helpers."

  require Logger

  @type id :: :compile | :compliance


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Waits in the given queue, executes the function when approved, and returns the function result.

  The queue will be created if needed.
  """
  @spec exec(id, (() -> result)) :: result when result: var
  def exec(id, fun) do
    ensure_queue(id)
    {:ok, ref} = :jobs.ask(id)
    try do
      fun.()
    after
      :jobs.done(ref)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp ensure_queue(id) do
    case :jobs.queue_info(id) do
      :undefined -> :jobs.add_queue(id, spec(id))
      _ -> :ok
    end
  end


  # -------------------------------------------------------------------
  # Queue specifications
  # -------------------------------------------------------------------

  defp spec(:compile), do:
    queue_spec(concurrency: 5, max_waiting_time: :timer.hours(1))
  defp spec(:compliance), do:
    queue_spec(concurrency: 1, max_waiting_time: :timer.hours(1))
  defp spec({:project, _}), do:
    queue_spec(concurrency: 1, max_waiting_time: :timer.hours(1))

  defp queue_spec(opts), do:
    [
      max_time: Keyword.get(opts, :max_waiting_time, :undefined),
      regulators: [counter: [limit: Keyword.fetch!(opts, :concurrency)]]
    ]
end
