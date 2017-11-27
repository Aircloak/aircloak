defmodule AircloakCI.Job.Queue do
  @moduledoc "Queueing helpers."

  require Logger

  @type id :: :compile | :compliance | {:build, String.t}


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

  @doc "Removes build queues which ar not needed anymore."
  @spec remove_needless_build_queues() :: :ok
  def remove_needless_build_queues(), do:
    :jobs.info(:queues)
    |> Enum.map(fn({:queue, queue}) -> queue |> Keyword.take([:name, :queued, :waiters]) |> Enum.into(%{}) end)
    |> Enum.filter(&(&1.waiters == [] && &1.queued == 0 && match?({:build, _}, &1.name)))
    |> Enum.map(&(&1.name))
    |> Enum.map(fn({:build, path}) -> path end)
    |> Enum.reject(&File.exists?/1)
    |> Enum.map(&{:build, &1})
    |> Enum.each(fn(queue_name) ->
      Logger.info("removing queue #{inspect queue_name}")
      :jobs.delete_queue(queue_name)
    end)


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
  defp spec({:build, _}), do:
    queue_spec(concurrency: 1, max_waiting_time: :timer.hours(1))

  defp queue_spec(opts), do:
    [
      max_time: Keyword.get(opts, :max_waiting_time, :undefined),
      regulators: [counter: [limit: Keyword.fetch!(opts, :concurrency)]]
    ]
end
