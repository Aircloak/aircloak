defmodule AircloakCI.Queue do
  @moduledoc "Queueing helpers."

  require Logger

  @type id :: :compile | :standard_test | :compliance


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Waits in the given queue, executes the function when approved, and returns the function result.

  The queue will be created if needed.
  """
  @spec exec(id, (() -> result)) :: result when result: var
  def exec(id, fun) do
    {:ok, ref} = :jobs.ask(id)
    try do
      fun.()
    after
      :jobs.done(ref)
    end
  end

  @doc "Sets up the CI queues."
  @spec create_queues() :: :ok
  def create_queues(), do:
    Enum.each([:compile, :standard_test, :compliance], &add_queue/1)


  # -------------------------------------------------------------------
  # Queue specifications
  # -------------------------------------------------------------------

  defp add_queue(id), do:
    :jobs.add_queue(id, spec(id))

  defp spec(:compile), do:
    queue_spec(concurrency: 5, max_waiting_time: :timer.hours(1))
  defp spec(:standard_test), do:
    queue_spec(concurrency: 20, max_waiting_time: :timer.hours(1))
  defp spec(:compliance), do:
    queue_spec(concurrency: 1, max_waiting_time: :timer.hours(1))

  defp queue_spec(opts), do:
    [
      max_time: Keyword.get(opts, :max_waiting_time, :undefined),
      regulators: [counter: [limit: Keyword.fetch!(opts, :concurrency)]]
    ]
end
