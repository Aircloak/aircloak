defmodule Cloak.AnalystTable.Jobs do
  @moduledoc "Manages the queue of pending create table jobs."

  @max_concurrent_stores 5

  @opaque t :: %{queue: :queue.queue(job), running: [job]}
  @type job :: {:create_table, Cloak.AnalystTable.t()}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the jobs structure."
  @spec new() :: t
  def new(), do: %{queue: :queue.new(), running: []}

  @doc "Puts the job into the queue."
  @spec enqueue_job(t, job) :: t
  def enqueue_job(jobs, {:create_table, _} = job) do
    false = Enum.member?(jobs.running, job)
    false = Enum.member?(:queue.to_list(jobs.queue), job)

    update_in(jobs.queue, &:queue.in(job, &1))
  end

  @doc "Retrieves the list of next jobs to execute, and marks them as running."
  @spec next_jobs(t) :: {[job], t}
  def next_jobs(jobs) do
    with {job, jobs} <- pop_next_job(jobs),
         {:ok, jobs} <- mark_job_as_running(jobs, job),
         {remaining_jobs, jobs} <- next_jobs(jobs) do
      {[job | remaining_jobs], jobs}
    else
      _ -> {[], jobs}
    end
  end

  @doc "Removes the provided running job from the data structure."
  @spec job_finished(t, job) :: t
  def job_finished(jobs, {:create_table, table}) do
    {[_], remaining} = Enum.split_with(jobs.running, &match?({:create_table, ^table}, &1))
    %{jobs | running: remaining}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pop_next_job(jobs) do
    case :queue.out(jobs.queue) do
      {:empty, _queue} -> nil
      {{:value, job}, queue} -> {job, %{jobs | queue: queue}}
    end
  end

  defp mark_job_as_running(jobs, job) do
    if length(jobs.running) < @max_concurrent_stores,
      do: {:ok, update_in(jobs.running, &[job | &1])},
      else: :error
  end
end
