defmodule Cloak.AnalystTable.Jobs do
  @moduledoc """
  Manages the queue of pending jobs.

  This module distinguishes between create table jobs and serialized jobs. It allows multiple create table jobs to run
  concurrently, while the serialized job is exclusive - while it is runniing, no other job of any kind is allowed to
  run.
  """

  @max_concurrent_stores 5

  @opaque t :: %{queue: :queue.queue(job), running: [job]}
  @type job :: {:create_table, Cloak.AnalystTable.table()} | {:serialized, (() -> any)}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the jobs structure."
  @spec new() :: t
  def new(), do: %{queue: :queue.new(), running: []}

  @doc "Puts the job into the queue."
  @spec enqueue_job(t, job) :: t
  def enqueue_job(jobs, job) do
    if match?({:create_table, _table}, job) do
      false = Enum.member?(jobs.running, job)
      false = Enum.member?(:queue.to_list(jobs.queue), job)
    end

    update_in(jobs.queue, &:queue.in(job, &1))
  end

  @doc "Retrieves the list of next jobs to execute, and marks them as running."
  @spec next_jobs(t) :: {[job], t}
  def next_jobs(jobs) do
    with {job, jobs} <- pop_next_job(jobs),
         {:ok, jobs} <- mark_job_as_running(jobs, job),
         {remaining_jobs, jobs} = next_jobs(jobs),
         do: {[job | remaining_jobs], jobs},
         else: (_ -> {[], jobs})
  end

  @doc "Removes the provided running job from the data structure."
  @spec job_finished(t, job) :: t
  def job_finished(jobs, {:create_table, table}) do
    {[_], remaining} = Enum.split_with(jobs.running, &match?({:create_table, ^table}, &1))
    %{jobs | running: remaining}
  end

  def job_finished(%{running: [{:serialized, fun}]} = jobs, {:serialized, fun}), do: %{jobs | running: []}

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
    if can_run?(jobs, job) do
      {:ok, update_in(jobs.running, &[job | &1])}
    else
      :error
    end
  end

  defp can_run?(jobs, job) do
    cond do
      jobs.running == [] -> true
      serialized_job?(job) -> false
      running_serialized?(jobs) -> false
      length(jobs.running) == @max_concurrent_stores -> false
      true -> true
    end
  end

  @doc false
  def running_serialized?(jobs), do: Enum.any?(jobs.running, &serialized_job?(&1))

  defp serialized_job?(job), do: match?({:serialized, _}, job)
end
