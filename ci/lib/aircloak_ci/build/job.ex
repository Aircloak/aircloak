defmodule AircloakCI.Build.Job do
  @moduledoc "Helper functions for job execution."

  alias AircloakCI.{LocalProject, Queue}
  alias AircloakCI.Build

  @type run_queued_opts :: [job_name: String.t, log_name: String.t, report_result: pid]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Starts the job using the provided lambda if mandatory conditions are met.

  Notes:

  - if the job is already running it is not started
  - if the job has finished, it is started only if the build is forced

  It's worth noting that the finished status is tied to a particular commit. Therefore, if new commits are pushed,
  the finished status is reset.

  This function can therefore be safely called repeatedly for the same job, and it will still lead to the single
  execution of that job. Once the job is finished, it won't be restarted again as long as nothing is pushed to the
  corresponding branch.
  """
  @spec maybe_start(Build.Server.state, Build.Server.job_name, ((Build.Server.state) -> Build.Server.state)) ::
    Build.Server.state
  def maybe_start(build_state, job_name, fun) do
    if not Build.Server.running?(build_state, job_name) and
      (
        LocalProject.forced?(build_state.project, job_name) or
        not LocalProject.finished?(build_state.project, job_name)
      )
    do
      fun.(build_state)
    else
      build_state
    end
  end

  @doc """
  Executes a job in the given queue.

  This function will queue the given job in the desired queue, execute it, and log various events, such as start,
  finish, execution time, and crashes.
  """
  @spec run_queued(Queue.id, LocalProject.t, (() -> :ok | {:error, String.t}), run_queued_opts) :: :ok | :error
  def run_queued(queue, project, fun, opts \\ []) do
    start_watcher(self(), project, queue, opts)

    LocalProject.truncate_log(project, log_name(queue, opts))
    LocalProject.log(project, log_name(queue, opts), "entering queue `#{queue}`")
    Queue.exec(
      queue,
      fn ->
        LocalProject.log(project, log_name(queue, opts), "entered queue `#{queue}`")
        result =
          if Enum.member?(Application.get_env(:aircloak_ci, :simulated_jobs, []), queue) do
            IO.puts("simulating job #{log_name(queue, opts)}")
            :timer.sleep(:timer.seconds(1))
            :ok
          else
            fun.()
          end

        result =
          with {:error, reason} <- result do
            LocalProject.log(project, log_name(queue, opts), "error: #{reason}")
            :error
          end

        maybe_report_result(queue, opts, result)
        result
      end
    )
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # Starts a watcher process as the child of the job. This process will detect job termination, and log interesting
  # events, such as running time, and possible crash reasons.
  defp start_watcher(owner_pid, project, queue, opts) do
    start = :erlang.monotonic_time(:second)
    Task.start_link(fn ->
      Process.flag(:trap_exit, true)
      receive do
        {:EXIT, ^owner_pid, reason} ->
          handle_exit(reason, project, queue, opts)

          diff_sec = :erlang.monotonic_time(:second) - start
          time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])
          LocalProject.log(project, log_name(queue, opts), "finished in #{time_output} min")
      end
    end)
  end

  defp handle_exit(normal, _project, _queue, _opts) when normal in [:normal, :shutdown], do: :ok
  defp handle_exit(crash_reason, project, queue, opts) do
    maybe_report_result(queue, opts, :failure, crash_reason)
    LocalProject.log(project, log_name(queue, opts), "crashed: #{Exception.format_exit(crash_reason)}")
  end

  defp maybe_report_result(queue, opts, result, extra_info \\ nil) do
    case Keyword.fetch(opts, :report_result) do
      :error -> :ok
      {:ok, pid} -> Build.Server.report_result(pid, job_name(queue, opts), result, extra_info)
    end
  end

  defp job_name(queue, opts), do:
    Keyword.get(opts, :job_name, to_string(queue))

  defp log_name(queue, opts), do:
    Keyword.get(opts, :log_name, to_string(queue))
end
