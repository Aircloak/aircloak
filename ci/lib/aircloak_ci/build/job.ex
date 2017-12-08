defmodule AircloakCI.Build.Job do
  @moduledoc "Helper functions for job execution."

  alias AircloakCI.{LocalProject, Queue}
  alias AircloakCI.Build

  @type run_queued_opts :: [log_name: String.t, report_result: pid]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the job using the provided lambda if mandatory conditions are met."
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
  @spec run_queued(Queue.id, LocalProject.t, run_queued_opts, (() -> result)) :: result when result: var
  def run_queued(queue, project, opts \\ [], fun), do:
    LocalProject.log_start_stop(project, "job #{queue} for #{LocalProject.name(project)}",
      fn ->
        log_name = Keyword.get(opts, :log_name, to_string(queue))
        start_watcher(self(), project, queue, opts)

        LocalProject.truncate_log(project, log_name)
        LocalProject.log(project, log_name, "entering queue `#{queue}`")
        Queue.exec(queue, fn ->
          LocalProject.log(project, log_name, "entered queue `#{queue}`")
          result = fun.()
          maybe_report_result(queue, opts, result)
          result
        end)
      end
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # Starts a watcher process as the child of the job. This process will detect job termination, and log interesting
  # events, such as running time, and possible crash reasons.
  defp start_watcher(owner_pid, project, queue, opts) do
    start = :erlang.monotonic_time(:second)
    Task.start_link(fn ->
      Process.flag(:trap_exit, true)
      log_name = Keyword.get(opts, :log_name, to_string(queue))
      receive do
        {:EXIT, ^owner_pid, reason} ->
          handle_exit(reason, project, log_name, queue, opts)

          diff_sec = :erlang.monotonic_time(:second) - start
          time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])
          LocalProject.log(project, log_name, "finished in #{time_output} min")
      end
    end)
  end

  defp handle_exit(:normal, _project, _log_name, _queue, _opts), do: :ok
  defp handle_exit(crash_reason, project, log_name, queue, opts) do
    maybe_report_result(queue, opts, :failure, crash_reason)
    LocalProject.log(project, log_name, "crashed: #{Exception.format_exit(crash_reason)}")
  end

  defp maybe_report_result(queue, opts, result, extra_info \\ nil) do
    case Keyword.fetch(opts, :report_result) do
      :error -> :ok
      {:ok, pid} -> Build.Server.report_result(pid, to_string(queue), result, extra_info)
    end
  end
end
