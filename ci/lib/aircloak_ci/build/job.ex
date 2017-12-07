defmodule AircloakCI.Build.Job do
  @moduledoc "Helper functions for job execution."

  alias AircloakCI.{LocalProject, Queue}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Executes a job in the given queue.

  This function will queue the given job in the desired queue, execute it, and log various events, such as start,
  finish, execution time, and crashes.
  """
  @spec run_queued(Queue.id, LocalProject.t, [log_name: String.t], (() -> result)) :: result when result: var
  def run_queued(queue, project, opts \\ [], fun), do:
    LocalProject.log_start_stop(project, "job #{queue} for #{LocalProject.name(project)}",
      fn ->
        log_name = Keyword.get(opts, :log_name, to_string(queue))
        start_watcher(self(), project, log_name)

        LocalProject.truncate_log(project, log_name)
        LocalProject.log(project, log_name, "entering queue `#{queue}`")
        Queue.exec(queue, fn ->
          LocalProject.log(project, log_name, "entered queue `#{queue}`")
          fun.()
        end)
      end
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # Starts a watcher process as the child of the job. This process will detect job termination, and log interesting
  # events, such as running time, and possible crash reasons.
  defp start_watcher(owner_pid, project, log_name) do
    start = :erlang.monotonic_time(:second)
    Task.start_link(fn ->
      Process.flag(:trap_exit, true)
      receive do
        {:EXIT, ^owner_pid, reason} ->
          diff_sec = :erlang.monotonic_time(:second) - start
          time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

          log_exit_reason(project, log_name, reason)

          LocalProject.log(project, log_name, "finished in #{time_output} min")
      end
    end)
  end

  defp log_exit_reason(_project, _log_name, :normal), do: :ok
  defp log_exit_reason(project, log_name, reason), do:
    LocalProject.log(project, log_name, "crashed: #{Exception.format_exit(reason)}")
end
