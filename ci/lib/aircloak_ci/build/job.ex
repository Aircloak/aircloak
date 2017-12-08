defmodule AircloakCI.Build.Job do
  @moduledoc "Helper functions for job execution."

  alias AircloakCI.{Emoji, Github, LocalProject, Queue}
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

          case Keyword.fetch(opts, :report_result) do
            :error -> :ok
            {:ok, pid} -> Build.Server.report_result(pid, to_string(queue), result)
          end

          result
        end)
      end
    )

  @doc "Asynchronously sends a commit status to Github."
  @spec send_github_status(Github.API.repo, String.t, String.t, Github.API.statuses, Github.API.status, String.t) :: :ok
  def send_github_status(repo, sha, job_name, previous_statuses, status, description) do
    unless description == previous_statuses[job_name][:description], do:
      Github.put_status_check_state(repo.owner, repo.name, sha, full_github_context(job_name), description, status)
    :ok
  end

  @doc "Reports the result of the given job."
  @spec report_result(Build.Server.state, String.t, :ok | :error | :failure, any) :: Build.Server.state
  def report_result(build_state, job_name, result, extra_info \\ nil) do
    LocalProject.log(build_state.project, job_name, "result: `#{result}`")
    post_result_to_github(build_state, job_name, result, extra_info)
    LocalProject.mark_finished(build_state.project, job_name)
    build_state
  end


  # -------------------------------------------------------------------
  # Communication with Github
  # -------------------------------------------------------------------

  defp full_github_context(context), do:
    "continuous-integration/aircloak/#{context}"

  defp github_status(:ok), do: :success
  defp github_status(:error), do: :error
  defp github_status(:failure), do: :failure

  defp result_description(:ok), do: "succeeded"
  defp result_description(:error), do: "errored"
  defp result_description(:failure), do: "failed"

  defp post_result_to_github(build_state, job_name, result, extra_info) do
    send_github_status(
      build_state.source.repo,
      LocalProject.target_sha(build_state.project),
      job_name,
      Map.get(build_state.source, :status_checks, %{}),
      github_status(result),
      result_description(result)
    )

    post_job_comment(build_state, comment_body(build_state, job_name, result, extra_info))
  end

  defp post_job_comment(%{source_type: :pull_request, source: pr}, body), do:
    Github.comment_on_issue(pr.repo.owner, pr.repo.name, pr.number, body)
  defp post_job_comment(%{source_type: :branch, source: branch}, body), do:
    Github.comment_on_commit(branch.repo.owner, branch.repo.name, branch.sha, body)

  defp comment_body(_build_state, job_name, :ok, nil), do:
    "#{String.capitalize(job_name)} job succeeded #{Emoji.happy()}"
  defp comment_body(build_state, job_name, :error, nil), do:
    error_comment_body(build_state, job_name, "errored")
  defp comment_body(build_state, job_name, :failure, crash_reason), do:
    error_comment_body(build_state, job_name, "crashed", "```\n#{Exception.format_exit(crash_reason)}\n```")

  defp error_comment_body(build_state, job_name, crash_verb, extra_info \\ nil), do:
    Enum.join(
      [
        "#{String.capitalize(job_name)} job #{crash_verb} #{Emoji.sad()}",
        (if not is_nil(extra_info), do: "\n#{extra_info}\n", else: ""),
        "You can see the full build log by running: `ci/production.sh build_log #{build_state.source.number}`\n",
        "Log tail:\n", "```", log_tail(build_state.project, job_name), "```"
      ],
      "\n"
    )

  defp log_tail(project, job_name) do
    max_lines = 100
    lines = project |> LocalProject.log_contents(job_name) |> String.split("\n")

    lines
    |> Enum.drop(max(length(lines) - max_lines, 0))
    |> Enum.join("\n")
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
    case Keyword.fetch(opts, :report_result) do
      :error -> :ok
      {:ok, pid} -> Build.Server.report_result(pid, to_string(queue), :failure, crash_reason)
    end

    LocalProject.log(project, log_name, "crashed: #{Exception.format_exit(crash_reason)}")
  end
end
