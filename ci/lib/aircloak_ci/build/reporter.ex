defmodule AircloakCI.Build.Reporter do
  @moduledoc "Helper functions for reporting job events."

  alias AircloakCI.{Emoji, Github, LocalProject}
  alias AircloakCI.Build

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Asynchronously reports the status of the given job."
  @spec report_status(Github.API.repo, String.t, String.t, Github.API.status, Github.API.status, String.t) :: :ok
  def report_status(repo, sha, job_name, previous_statuses, status, description) do
    unless description == previous_statuses[job_name][:description], do:
      Github.put_status_check_state(repo.owner, repo.name, sha, full_github_context(job_name), description, status)
    :ok
  end

  @doc "Asynchronously reports the result of the given job."
  @spec report_result(Build.Server.state, String.t, :ok | :error | :failure, any) :: :ok
  def report_result(build_state, job_name, result, extra_info \\ nil) do
    LocalProject.log(build_state.project, job_name, "result: `#{result}`")
    post_result_to_github(build_state, job_name, result, extra_info)
  end


  # -------------------------------------------------------------------
  # Internal functions
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
    report_status(
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
    "#{job_name} job succeeded #{Emoji.happy()}"
  defp comment_body(build_state, job_name, :error, nil), do:
    error_comment_body(build_state, job_name, "errored")
  defp comment_body(build_state, job_name, :failure, crash_reason), do:
    error_comment_body(build_state, job_name, "crashed", "```\n#{Exception.format_exit(crash_reason)}\n```")

  defp error_comment_body(build_state, job_name, crash_verb, extra_info \\ nil), do:
    Enum.join(
      [
        "#{job_name} job #{crash_verb} #{Emoji.sad()}",
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
end
