defmodule AircloakCI.Build.Reporter do
  @moduledoc "Helper functions for reporting job events."

  alias AircloakCI.Build
  alias AircloakCI.{Emoji, Github, LocalProject}

  @type report_info :: %__MODULE__{
          project: LocalProject.t(),
          source_type: Build.Server.source_type(),
          source: Build.Server.source(),
          job_name: String.t(),
          log_name: String.t(),
          result: :ok | :error | :failure,
          extra_info: nil | String.t(),
          build_log_command: nil | String.t(),
          restart_command: nil | String.t(),
          remote_console_command: nil | String.t(),
          prologue: nil | String.t()
        }

  @enforce_keys [:source_type, :source, :job_name, :log_name, :result]
  defstruct [
    :project,
    :source_type,
    :source,
    :job_name,
    :log_name,
    :result,
    extra_info: nil,
    build_log_command: nil,
    restart_command: nil,
    remote_console_command: nil,
    prologue: nil
  ]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Asynchronously reports the result of the given job."
  @spec report_result(report_info) :: :ok
  def report_result(report_info) do
    LocalProject.log(report_info.project, report_info.log_name, "result: `#{report_info.result}`")
    post_result_to_github(report_info)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp post_result_to_github(report_info) do
    # Only report error/failure to reduce the noise in PR builds.
    if report_info.result in [:error, :failure], do: post_job_comment(report_info, comment_body(report_info))
    :ok
  end

  defp post_job_comment(%__MODULE__{source_type: :pull_request, source: pr}, body),
    do: Github.comment_on_issue(pr.repo.owner, pr.repo.name, pr.number, body)

  defp post_job_comment(%__MODULE__{source_type: :branch, source: branch}, body),
    do: Github.comment_on_commit(branch.repo.owner, branch.repo.name, branch.sha, body)

  defp post_job_comment(%__MODULE__{source_type: :local, source: local}, body),
    do: Github.comment_on_commit(local.repo.owner, local.repo.name, local.sha, body)

  defp comment_body(report_info) do
    Enum.join(
      [
        if(not is_nil(report_info.prologue), do: "#{report_info.prologue}\n\n"),
        "#{report_info.job_name} job #{crash_verb(report_info)} #{Emoji.sad()}",
        if(is_nil(report_info.extra_info), do: "", else: "\n#{report_info.extra_info}\n"),
        report_command("see the full build log", report_info.build_log_command),
        report_command("restart the build", report_info.restart_command),
        report_command("start the remote console", report_info.remote_console_command),
        "Log tail:\n",
        "```",
        log_tail(report_info),
        "```"
      ],
      "\n"
    )
  end

  defp crash_verb(%__MODULE__{result: :error}), do: "errored"
  defp crash_verb(%__MODULE__{result: :failure}), do: "crashed"

  defp report_command(what, command) do
    if is_nil(command),
      do: "",
      else: "You can #{what} by running: `#{command}`\n"
  end

  defp log_tail(report_info) do
    max_lines = 100
    lines = report_info.project |> LocalProject.log_contents(report_info.log_name) |> String.split("\n")

    lines
    |> Enum.drop(max(length(lines) - max_lines, 0))
    |> Enum.join("\n")
  end
end
