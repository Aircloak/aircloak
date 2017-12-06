defmodule AircloakCI.Build.Task.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.{Github, JobRunner, LocalProject, Queue}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @spec run(JobRunner.state) :: JobRunner.state
  def run(job_runner_state) do
    if not mergeable?(job_runner_state.source) do
      job_runner_state
    else
      maybe_start_test(job_runner_state)
    end
  end

  @spec handle_finish(JobRunner.state, :ok | :error | :failure, any) :: JobRunner.state
  def handle_finish(job_runner_state, result, context) do
    diff_sec = :erlang.monotonic_time(:second) - job_runner_state.data.start
    time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

    LocalProject.log(job_runner_state.project, "compliance", "finished with result `#{result}` in #{time_output} min")

    send_status_to_github(job_runner_state.source, github_status(result), description(result))

    Github.post_comment(
      job_runner_state.source.repo.owner,
      job_runner_state.source.repo.name,
      job_runner_state.source.number,
      comment(job_runner_state, result, context)
    )

    LocalProject.mark_finished(job_runner_state.project)

    job_runner_state
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp mergeable?(pr), do:
    pr.mergeable? and pr.merge_sha != nil

  defp maybe_start_test(job_runner_state) do
    if not LocalProject.finished?(job_runner_state.project) or LocalProject.forced?(job_runner_state.project) do
      case check_start_preconditions(job_runner_state) do
        :ok -> start_test(job_runner_state)

        {:error, status} ->
          send_status_to_github(job_runner_state.source, :pending, status)
          job_runner_state
      end
    else
      job_runner_state
    end
  end

  defp check_start_preconditions(job_runner_state) do
    if LocalProject.forced?(job_runner_state.project) do
      :ok
    else
      with \
        {_status, true} <- {"waiting for Travis builds to succeed", travis_succeeded?(job_runner_state.source)},
        {_status, true} <- {"waiting for approval", job_runner_state.source.approved?}
      do
        :ok
      else
        {error, false} -> {:error, error}
      end
    end
  end

  defp travis_succeeded?(pr), do:
    (pr.status_checks["continuous-integration/travis-ci/pr"] || %{status: nil}).status == :success and
    (pr.status_checks["continuous-integration/travis-ci/push"] || %{status: nil}).status == :success

  defp start_test(%{source: pr, project: project} = job_runner_state) do
    me = self()
    job_runner_state = %{job_runner_state | data: %{start: :erlang.monotonic_time(:second)}}
    JobRunner.start_task(job_runner_state, __MODULE__, fn -> send(me, {__MODULE__, run_test(pr, project)}) end)
  end

  defp run_test(pr, project) do
    send_status_to_github(pr, :pending, "build started")
    with {:error, reason} <- execute_compliance(project) do
      LocalProject.log(project, "compliance", "error: #{reason}")
      :error
    end
  end

  defp execute_compliance(project), do:
    LocalProject.log_start_stop(project, "running compliance for #{LocalProject.name(project)}",
      fn ->
        LocalProject.truncate_log(project, "compliance")
        LocalProject.log(project, "compliance", "waiting in compliance queue")
        Queue.exec(:compliance, fn ->
          if Application.get_env(:aircloak_ci, :simulate_compliance, false) do
            IO.puts("simulating compliance execution")
            :timer.sleep(:timer.seconds(1))
          else
            LocalProject.cmd(project, "compliance", "ci/scripts/run.sh cloak_compliance", timeout: :timer.minutes(10))
          end
        end)
      end
    )


  # -------------------------------------------------------------------
  # Communication with Github
  # -------------------------------------------------------------------

  defp send_status_to_github(pr, status, description) do
    status_context = "continuous-integration/aircloak/compliance"
    current_description = (pr.status_checks[status_context] || %{description: nil}).description
    if description != current_description, do:
      Github.put_status_check_state(pr.repo.owner, pr.repo.name, pr.sha, status_context, description, status)
  end

  defp github_status(:ok), do: :success
  defp github_status(:error), do: :error
  defp github_status(:failure), do: :failure

  defp description(:ok), do: "build succeeded"
  defp description(:error), do: "build errored"
  defp description(:failure), do: "build failed"

  defp comment(_job_runner_state, :ok, nil), do:
    "Compliance build succeeded #{happy_emoji()}"
  defp comment(job_runner_state, :error, nil), do:
    error_comment(job_runner_state, "Compliance build errored")
  defp comment(job_runner_state, :failure, crash_reason), do:
    error_comment(job_runner_state, "Compliance build crashed", "```\n#{Exception.format_exit(crash_reason)}\n```")

  defp error_comment(job_runner_state, title, extra_info \\ nil), do:
    Enum.join(
      [
        "#{title} #{sad_emoji()}",
        (if not is_nil(extra_info), do: "\n#{extra_info}\n", else: ""),
        "You can see the full build log by running: `ci/production.sh build_log #{job_runner_state.source.number}`\n",
        "Log tail:\n", "```", log_tail(job_runner_state.project), "```"
      ],
      "\n"
    )

  defp happy_emoji(), do: Enum.random(["💯", "👍", "😊", "❤️", "🎉", "👏"])

  defp sad_emoji(), do: Enum.random(["😞", "😢", "😟", "💔", "👿", "🔥"])

  defp log_tail(project) do
    max_lines = 100
    lines = project |> LocalProject.log_contents("compliance") |> String.split("\n")

    lines
    |> Enum.drop(max(length(lines) - max_lines, 0))
    |> Enum.join("\n")
  end
end
