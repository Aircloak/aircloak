  defmodule AircloakCI.Build.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.{Github, Build, LocalProject, Queue}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Invokes the compliance job."
  @spec run(Build.Server.state) :: Build.Server.state
  def run(build_state) do
    if not mergeable?(build_state.source) do
      build_state
    else
      maybe_start_test(build_state)
    end
  end

  @doc "Handles the outcome of the compliance job."
  @spec handle_finish(Build.Server.state, :ok | :error | :failure, any) :: Build.Server.state
  def handle_finish(build_state, result, context) do
    diff_sec = :erlang.monotonic_time(:second) - build_state.data.start
    time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

    LocalProject.log(build_state.project, "compliance", "finished with result `#{result}` in #{time_output} min")

    send_status_to_github(build_state.source, github_status(result), description(result))

    Github.post_comment(
      build_state.source.repo.owner,
      build_state.source.repo.name,
      build_state.source.number,
      comment(build_state, result, context)
    )

    LocalProject.mark_finished(build_state.project)

    build_state
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp mergeable?(pr), do:
    pr.mergeable? and pr.merge_sha != nil

  defp maybe_start_test(build_state) do
    if not LocalProject.finished?(build_state.project) or LocalProject.forced?(build_state.project) do
      case check_start_preconditions(build_state) do
        :ok -> start_test(build_state)

        {:error, status} ->
          send_status_to_github(build_state.source, :pending, status)
          build_state
      end
    else
      build_state
    end
  end

  defp check_start_preconditions(build_state) do
    if LocalProject.forced?(build_state.project) do
      :ok
    else
      with \
        {_status, true} <- {"waiting for Travis builds to succeed", travis_succeeded?(build_state.source)},
        {_status, true} <- {"waiting for approval", build_state.source.approved?}
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

  defp start_test(%{source: pr, project: project} = build_state) do
    me = self()
    build_state = %{build_state | data: %{start: :erlang.monotonic_time(:second)}}
    Build.Server.start_job(build_state, __MODULE__, fn -> send(me, {__MODULE__, run_test(pr, project)}) end)
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

  defp comment(_build_state, :ok, nil), do:
    "Compliance build succeeded #{happy_emoji()}"
  defp comment(build_state, :error, nil), do:
    error_comment(build_state, "Compliance build errored")
  defp comment(build_state, :failure, crash_reason), do:
    error_comment(build_state, "Compliance build crashed", "```\n#{Exception.format_exit(crash_reason)}\n```")

  defp error_comment(build_state, title, extra_info \\ nil), do:
    Enum.join(
      [
        "#{title} #{sad_emoji()}",
        (if not is_nil(extra_info), do: "\n#{extra_info}\n", else: ""),
        "You can see the full build log by running: `ci/production.sh build_log #{build_state.source.number}`\n",
        "Log tail:\n", "```", log_tail(build_state.project), "```"
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
