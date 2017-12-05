defmodule AircloakCI.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  use AircloakCI.JobRunner.PullRequest
  require Logger
  alias AircloakCI.{Github, JobRunner, LocalProject}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the compliance job."
  @spec start_link(Github.API.pull_request, LocalProject.t, Github.API.repo_data) :: {:ok, pid} | :ignore
  def start_link(pr, project, repo_data) do
    if LocalProject.finished?(project) or not mergeable?(pr) do
      :ignore
    else
      JobRunner.start_link(__MODULE__, pr, repo_data, nil)
    end
  end


  # -------------------------------------------------------------------
  # JobRunner callbacks
  # -------------------------------------------------------------------

  @impl JobRunner
  def init(nil, state) do
    LocalProject.log(state.project, "compliance", "starting the compliance build")
    {:ok, maybe_start_test(%{state | data: %{start: nil}})}
  end

  @impl JobRunner
  def handle_source_change(state), do:
    {:noreply, (if running?(state), do: state, else: maybe_start_test(state))}

  @impl JobRunner
  def handle_job_failed(:compliance_build, crash_reason, state) do
    handle_build_finish(state, :failure, crash_reason)
    {:stop, :normal, state}
  end

  @impl JobRunner
  def handle_info({:compliance_result, result}, state) do
    handle_build_finish(state, result, nil)
    {:stop, :normal, state}
  end


  # -------------------------------------------------------------------
  # Compliance execution
  # -------------------------------------------------------------------

  defp running?(state), do: not is_nil(state.data.start)

  defp maybe_start_test(state) do
    case check_start_preconditions(state) do
      :ok -> start_test(state)

      {:error, status} ->
        send_status_to_github(state.source, :pending, status)
        state
    end
  end

  defp check_start_preconditions(state) do
    if LocalProject.status(state.project) == :force_start do
      :ok
    else
      with \
        {_status, true} <- {"waiting for Travis builds to succeed", travis_succeeded?(state.source)},
        {_status, true} <- {"waiting for approval", state.source.approved?}
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

  defp start_test(%{source: pr, project: project} = state) do
    me = self()
    state = %{state | data: %{start: :erlang.monotonic_time(:second)}}
    JobRunner.start_job(state, :compliance_build, fn ->
      Task.start_link(fn -> send(me, {:compliance_result, run_test(pr, project)}) end)
    end)
  end

  defp run_test(pr, project) do
    send_status_to_github(pr, :pending, "build started")
    LocalProject.set_status(project, :started)
    with {:error, reason} <- LocalProject.compliance(project) do
      LocalProject.log(project, "compliance", "error: #{reason}")
      :error
    end
  end

  defp handle_build_finish(state, result, context) do
    diff_sec = :erlang.monotonic_time(:second) - state.data.start
    time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

    LocalProject.log(state.project, "compliance", "finished with result `#{result}` in #{time_output} min")

    send_status_to_github(state.source, github_status(result), description(result))

    Github.post_comment(
      state.source.repo.owner,
      state.source.repo.name,
      state.source.number,
      comment(state, result, context)
    )

    LocalProject.set_status(state.project, :finished)
  end

  defp mergeable?(pr), do:
    pr.mergeable? and pr.merge_sha != nil


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

  defp comment(_state, :ok, nil), do:
    "Compliance build succeeded #{happy_emoji()}"
  defp comment(state, :error, nil), do:
    error_comment(state, "Compliance build errored")
  defp comment(state, :failure, crash_reason), do:
    error_comment(state, "Compliance build crashed", "```\n#{Exception.format_exit(crash_reason)}\n```")

  defp error_comment(state, title, extra_info \\ nil), do:
    Enum.join(
      [
        "#{title} #{sad_emoji()}",
        (if not is_nil(extra_info), do: "\n#{extra_info}\n", else: ""),
        "You can see the full build log by running: `ci/production.sh build_log #{state.source.number}`\n",
        "Log tail:\n", "```", log_tail(state.project), "```"
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
