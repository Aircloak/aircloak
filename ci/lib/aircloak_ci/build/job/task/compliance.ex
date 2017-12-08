  defmodule AircloakCI.Build.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.Job


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


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp mergeable?(pr), do:
    pr.mergeable? and pr.merge_sha != nil

  defp forced?(build_state), do:
    LocalProject.forced?(build_state.project, "compliance")

  defp maybe_start_test(%{source: pr} = build_state) do
    if not LocalProject.finished?(build_state.project, "compliance") or forced?(build_state) do
      case check_start_preconditions(build_state) do
        :ok -> start_test(build_state)

        {:error, status} ->
          Job.send_github_status(pr.repo, pr.sha, "compliance", pr.status_checks, :pending, status)
          build_state
      end
    else
      build_state
    end
  end

  defp check_start_preconditions(build_state) do
    if forced?(build_state) do
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
    Build.Server.start_job(build_state, __MODULE__, fn -> send(me, {__MODULE__, run_test(pr, project)}) end)
  end

  defp run_test(pr, project) do
    Job.send_github_status(pr.repo, pr.sha, "compliance", pr.status_checks, :pending, "build started")
    with {:error, reason} <- Job.run_queued(:compliance, project, fn -> execute_compliance(project) end) do
      LocalProject.log(project, "compliance", "error: #{reason}")
      :error
    end
  end

  defp execute_compliance(project) do
    if Application.get_env(:aircloak_ci, :simulate_compliance, false) do
      IO.puts("simulating compliance execution")
      :timer.sleep(:timer.seconds(1))
    else
      LocalProject.cmd(project, "compliance", "ci/scripts/run.sh cloak_compliance", timeout: :timer.minutes(10))
    end
  end
end
