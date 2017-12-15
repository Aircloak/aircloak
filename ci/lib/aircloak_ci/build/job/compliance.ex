  defmodule AircloakCI.Build.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.{Component, Job}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def job_name(), do: "compliance"

  @doc "Invokes the compliance job."
  @spec run(Build.Server.state) :: Build.Server.state
  def run(build_state), do:
    Job.maybe_start(build_state, job_name(),
      fn(build_state) ->
        if not mergeable?(build_state.source),
          do: build_state,
          else: maybe_start_test(build_state)
      end
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp mergeable?(pr), do:
    pr.mergeable? and pr.merge_sha != nil

  defp maybe_start_test(%{source: pr} = build_state) do
    case check_start_preconditions(build_state) do
      :ok -> start_test(self(), build_state)

      {:error, status} ->
        Build.Reporter.report_status(pr.repo, pr.sha, job_name(), pr.status_checks, :pending, status)
        build_state
    end
  end

  defp check_start_preconditions(build_state) do
    if LocalProject.forced?(build_state.project, job_name()) do
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

  defp start_test(build_server, %{source: pr, project: project} = build_state), do:
    Build.Server.start_job(
      build_state,
      job_name(),
      fn -> Component.start_job(project, "cloak", :compliance, report_result: build_server) end,
      report_status: {pr.repo, pr.sha}
    )
end
