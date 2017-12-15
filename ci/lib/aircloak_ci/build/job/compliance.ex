  defmodule AircloakCI.Build.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.{Container, Build, LocalProject}
  alias AircloakCI.Build.Job


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
      fn -> run_test(build_server, project) end,
      report_status: {pr.repo, pr.sha}
    )

  defp run_test(build_server, project) do
    Job.run_queued(:compliance, project,
      fn -> execute_compliance(LocalProject.ci_version(project), project) end,
      report_result: build_server
    )
  end

  defp execute_compliance(ci_version, project) when ci_version < 4, do:
    LocalProject.cmd(project, "compliance", "ci/scripts/run.sh cloak_compliance", timeout: :timer.hours(1))
  defp execute_compliance(ci_version, project) when ci_version >= 4, do:
    Container.with(
        script(project),
        LocalProject.log_file(project, "compliance"),
        fn(cloak) ->
          with :ok <- prepare_for_compliance(cloak), do:
            Container.exec(cloak, LocalProject.commands(project, "cloak", :compliance), timeout: :timer.hours(1))
        end
      )

  defp prepare_for_compliance(cloak), do:
    Container.invoke_script(cloak, "prepare_for_compliance #{cloak.name}", timeout: :timer.minutes(30))

  defp script(project), do:
    project |> LocalProject.src_folder() |> Path.join("ci/scripts/cloak.sh")
end
