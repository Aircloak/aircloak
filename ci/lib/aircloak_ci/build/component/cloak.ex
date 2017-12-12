defmodule AircloakCI.Build.Component.Cloak do
  @moduledoc "Implements CI jobs for the Cloak component."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.Job

  @behaviour Job.Compile
  @behaviour Job.StandardTest


  # -------------------------------------------------------------------
  # Job.Compile callbacks
  # -------------------------------------------------------------------

  @impl Job.Compile
  def name(), do: "cloak"

  @impl Job.Compile
  def compile(project, _name, log_name), do:
    LocalProject.cmd(project, log_name, "ci/scripts/run.sh build_cloak", timeout: :timer.minutes(30))


  # -------------------------------------------------------------------
  # Job.StandardTest callbacks
  # -------------------------------------------------------------------

  @impl Job.StandardTest
  def standard_test_job_name(), do: "cloak_test"

  @impl Job.StandardTest
  def standard_test(%{project: project, source: source} = build_state), do:
    Job.maybe_start(
      build_state,
      standard_test_job_name(),
      &start_test(LocalProject.ci_version(project), &1, self(), project, source)
    )

  defp start_test(ci_version, build_state, build_server, project, source) when ci_version >= 2, do:
    Build.Server.start_job(
      build_state,
      standard_test_job_name(),
      fn -> run_standard_test(build_server, project) end,
      report_status: {source.repo, source.sha}
    )
  defp start_test(_ci_version, build_state, _build_server, _project, _source), do:
    build_state


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_standard_test(build_server, project), do:
    Job.run_queued(:standard_test, project,
      fn ->
        run_in_cloak(project, [
          "mix lint",
          ~s(MIX_ENV='test' mix lint)
        ])
      end,
      job_name: standard_test_job_name(),
      log_name: standard_test_job_name(),
      report_result: build_server,
    )

  defp run_in_cloak(project, cmds), do:
    LocalProject.cmd(
        project,
        standard_test_job_name(),
        "ci/scripts/run.sh run_in_cloak_test #{cmds |> Stream.map(&~s("#{&1}")) |> Enum.join(" ")}",
        timeout: :timer.hours(1)
      )
end
