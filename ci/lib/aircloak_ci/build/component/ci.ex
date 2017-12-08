defmodule AircloakCI.Build.Component.CI do
  @moduledoc "Implements CI jobs for the CI component."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.Job

  @behaviour Job.Compile
  @behaviour Job.StandardTest


  # -------------------------------------------------------------------
  # Job.Compile callbacks
  # -------------------------------------------------------------------

  @impl Job.Compile
  def name(), do: "ci"

  @impl Job.Compile
  def compile(project, name, log_name), do:
    LocalProject.component_cmds(project, name, log_name,
      [
        {"make all", timeout: :timer.minutes(10)},
        {"MIX_ENV=test make compile", timeout: :timer.minutes(10)},
        {"mix dialyze --no-analyse", timeout: :timer.minutes(10)},
      ]
    )


  # -------------------------------------------------------------------
  # Job.StandardTest callbacks
  # -------------------------------------------------------------------

  @impl Job.StandardTest
  def standard_test_job_name(), do: "ci_test"

  @impl Job.StandardTest
  def standard_test(%{project: project, source: source} = build_state), do:
    Job.maybe_start(build_state, standard_test_job_name(),
      fn(build_state) ->
        build_server = self()
        Build.Server.start_job(
          build_state,
          standard_test_job_name(),
          fn -> run_standard_test(build_server, project) end,
          report_status: {source.repo, source.sha}
        )
      end
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_standard_test(build_server, project), do:
    Job.run_queued(:standard_test, project,
      fn ->
        with {:error, reason} <-
          LocalProject.component_cmds(project, "ci", standard_test_job_name(),
            [{"make docs lint dialyze test", timeout: :timer.minutes(10)}]
          )
        do
          LocalProject.log(project, standard_test_job_name(), "error: #{reason}")
          :error
        end
      end,
      job_name: standard_test_job_name(),
      log_name: standard_test_job_name(),
      report_result: build_server,
    )
end
