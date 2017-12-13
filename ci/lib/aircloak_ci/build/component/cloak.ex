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
  def compile(project, _name, _log_name) do
    with :ok <- build_cloak_image(project), do:
      compile_cloak(LocalProject.ci_version(project), project)
  end


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

  defp build_cloak_image(project), do:
    Job.build_docker_image(project, "cloak", "ci/scripts/run.sh build_cloak")

  defp compile_cloak(1, _project), do: :ok
  defp compile_cloak(ci_version, project) when ci_version >= 2, do:
    run_in_cloak(project, [
      "make deps",
      "MIX_ENV=dev mix compile",
      "MIX_ENV=test mix compile",
      "MIX_ENV=prod mix compile",
    ])

  defp run_standard_test(build_server, project), do:
    Job.run_queued(:standard_test, project,
      fn ->
        with {:error, reason} <-
          run_in_cloak(project, [
            "MIX_ENV=dev mix compile --force --warnings-as-errors",
            "MIX_ENV=test mix compile --force --warnings-as-errors",
            "MIX_ENV=prod mix compile --force --warnings-as-errors",
            "mix lint",
            "MIX_ENV=test mix lint",
            # hacky solution for recreating the test database
            "MIX_ENV=test mix gen.test_data dockerized_ci 1",
            "mix test",
          ])
        do
          LocalProject.log(project, standard_test_job_name(), "error: #{reason}")
          :error
        end
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
