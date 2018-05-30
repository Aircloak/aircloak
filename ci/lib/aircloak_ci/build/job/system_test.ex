defmodule AircloakCI.Build.Job.SystemTest do
  @moduledoc "Execution of the system test."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.{Component, Job}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Invokes the compliance job."
  @spec start_if_possible(Build.Server.state()) :: Build.Server.state()
  def start_if_possible(build_state) do
    if LocalProject.system_test?(build_state.project) do
      case LocalProject.job_outcome(build_state.project, "system_test_compile") do
        nil -> Job.Compile.start_if_possible(build_state, "system_test")
        :ok -> Job.maybe_start(build_state, "system_test", &start_test(&1, self()))
        _ -> build_state
      end
    else
      build_state
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_test(%{project: project} = build_state, build_server),
    do:
      Build.Server.start_job(build_state, "system_test", fn ->
        Component.start_job(
          project,
          "system_test",
          :system_test,
          report_result: build_server,
          log_name: "system_test"
        )
      end)
end
