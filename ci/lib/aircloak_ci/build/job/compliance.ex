defmodule AircloakCI.Build.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.{Component, Job}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Invokes the compliance job."
  @spec start_if_possible(Build.Server.state()) :: Build.Server.state()
  def start_if_possible(build_state) do
    case LocalProject.job_outcome(build_state.project, "cloak_compile") do
      nil -> Job.Compile.start_if_possible(build_state, "cloak")
      :ok -> Job.maybe_start(build_state, "compliance", &start_test(&1, self()))
      _ -> build_state
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_test(%{project: project} = build_state, build_server),
    do:
      Build.Server.start_job(build_state, "compliance", fn ->
        Component.start_job(
          project,
          "cloak",
          :compliance,
          report_result: build_server,
          log_name: "compliance"
        )
      end)
end
