defmodule AircloakCI.Build.Job.Test do
  @moduledoc "Execution of standard tests."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.{Component, Job}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts test jobs for all components."
  @spec start(Server.state) :: Server.state
  def start(build_state), do:
    Enum.reduce(LocalProject.components(build_state.project), build_state, &start(&2, &1))

  @doc "Starts test job for the given component."
  @spec start(Server.state, String.t) :: Server.state
  def start(build_state, component) do
    case LocalProject.job_outcome(build_state.project, "#{component}_compile") do
      nil -> Job.Compile.start(build_state, component)
      :ok -> Job.maybe_start(build_state, "#{component}_test", &start_test(&1, self(), build_state.project, component))
      _ -> build_state
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_test(build_state, build_server, project, component), do:
    Build.Server.start_job(build_state, "#{component}_test",
      fn ->
        Component.start_job(project, component, :test,
          report_result: build_server, job_name: "#{component}_test")
      end
    )
end
