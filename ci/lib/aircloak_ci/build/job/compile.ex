defmodule AircloakCI.Build.Job.Compile do
  @moduledoc "Compilation of all components in a local project."

  alias AircloakCI.{Build, LocalProject}
  alias AircloakCI.Build.{Component, Job}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the compilation job for all the components in the given project."
  @spec start_if_possible(Build.Server.state) :: Build.Server.state
  def start_if_possible(build_state), do:
    Enum.reduce(LocalProject.components(build_state.project), build_state, &start_if_possible(&2, &1))

  @doc "Starts the compilation job for the given component in the given project."
  @spec start_if_possible(Build.Server.state, String.t) :: Build.Server.state
  def start_if_possible(%{project: project} = build_state, component), do:
    Job.maybe_start(build_state, "#{component}_compile", &start_compilation(&1, self(), project, component))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_compilation(build_state, build_server, project, component), do:
    Build.Server.start_job(build_state, "#{component}_compile",
      fn ->
        Component.start_job(project, component, :compile,
          report_result: build_server, job_name: "#{component}_compile")
      end
    )
end
