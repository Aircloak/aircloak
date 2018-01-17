  defmodule AircloakCI.Build.Job.Compliance do
  @moduledoc "Execution of the compliance test suite."

  alias AircloakCI.Build
  alias AircloakCI.Build.{Component, Job}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Invokes the compliance job."
  @spec run(Build.Server.state) :: Build.Server.state
  def run(build_state), do:
    Job.maybe_start(build_state, "compliance", &start_test(&1, self()))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_test(%{project: project} = build_state, build_server), do:
    Build.Server.start_job(
      build_state,
      "compliance",
      fn -> Component.start_job(project, "cloak", :compliance, report_result: build_server, log_name: "compliance") end
    )
end
