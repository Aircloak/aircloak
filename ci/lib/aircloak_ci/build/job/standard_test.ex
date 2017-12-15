defmodule AircloakCI.Build.Job.StandardTest do
  @moduledoc "Execution of standard tests."

  alias AircloakCI.Build
  alias AircloakCI.Build.{Component, Job}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @spec run(Server.state) :: Server.state
  @doc "Starts test jobs for all components."
  def run(build_state), do:
    Enum.reduce(components(), build_state, &start_standard_test(&2, &1))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_standard_test(%{project: project, source: source} = build_state, component), do:
    Job.maybe_start(build_state, "#{component}_test", &start_test(&1, self(), project, source, component))

  defp start_test(build_state, build_server, project, source, component), do:
    Build.Server.start_job(build_state, "#{component}_test",
      fn ->
        Component.start_job(project, component, :standard_test,
          report_result: build_server, job_name: "#{component}_test")
      end,
      report_status: {source.repo, source.sha}
    )

  defp components(), do:
    ["cloak"]
end
