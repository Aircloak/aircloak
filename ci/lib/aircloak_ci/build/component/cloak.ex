defmodule AircloakCI.Build.Component.Cloak do
  @moduledoc "Implements CI jobs for the Cloak component."

  alias AircloakCI.LocalProject
  alias AircloakCI.Build.Job

  @behaviour Job.Compile


  # -------------------------------------------------------------------
  # Job.Compile callbacks
  # -------------------------------------------------------------------

  @impl Job.Compile
  def name(), do: "cloak"

  @impl Job.Compile
  def compile(project, _name, log_name), do:
    LocalProject.cmd(project, log_name, "ci/scripts/run.sh build_cloak", timeout: :timer.minutes(30))
end
