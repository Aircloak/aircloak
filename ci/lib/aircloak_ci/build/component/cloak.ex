defmodule AircloakCI.Build.Component.Cloak do
  @moduledoc "Implements CI tasks for the Cloak component."

  alias AircloakCI.LocalProject
  alias AircloakCI.Build.Task

  @behaviour Task.Compile


  # -------------------------------------------------------------------
  # Task.Compile callbacks
  # -------------------------------------------------------------------

  @impl Task.Compile
  def name(), do: "cloak"

  @impl Task.Compile
  def compile(project, _name, log_name), do:
    LocalProject.cmd(project, log_name, "ci/scripts/run.sh build_cloak", timeout: :timer.minutes(30))
end
