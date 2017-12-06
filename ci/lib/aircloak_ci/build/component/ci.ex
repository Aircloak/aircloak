defmodule AircloakCI.Build.Component.CI do
  @moduledoc "Implements CI tasks for the CI component."

  alias AircloakCI.LocalProject
  alias AircloakCI.Build.Task

  @behaviour Task.Compile


  # -------------------------------------------------------------------
  # Task.Compile callbacks
  # -------------------------------------------------------------------

  @impl Task.Compile
  def name(), do: "ci"

  @impl Task.Compile
  def compile(project, name, log_name), do:
    LocalProject.component_cmds(project, name, log_name,
      [
        {"make all", timeout: :timer.minutes(10)},
        {"MIX_ENV=test make compile", timeout: :timer.minutes(10)},
      ]
    )
end
