defmodule AircloakCI.Build.Component.CI do
  @moduledoc "Implements CI jobs for the CI component."

  alias AircloakCI.LocalProject
  alias AircloakCI.Build.Job

  @behaviour Job.Compile


  # -------------------------------------------------------------------
  # Job.Compile callbacks
  # -------------------------------------------------------------------

  @impl Job.Compile
  def name(), do: "ci"

  @impl Job.Compile
  def compile(project, name, log_name), do:
    LocalProject.component_cmds(project, name, log_name,
      [
        {"make all", timeout: :timer.minutes(10)},
        {"MIX_ENV=test make compile", timeout: :timer.minutes(10)},
      ]
    )
end
