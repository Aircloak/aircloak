defmodule AircloakCI.Build.Job.StandardTest do
  @moduledoc "Execution of standard tests."

  alias AircloakCI.Build.Component


  # -------------------------------------------------------------------
  # Behaviour
  # -------------------------------------------------------------------

  @doc "Invoked to get the standard test job name from the component."
  @callback standard_test_job_name() :: String.t

  @doc "Invoked to start the standard test for the component the component."
  @callback standard_test(Server.state) :: Server.state


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the job names for all components."
  @spec job_names() :: [String.t]
  def job_names(), do:
    Enum.map(components(), &(&1.standard_test_job_name()))

  @spec run(Server.state) :: Server.state
  @doc "Starts test jobs for all components."
  def run(build_state), do:
    Enum.reduce(components(), build_state, &(&1.standard_test(&2)))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp components(), do:
    [Component.Cloak]
end
