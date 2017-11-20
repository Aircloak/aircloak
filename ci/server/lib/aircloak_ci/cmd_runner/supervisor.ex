defmodule AircloakCI.CmdRunner.Supervisor do
  @moduledoc "Supervisor of commands started by `AircloakCI.CmdRunner`."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the new runner process."
  @spec start_runner(pid) :: {:ok, pid}
  def start_runner(owner), do:
    Supervisor.start_child(__MODULE__, [owner])


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    import Aircloak.ChildSpec, warn: false

    supervisor([AircloakCI.CmdRunner], strategy: :simple_one_for_one, name: __MODULE__)
  end
end
