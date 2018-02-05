defmodule AircloakCI.CmdRunner.Supervisor do
  @moduledoc "Supervisor of commands started by `AircloakCI.CmdRunner`."

  alias Aircloak.ChildSpec

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the new runner process."
  @spec start_runner(pid) :: {:ok, pid}
  def start_runner(owner), do:
    DynamicSupervisor.start_child(__MODULE__, {AircloakCI.CmdRunner, owner})


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.dynamic_supervisor(name: __MODULE__)
  end
end
