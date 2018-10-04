defmodule AircloakCI.CmdRunner.Supervisor do
  @moduledoc "Supervisor of commands started by `AircloakCI.CmdRunner`."

  alias Aircloak.ChildSpec

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the new runner process."
  @spec start_runner(pid) :: {:ok, pid}
  def start_runner(owner), do: DynamicSupervisor.start_child(__MODULE__, {AircloakCI.CmdRunner, owner})

  @doc "Returns the count of currently running commands."
  @spec job_count() :: non_neg_integer
  def job_count(), do: DynamicSupervisor.count_children(__MODULE__).active
  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.dynamic_supervisor(name: __MODULE__)
  end
end
