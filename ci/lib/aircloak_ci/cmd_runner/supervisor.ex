defmodule AircloakCI.CmdRunner.Supervisor do
  @moduledoc "Supervisor of commands started by `AircloakCI.CmdRunner`."

  alias Aircloak.ChildSpec

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the new runner process."
  @spec start_runner(pid, lock_start?: boolean) :: {:ok, pid}
  def start_runner(owner, opts) do
    wrapper = if Keyword.get(opts, :lock_start?, true) == true, do: &lock_start/1, else: & &1.()
    wrapper.(fn -> DynamicSupervisor.start_child(__MODULE__, {AircloakCI.CmdRunner, owner}) end)
  end

  @doc "Returns the count of currently running commands."
  @spec job_count() :: non_neg_integer
  def job_count(), do: DynamicSupervisor.count_children(__MODULE__).active

  @doc "Invokes the provided lambda, making sure that no command is started until the lambda returns."
  @spec lock_start((() -> result)) :: result when result: var
  def lock_start(fun), do: AircloakCI.Queue.exec(:start_command, fun)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    ChildSpec.dynamic_supervisor(name: __MODULE__)
  end
end
