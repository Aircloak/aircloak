defmodule AircloakCI.Build.Supervisor do
  @moduledoc "Supervisor of individual build servers."

  use Aircloak.ChildSpec.Supervisor

  @doc "Starts the new build server."
  @spec start_build(module, [any]) :: Supervisor.on_start_child()
  def start_build(module, args), do: DynamicSupervisor.start_child(__MODULE__, build_worker_spec(module, args))

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(),
    do: DynamicSupervisor.start_link(strategy: :one_for_one, name: __MODULE__)

  defp build_worker_spec(module, args),
    do: %{
      id: __MODULE__.BuildChild,
      start: {module, :start_link, args},
      restart: :temporary,
      shutdown: 5000,
      type: :worker
    }
end
