defmodule AircloakCI.Build.Supervisor do
  @moduledoc "Supervisor of individual build servers."

  use Aircloak.ChildSpec.Supervisor

  @doc "Starts the new build server."
  @spec start_build(module, [any]) :: Supervisor.on_start_child
  def start_build(module, args), do:
    Supervisor.start_child(__MODULE__, [module, :start_link, args])


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do:
    Supervisor.start_link(
      [build_worker_spec()],
      strategy: :simple_one_for_one, name: __MODULE__
    )

  defp build_worker_spec(), do:
    %{
      id: __MODULE__.BuildChild,
      start: {Kernel, :apply, []},
      restart: :permanent,
      shutdown: 5000,
      type: :worker
    }
end
