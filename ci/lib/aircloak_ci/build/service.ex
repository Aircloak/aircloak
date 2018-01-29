defmodule AircloakCI.Build.Service do
  @moduledoc "Starts the processes needed to start build servers."

  use Aircloak.ChildSpec.Supervisor


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link() do
    import Aircloak.ChildSpec

    Supervisor.start_link(
      [
        registry(:unique, AircloakCI.Build.Registry),
        AircloakCI.Build.Supervisor
      ],
      strategy: :one_for_one, name: __MODULE__
    )
  end
end
