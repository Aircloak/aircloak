defmodule AircloakCI.Build.Service do
  @moduledoc "Starts the processes needed to start build servers."

  alias Aircloak.ChildSpec
  use ChildSpec.Supervisor


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link() do
    Supervisor.start_link(
      [
        ChildSpec.registry(:unique, AircloakCI.Build.Registry),
        AircloakCI.Build.Supervisor
      ],
      strategy: :one_for_one, name: __MODULE__
    )
  end
end
