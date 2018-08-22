defmodule Air.Service.ShadowDb.Database do
  @moduledoc "Top-level supervisor of a single shadow database."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the pid of the database process related to the given data source."
  @spec whereis(String.t()) :: pid | nil
  def whereis(data_source_name), do: GenServer.whereis(name(data_source_name))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(data_source_name), do: Air.Service.ShadowDb.registered_name(data_source_name, __MODULE__)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(data_source_name) do
    Aircloak.ChildSpec.supervisor(
      [
        {Air.Service.ShadowDb.Server, data_source_name}
      ],
      name: name(data_source_name),
      strategy: :one_for_one
    )
    |> Supervisor.child_spec(id: {__MODULE__, data_source_name})
  end
end
