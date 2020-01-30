defmodule Air.PsqlServer.ShadowDb.Database do
  @moduledoc "Top-level supervisor of a single shadow database."

  alias Air.Schemas.User

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the pid of the database process related to the given data source."
  @spec whereis(User.t(), String.t()) :: pid | nil
  def whereis(user, data_source_name), do: GenServer.whereis(name(user, data_source_name))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(user, data_source_name), do: Air.PsqlServer.ShadowDb.registered_name(user, data_source_name, __MODULE__)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec({user, data_source_name}) do
    Aircloak.ChildSpec.supervisor(
      [
        {Air.PsqlServer.ShadowDb.Manager, {user, data_source_name}},
        {Air.PsqlServer.ShadowDb.ConnectionPool, {user, data_source_name}}
      ],
      name: name(user, data_source_name),
      strategy: :one_for_one
    )
    |> Supervisor.child_spec(id: {__MODULE__, data_source_name})
  end
end
