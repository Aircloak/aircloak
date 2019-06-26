defmodule Air.PsqlServer.ShadowDb.ConnectionPool do
  @moduledoc "Manages a pool of connections to the single shadow database."

  alias Air.PsqlServer.ShadowDb.ConnectionOwner
  alias Air.Schemas.User

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the given SQL query."
  @spec query(User.t(), String.t(), String.t(), [term]) :: Air.PsqlServer.ShadowDb.Connection.query_result()
  def query(user, data_source_name, query, params),
    do: :poolboy.transaction(name(user, data_source_name), &ConnectionOwner.query(&1, query, params))

  @doc "Parses the given SQL query."
  @spec parse(User.t(), String.t(), String.t()) :: Air.PsqlServer.ShadowDb.Connection.parse_result()
  def parse(user, data_source_name, query),
    do: :poolboy.transaction(name(user, data_source_name), &ConnectionOwner.parse(&1, query))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(user, data_source_name), do: Air.PsqlServer.ShadowDb.registered_name(user, data_source_name, __MODULE__)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec({user, data_source_name}) do
    :poolboy.child_spec(
      {__MODULE__, data_source_name},
      [
        name: name(user, data_source_name),
        worker_module: ConnectionOwner,
        max_overflow: 0,
        size: 10
      ],
      # Sending a proplist, because poolboy requires proplist in its typespec here
      [{:user, user}, {:data_source_name, data_source_name}]
    )
  end
end
