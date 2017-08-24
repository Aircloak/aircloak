defmodule Air.PsqlServer.ConnectionRegistry do
  @moduledoc """
  This registry maintains an association of process and session ids to queries
  started through the Postgres Message Protocol interface. This allows queries
  to be cancelled using out of bounds connections.
  """

  alias Air.Service.{Query, User, DataSource}
  require Logger

  @type key_data :: %{process_id: integer, secret_key: integer}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the queue process."
  @spec start_link() :: {:ok, pid}
  def start_link(), do:
    Registry.start_link(:unique, __MODULE__)

  @doc """
  Produces a random pair of process and session ids that can be communicated to
  a connecting client and later used to cancel queries.

  Nothing is stored in the registry at the point of generation.

  The IDs are not guaranteed to be unique, neither individually nor collectively.
  However given the space of potential IDs (64-bit integer), the risk of collision
  given any number of realistically concurrently open connections are low.
  """
  @spec register() :: key_data
  def register() do
    <<process_id::32, secret_key::32>> = :crypto.strong_rand_bytes(8)
    %{process_id: process_id, secret_key: secret_key}
  end

  @doc "Registers a running query, so that it can be cancelled"
  @spec register_query(key_data, non_neg_integer, String.t) :: :ok
  def register_query(key, user_id, query_id) do
    value = {user_id, query_id}
    case Registry.register(__MODULE__, key, value) do
      {:ok, _} -> :ok
      {:error, {:already_registered, pid}} ->
        if pid != self() do
          Logger.warn("Key collision in cancellation registry. This query won't be cancellable.")
          :ok
        else
          Registry.update_value(__MODULE__, key, fn(_) -> value end)
          :ok
        end
    end
  end

  @doc """
  Attempts to cancel a query.
  Returns a positive response irrespective of whether the connection existed,
  or had an active query or not.
  """
  @spec cancel_query(key_data) :: :ok
  def cancel_query(key) do
    case Registry.lookup(__MODULE__, key) do
      [] -> :ok
      [{_pid, {user_id, query_id}}] ->
        user = User.load(user_id)
        case Query.get_as_user(user, query_id) do
          {:ok, query} ->
            Logger.debug("Issued request to cancel query: #{query_id} on behalf of user #{user_id}")
            DataSource.stop_query(query, user)
            :ok
          {:error, _reason} -> :ok
        end
    end
  end
end
