defmodule Air.PsqlServer.BackendProcessRegistry do
  @moduledoc """
  Maintains a list of active psql connections and their associated process id and secret key.
  Along with that it maintains information about the running query of a given process.

  This allows clients to cancel running queries, which happens through a separate channel,
  hence the need for this server.
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

  @doc "Register a connection, producing a process id and key."
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
  Attempts to cancel a query. Returns immediately irrespective of
  whether or not the query, backend process exists, or whether or
  not the request was succesful.
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
