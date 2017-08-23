defmodule Air.PsqlServer.BackendProcessRegistry do
  @moduledoc """
  Maintains a list of active psql connections and their associated process id and secret key.
  Along with that it maintains information about the running query of a given process.

  This allows clients to cancel running queries, which happens through a separate channel,
  hence the need for this server.
  """

  use GenServer

  alias Air.Service.{Query, User, DataSource}

  require Logger

  @max_int32 2_147_483_647

  @type backend_key_data :: %{process_id: integer, secret_key: integer}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the queue process."
  @spec start_link() :: GenServer.on_start
  def start_link(), do:
    GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc "Register a connection, producing a process id and key."
  @spec register() :: backend_key_data
  def register(), do:
    GenServer.call(__MODULE__, :register)

  @doc "Unregisters a connection"
  @spec unregister(backend_key_data) :: :ok
  def unregister(nil), do: :ok
  def unregister(key), do: GenServer.cast(__MODULE__, {:unregister, key})

  @doc "Registers a running query, so that it can be cancelled"
  @spec register_query(backend_key_data, non_neg_integer, String.t) :: :ok
  def register_query(key, user_id, query_id), do:
    GenServer.cast(__MODULE__, {:register_query, key, {user_id, query_id}})

  @doc """
  Attempts to cancel a query. Returns immediately irrespective of
  whether or not the query, backend process exists, or whether or
  not the request was succesful.
  """
  @spec cancel_query(backend_key_data) :: :ok
  def cancel_query(backend_key_data), do:
    GenServer.cast(__MODULE__, {:cancel, backend_key_data})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init([]), do:
    {:ok, Map.new()}

  @doc false
  def handle_call(:register, _from, state) do
    {:ok, response, new_state} = create_registry_entry(state)
    {:reply, response, new_state}
  end

  def handle_cast({:unregister, key}, state), do:
    {:noreply, Map.delete(state, key)}
  def handle_cast({:register_query, key, query_info}, state), do:
    {:noreply, Map.put(state, key, query_info)}
  def handle_cast({:cancel, key}, state) do
    case Map.get(state, key, :idle) do
      :idle -> :ok
      {user_id, query_id} ->
        user = User.load(user_id)
        case Query.get_as_user(user, query_id) do
          {:ok, query} ->
            Logger.debug("Issued request to cancel query: #{query_id} on behalf of user #{user_id}")
            DataSource.stop_query(query, user)
          {:error, _reason} -> :ok
        end
    end
    {:noreply, Map.put(state, key, :idle)}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp create_registry_entry(map) do
    process_id = :rand.uniform(@max_int32)
    secret_key = :rand.uniform(@max_int32)
    key = %{process_id: process_id, secret_key: secret_key}
    case Map.get(map, key) do
      nil -> {:ok, key, Map.put(map, key, :idle)}
      _ -> create_registry_entry(map)
    end
  end
end
