defmodule Cloak.DataSource.Connection.Pool do
  @moduledoc """
  Pooling of data source connections.

  This module supports custom pooling functionality, which allows us to reuse database connections in the cases when
  many queries are frequently issued.

  The properties of the connection pool are somewhat different than typical Erlang/Elixir pools, such as poolboy, so
  we made our own custom implementation. In particular, this pool has the following properties:

    - The pool can hold arbitrary number of connections.
    - When a connection is returned to the pool, it can be reused by future clients. If the connection is not checked
      out in some period, it will be closed.

  Each distinct data source (uniquely identified by the driver and the connection parameters) is managed by its own
  pool, which is a `GenServer`.

  The connection is established and managed by a separate process, powered by the `Cloak.DataSource.Connection` module.
  Such approach simplifies the implementation of the pool server (which doesn't have to juggle with timers), and also
  improves throughput, since multiple clients can simultaneously create a connection.
  """

  use Parent.GenServer
  require Logger
  alias Aircloak.ChildSpec
  alias Cloak.DataSource.Connection

  @type checkout_options :: [force_new_connection: boolean]

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Checks out the connection from the connection pool."
  @spec checkout(Cloak.DataSource.t(), checkout_options) :: pid
  def checkout(data_source, options \\ []) do
    Logger.debug(fn -> "Acquiring connection to `#{data_source.name}` ..." end)

    data_source
    |> pool_server()
    |> GenServer.call({:checkout, options})
  end

  @doc "Returns the connection to the pool."
  @spec checkin(pid, pid) :: :ok
  def checkin(pool_pid, connection \\ self()), do: GenServer.cast(pool_pid, {:checkin, connection})

  @doc "Removes the connection from the pool."
  @spec remove_connection(pid, reference) :: :ok
  def remove_connection(pool_pid, checkout_id), do: GenServer.cast(pool_pid, {:remove_connection, self(), checkout_id})

  @doc "Removes all connections associated with the specified data source from the pool."
  @spec cleanup(DataSource.t()) :: :ok
  def cleanup(data_source), do: data_source |> pool_server() |> GenServer.cast(:cleanup)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({driver, connection_params}), do: {:ok, %{driver: driver, connection_params: connection_params}}

  @impl GenServer
  def handle_call({:checkout, options}, _from, state) do
    checkout_id = make_ref()

    connection =
      if Keyword.get(options, :force_new_connection, false),
        do: new_connection(state, checkout_id),
        else: available_connection(checkout_id) || new_connection(state, checkout_id)

    Parent.GenServer.update_child_meta(child_id!(connection), &%{&1 | available?: false, checkout_id: checkout_id})
    {:reply, connection, state}
  end

  @impl GenServer
  def handle_cast({:checkin, connection}, state) do
    Parent.GenServer.update_child_meta(child_id!(connection), &%{&1 | available?: true})
    {:noreply, state}
  end

  def handle_cast({:remove_connection, connection, checkout_id}, state) do
    child_id = child_id!(connection)
    {:ok, %{checkout_id: ^checkout_id}} = Parent.GenServer.child_meta(child_id)
    remove_connection(connection)
    {:noreply, state}
  end

  @impl GenServer
  def handle_cast(:cleanup, state), do: {:noreply, remove_all(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp child_id!(connection) do
    {:ok, id} = Parent.GenServer.child_id(connection)
    id
  end

  defp new_connection(state, checkout_id) do
    start = {Connection, :start_link, [state.driver, state.connection_params, checkout_id]}

    {:ok, conn} =
      Parent.GenServer.start_child(%{
        id: {:connection, make_ref()},
        meta: %{available?: true, checkout_id: checkout_id},
        start: start
      })

    conn
  end

  defp available_connection(checkout_id) do
    with connection when not is_nil(connection) <- Enum.find(connections(), & &1.available?) do
      Connection.set_checkout_id(connection.pid, checkout_id)
      connection.pid
    end
  end

  defp connections() do
    Parent.GenServer.children()
    |> Stream.filter(&match?({{:connection, _ref}, _pid, _meta}, &1))
    |> Enum.map(fn {{:connection, _ref}, pid, meta} -> Map.put(meta, :pid, pid) end)
  end

  defp stop_connection(connection) do
    Logger.debug("Removing an idle connection from the pool")
    Connection.stop(connection)
  end

  defp remove_connection(connection) do
    # mark the connection as not available so it's not checked out
    connection |> child_id!() |> Parent.GenServer.update_child_meta(&%{&1 | available?: false})

    # stoping the connection from a task to avoid longer disconnect blocking the pool process
    Parent.GenServer.start_child(%{
      id: {:shutdown, make_ref()},
      start: {Task, :start_link, [fn -> stop_connection(connection) end]}
    })
  end

  defp remove_all(state) do
    case make_ref() |> available_connection() do
      nil ->
        state

      connection ->
        remove_connection(connection)
        remove_all(state)
    end
  end

  @doc false
  def pool_server(data_source) do
    case Registry.lookup(__MODULE__.Registry, {data_source.driver, data_source.parameters}) do
      [{pid, _}] ->
        pid

      [] ->
        case DynamicSupervisor.start_child(__MODULE__.Supervisor, %{
               id: __MODULE__.Server,
               start: {__MODULE__, :start_server, [data_source.driver, data_source.parameters]}
             }) do
          {:ok, pid} -> pid
          {:error, {:already_started, pid}} -> pid
        end
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_) do
    ChildSpec.supervisor(
      [
        ChildSpec.registry(:unique, __MODULE__.Registry),
        ChildSpec.dynamic_supervisor(name: __MODULE__.Supervisor)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  @doc false
  def start_server(driver, connection_params) do
    Parent.GenServer.start_link(
      __MODULE__,
      {driver, connection_params},
      name: {:via, Registry, {__MODULE__.Registry, {driver, connection_params}}}
    )
  end
end
