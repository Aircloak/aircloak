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

  @type checkout_options :: [force_new_connection: boolean, connect_opts: [retries: non_neg_integer]]

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
  def checkin(pool_pid, connection \\ self()), do: GenServer.call(pool_pid, {:checkin, connection})

  @doc "Removes the connection from the pool."
  @spec remove_connection(pid, pid) :: :ok
  def remove_connection(pool_pid, connection \\ self()), do: GenServer.cast(pool_pid, {:remove_connection, connection})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({driver, connection_params}), do: {:ok, %{driver: driver, connection_params: connection_params}}

  @impl GenServer
  def handle_call({:checkout, options}, _from, state) do
    connection =
      if Keyword.get(options, :force_new_connection, false),
        do: new_connection(state, Keyword.get(options, :connect_opts, [])),
        else: available_connection() || new_connection(state, Keyword.get(options, :connect_opts, []))

    Parent.GenServer.update_child_meta(child_id!(connection), &%{&1 | available?: false})
    {:reply, connection, state}
  end

  def handle_call({:checkin, connection}, _from, state) do
    Parent.GenServer.update_child_meta(child_id!(connection), &%{&1 | available?: true})
    {:reply, :ok, state}
  end

  @impl GenServer
  def handle_cast({:remove_connection, connection}, state) do
    Logger.debug("Removing an idle connection from the pool")
    Parent.GenServer.shutdown_child(child_id!(connection))
    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp child_id!(connection) do
    {:ok, id} = Parent.GenServer.child_id(connection)
    id
  end

  defp new_connection(state, connect_opts) do
    start = {Connection, :start_link, [state.driver, state.connection_params, connect_opts]}
    {:ok, conn} = Parent.GenServer.start_child(%{id: make_ref(), meta: %{available?: true}, start: start})
    conn
  end

  defp available_connection() do
    with {_id, conn, _meta} <- Enum.find(Parent.GenServer.children(), fn {_id, _conn, meta} -> meta.available? end),
         do: conn
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
