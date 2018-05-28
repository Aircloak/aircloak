defmodule Cloak.DataSource.ConnectionPool do
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

  The connection is established and managed by a separate GenServer, powered by the
  `Cloak.DataSource.ConnectionPool.ConnectionOwner` module. Such implementation simplifies the implementation of the
  pool server (which doesn't have to juggle with timers), and also improves throughput, since multiple clients can
  simultaneously create a connection.
  """

  use Parent.GenServer
  require Logger
  alias Aircloak.ChildSpec
  alias Cloak.DataSource.Driver
  alias Cloak.DataSource.ConnectionPool.ConnectionOwner

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Acquires a connection, invokes the provided lambda, and returns its result."
  @spec execute!(Cloak.DataSource.t(), (Driver.connection() -> result)) :: result when result: var
  def execute!(data_source, fun) do
    if data_source.driver.supports_connection_sharing?() do
      data_source
      |> pool_server()
      |> GenServer.call(:checkout)
      |> on_connection(fun)
    else
      # needed for drivers which don't support connection sharing, such as ODBC
      connection = Cloak.DataSource.connect!(data_source.driver, data_source.parameters)

      try do
        fun.(connection)
      after
        data_source.driver.disconnect(connection)
      end
    end
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({driver, connection_params}), do: {:ok, %{driver: driver, connection_params: connection_params}}

  @impl GenServer
  def handle_call(:checkout, _from, state) do
    connection = available_connection() || new_connection(state)
    Parent.GenServer.update_child_meta(child_id!(connection), &%{&1 | available?: false})
    {:reply, connection, state}
  end

  def handle_call({:checkin, connection}, _from, state) do
    Parent.GenServer.update_child_meta(child_id!(connection), &%{&1 | available?: true})
    {:reply, :ok, state}
  end

  @impl GenServer
  def handle_cast({:remove_connection, connection}, state) do
    Parent.GenServer.shutdown_child(child_id!(connection))
    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp child_id!(connection) do
    {:ok, id} = Parent.GenServer.child_name(connection)
    id
  end

  defp new_connection(state) do
    start = {GenServer, :start_link, [ConnectionOwner, {self(), state.driver, state.connection_params}]}
    {:ok, conn} = Parent.GenServer.start_child(%{id: make_ref(), meta: %{available?: true}, start: start})
    conn
  end

  defp available_connection() do
    with {_id, conn, _meta} <- Enum.find(Parent.GenServer.children(), fn {_id, _conn, meta} -> meta.available? end),
         do: conn
  end

  defp pool_server(data_source) do
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

  defp on_connection(connection_owner, fun) do
    res =
      connection_owner
      |> start_client_usage()
      |> fun.()

    # To avoid possible corrupt state, we're returning the connection back only on success.
    # On error, we'll terminate the connection owner, and reraise. Finally, this process (client), is monitored
    # by the connection owner, so if it's killed from the outside, the connection owner will terminate.
    # This ensures proper cleanup in all situations.
    GenServer.call(connection_owner, :checkin)

    res
  catch
    type, error ->
      stacktrace = System.stacktrace()
      Process.exit(connection_owner, :kill)
      raise_client_error(type, error, stacktrace)
  end

  defp start_client_usage(connection_owner) do
    GenServer.call(
      connection_owner,
      :start_client_usage,
      :timer.minutes(1) + Driver.connect_timeout()
    )
  catch
    # If this call fails, then we failed to connect to the database, so we're raising an informative exception.
    # This prevents reporting "Unknown cloak error" when connecting to the database fails. Note that the real
    # exit reason will still be properly included in the crash log of the connection owner process.
    _type, _error ->
      Cloak.DataSource.raise_error("Failed connecting to the database")
  end

  defp raise_client_error(:exit, {{%Cloak.Query.ExecutionError{} = error, _}, _}, _stacktrace), do: raise(error)

  defp raise_client_error(:error, %{__exception__: true} = error, stacktrace), do: reraise(error, stacktrace)

  defp raise_client_error(type, error, stacktrace) do
    :erlang.raise(
      :error,
      RuntimeError.exception("Connection error #{inspect(type)}: #{inspect(error)}"),
      stacktrace
    )
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
