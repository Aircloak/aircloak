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

  use GenServer
  require Logger
  alias Aircloak.ChildSpec
  alias Cloak.DataSource.Driver
  alias Cloak.DataSource.ConnectionPool.ConnectionOwner


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Acquires a connection, invokes the provided lambda, and returns its result."
  @spec execute!(Cloak.DataSource.t, ((Driver.connection) -> result)) :: result when result: var
  def execute!(data_source, fun) do
    if data_source.driver.supports_connection_sharing?() do
      data_source
      |> pool_server()
      |> GenServer.call(:checkout)
      |> on_connection(fun)
    else
      # needed for drivers which don't support connection sharing, such as ODBC
      connection = data_source.driver.connect!(data_source.parameters)
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
  def init({driver, connection_params}) do
    Process.flag(:trap_exit, true)
    {:ok, %{driver: driver, connection_params: connection_params, connections: []}}
  end

  @impl GenServer
  def handle_call(:checkout, _from, state) do
    case state.connections do
      [connection | rest] ->
        {:reply, connection, %{state | connections: rest}}

      [] ->
        {:ok, connection} = GenServer.start_link(ConnectionOwner, {self(), state.driver, state.connection_params})
        {:reply, connection, state}
    end
  end
  def handle_call({:checkin, connection}, _from, state) do
    {:reply, :ok, update_in(state.connections, &[connection | &1])}
  end

  @impl GenServer
  def handle_cast({:remove_connection, connection}, state) do
    case Enum.split_with(state.connections, &(&1 == connection)) do
      {[^connection], remaining_connections} ->
        GenServer.stop(connection, :shutdown)
        {:noreply, %{state | connections: remaining_connections}}

      {[], _} ->
        # In this case, the connection has been checked out again, so we won't stop it.
        {:noreply, state}
    end
  end

  @impl GenServer
  def handle_info({:EXIT, connection, _}, state) do
    {:noreply, %{state | connections: Enum.reject(state.connections, &(&1 == connection))}}
  end
  def handle_info(other, state), do: super(other, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pool_server(data_source) do
    case Registry.lookup(__MODULE__.Registry, {data_source.driver, data_source.parameters}) do
      [{pid, _}] -> pid
      [] ->
        case DynamicSupervisor.start_child(
          __MODULE__.Supervisor,
          %{id: __MODULE__.Server, start: {__MODULE__, :start_server, [data_source.driver, data_source.parameters]}}
        ) do
          {:ok, pid} -> pid
          {:error, {:already_started, pid}} -> pid
        end
    end
  end

  defp on_connection(connection_owner, fun) do
    res =
      connection_owner
      |> GenServer.call(:start_client_usage, :timer.minutes(1) + Driver.connect_timeout())
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

  defp raise_client_error(:exit, {{%Cloak.Query.ExecutionError{} = error, _}, _}, _stacktrace), do: raise(error)
  defp raise_client_error(:error, %{__exception__: true} = error, stacktrace), do: reraise(error, stacktrace)
  defp raise_client_error(:error, error, stacktrace)
    when is_atom(error) or is_binary(error), do: reraise(error, stacktrace)
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
        ChildSpec.dynamic_supervisor(name: __MODULE__.Supervisor),
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  @doc false
  def start_server(driver, connection_params) do
    GenServer.start_link(
      __MODULE__,
      {driver, connection_params},
      name: {:via, Registry, {__MODULE__.Registry, {driver, connection_params}}}
    )
  end
end
