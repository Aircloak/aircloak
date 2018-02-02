defmodule Cloak.DataSource.ConnectionPool.ConnectionOwner do
  @moduledoc false

  # This module is an internal implementation detail of `Cloak.DataSource.ConnectionPool`, and is therefore not
  # documented. For the same reason, the module doesn't expose interface functions. The knowledge about the message
  # protocol is shared between this module and its only client (connection pool).

  use GenServer
  require Logger


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({pool_pid, driver, connection_params}) do
    # Delayed connecting to avoid blocking the pool process.
    send(self(), {:connect, driver, connection_params})
    state = %{pool_pid: pool_pid, connection: nil, client_mref: nil}
    {:ok, state, timeout(state)}
  end

  @impl GenServer
  def handle_call(:start_client_usage, {client_pid, _}, state) do
    {:reply, state.connection, %{state | client_mref: Process.monitor(client_pid)}}
  end
  def handle_call(:checkin, _from, state) do
    Process.demonitor(state.client_mref, [:flush])
    GenServer.call(state.pool_pid, {:checkin, self()})
    state = %{state | client_mref: nil}
    {:reply, :ok, state, timeout(state)}
  end

  @impl GenServer
  def handle_info({:connect, driver, connection_params}, state) do
    state = %{state | connection: driver.connect!(connection_params)}
    {:noreply, state, timeout(state)}
  end
  def handle_info({:DOWN, client_mref, _, _, _}, state) do
    if client_mref == state.client_mref do
      # If we end up here, then the client has crashed without returning the connection. In this case, we need to
      # terminate, since the connection might be in an open transaction state, and we don't want to reuse such
      # connection.
      {:stop, :shutdown, state}
    else
      # leftover message of a previous client -> just ignore
      {:noreply, state, timeout(state)}
    end
  end
  def handle_info(:timeout, state) do
    # We're asking the pool to remove this connection to avoid possible race condition, where the pool checks out
    # the owner which is stopping.
    GenServer.cast(state.pool_pid, {:remove_connection, self()})
    {:noreply, state}
  end
  def handle_info(other, state) do
    Logger.warn("Unknown message #{inspect(other)}")
    {:noreply, state, timeout(state)}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp timeout(state) do
    if is_nil(state.client_mref), do: Cloak.DataSource.Driver.connection_keep_time(), else: :infinity
  end
end
