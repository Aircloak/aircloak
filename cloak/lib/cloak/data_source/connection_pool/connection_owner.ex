defmodule Cloak.DataSource.ConnectionPool.ConnectionOwner do
  @moduledoc false

  # This module is an internal implementation detail of `Cloak.DataSource.ConnectionPool`, and is therefore not
  # documented. For the same reason, the module doesn't expose interface functions. The knowledge about the message
  # protocol is shared between this module and its only client (connection pool).
  #
  # Note: this module is powered by the `Task` module, although it behaves as a GenServer (i.e. handles messages).
  # Such implementation allows us to work with `DbConnection` drivers, where we need to atomically open up transaction,
  # and stream the results.
  #
  # Note that for synchronous calls, we're still using `GenServer.call` and matching the message in the owner process.
  # This is a bit hacky, but it simplifies the implementation, since we don't need to manually implement the client
  # side of a synchronous request.

  require Logger

  def start_link(driver, connection_params) do
    pool_pid = self()
    Task.start_link(fn -> start_loop({pool_pid, driver, connection_params}) end)
  end

  def start_client_usage(connection_owner) do
    GenServer.call(connection_owner, :start_client_usage, :timer.minutes(1) + Cloak.DataSource.Driver.connect_timeout())
  catch
    # If this call fails, then we failed to connect to the database, so we're raising an informative exception.
    # This prevents reporting "Unknown cloak error" when connecting to the database fails. Note that the real
    # exit reason will still be properly included in the crash log of the connection owner process.
    _type, _error ->
      Cloak.DataSource.raise_error("Failed connecting to the database")
  end

  def checkin(connection_owner), do: GenServer.call(connection_owner, :checkin)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_loop({pool_pid, driver, connection_params}) do
    loop(%{pool_pid: pool_pid, connection: Cloak.DataSource.connect!(driver, connection_params), client_mref: nil})
  end

  defp loop(state) do
    case handle_next_message(state) do
      {:resume, state} -> loop(state)
      {:stop, reason} -> exit(reason)
    end
  end

  defp handle_next_message(state) do
    receive do
      message -> handle_message(message, state)
    after
      timeout(state) ->
        # We're asking the pool to remove this connection to avoid possible race condition, where the pool checks out
        # the owner which is stopping.
        GenServer.cast(state.pool_pid, {:remove_connection, self()})
        {:resume, state}
    end
  end

  defp handle_message({:"$gen_call", {client_pid, _} = from, :start_client_usage}, state) do
    GenServer.reply(from, state.connection)
    {:resume, %{state | client_mref: Process.monitor(client_pid)}}
  end

  defp handle_message({:"$gen_call", from, :checkin}, state) do
    Process.demonitor(state.client_mref, [:flush])
    GenServer.call(state.pool_pid, {:checkin, self()})
    GenServer.reply(from, :ok)
    {:resume, %{state | client_mref: nil}}
  end

  defp handle_message({:DOWN, client_mref, _, _, _}, state) do
    if client_mref == state.client_mref do
      # If we end up here, then the client has crashed without returning the connection. In this case, we need to
      # terminate, since the connection might be in an open transaction state, and we don't want to reuse such
      # connection.
      {:stop, :shutdown}
    else
      # leftover message of a previous client -> just ignore
      {:resume, state}
    end
  end

  defp handle_message(:timeout, state) do
    GenServer.cast(state.pool_pid, {:remove_connection, self()})
    {:resume, state}
  end

  defp handle_message(other, state) do
    Logger.warn("Unknown message #{inspect(other)}")
    {:resume, state}
  end

  defp timeout(state) do
    if is_nil(state.client_mref),
      do: Cloak.DataSource.Driver.connection_keep_time(),
      else: :infinity
  end
end
