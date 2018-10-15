defmodule Cloak.DataSource.Connection do
  @moduledoc "Powers a process which owns the database connection."

  use GenServer
  require Logger
  alias Cloak.DataSource.{Connection.Pool, Driver}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Invoked by the streamer process to put the connection into the streaming state."
  @spec start_streaming(pid) :: {:ok, Cloak.DataSource.Driver.connection()} | {:error, String.t()}
  def start_streaming(connection) do
    {:ok, GenServer.call(connection, :start_streaming)}
  catch
    :exit, {{%Cloak.Query.ExecutionError{} = error, _}, _} ->
      {:error, error.message}
  end

  @doc "Invoked by the streamer process to indicate that the streaming has finished."
  @spec done_streaming(pid) :: :ok
  def done_streaming(connection), do: GenServer.cast(connection, {:done_streaming, self()})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({pool_pid, driver, connection_params}) do
    state = %{pool_pid: pool_pid, driver: driver, connection: nil, streamer_mref: nil, streamer: nil}
    {:ok, state, {:continue, {:connect, connection_params}}}
  end

  @impl GenServer
  def handle_continue({:connect, connection_params}, state),
    do: {:noreply, %{state | connection: state.driver.connect!(connection_params)}, Driver.connection_keep_time()}

  @impl GenServer
  def handle_call(:start_streaming, {streamer, _}, state) do
    nil = state.streamer
    nil = state.streamer_mref

    {:reply, state.connection, %{state | streamer_mref: Process.monitor(streamer), streamer: streamer}}
  end

  @impl GenServer
  def handle_cast({:done_streaming, streamer}, %{streamer: streamer} = state),
    do: {:noreply, checkin(state), Driver.connection_keep_time()}

  @impl GenServer
  def handle_info({:DOWN, streamer_mref, _, _, _reason}, %{streamer_mref: streamer_mref} = state),
    do: {:noreply, checkin(state), Driver.connection_keep_time()}

  def handle_info(:timeout, state) do
    Pool.remove_connection(state.pool_pid)
    {:noreply, state}
  end

  @impl GenServer
  def terminate(_reason, state) do
    if state.connection != nil do
      Logger.debug("Closing database connection")
      state.driver.disconnect(state.connection)
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp checkin(state) do
    Logger.debug("Returning the connection to the pool")
    Process.demonitor(state.streamer_mref, [:flush])
    Pool.checkin(state.pool_pid)
    %{state | streamer_mref: nil, streamer: nil}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(driver, connection_params),
    do: GenServer.start_link(__MODULE__, {self(), driver, connection_params})
end
