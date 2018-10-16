defmodule Cloak.DataSource.Connection do
  @moduledoc "Powers a process which owns the database connection."

  use GenServer
  require Logger
  alias Cloak.DataSource.{Connection.Pool, Driver, Streamer}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Starts the streamer process as the child of the given connection."
  @spec start_streamer(pid, String.t(), Cloak.Sql.Query.t(), Streamer.reporter()) :: {:ok, pid} | {:error, String.t()}
  def start_streamer(connection, query_id, query, reporter) do
    GenServer.call(connection, {:start_streamer, query_id, query, reporter})
  catch
    :exit, {{%Cloak.Query.ExecutionError{} = error, _}, _} ->
      {:error, error.message}
  end

  @doc "Invoked by the streamer process when it has sent all the rows to its consumers."
  @spec streaming_done(pid) :: :ok
  def streaming_done(connection), do: GenServer.cast(connection, {:streaming_done, self()})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({pool_pid, driver, connection_params}) do
    Process.flag(:trap_exit, true)

    state = %{pool_pid: pool_pid, driver: driver, connection: nil, query_runner: nil, streamer: nil}
    {:ok, state, {:continue, {:connect, connection_params}}}
  end

  @impl GenServer
  def handle_continue({:connect, connection_params}, state),
    do: {:noreply, %{state | connection: state.driver.connect!(connection_params)}, Driver.connection_keep_time()}

  @impl GenServer
  def handle_call({:start_streamer, query_id, query, reporter}, {query_runner, _} = from, state) do
    true = is_nil(state.streamer)
    Logger.metadata(query_id: query_id)

    case Streamer.start_link(state.connection, query_runner, query_id, query, reporter) do
      {:ok, streamer} ->
        {:reply, {:ok, streamer}, %{state | query_runner: query_runner, streamer: streamer}}

      {:error, streamer_pid, reason} ->
        GenServer.reply(from, {:error, reason})

        # flush the leftover exit message
        receive do
          {:EXIT, ^streamer_pid, _exit_reason} -> {:noreply, state}
        after
          :timer.seconds(5) ->
            Process.exit(streamer_pid, :kill)
            {:stop, :streamer_timeout, state}
        end
    end
  end

  @impl GenServer
  def handle_cast({:streaming_done, streamer}, state) do
    ^streamer = state.streamer

    # The streamer has reported that it's done. Now we're waiting for it to stop, so we can check in the connection.
    receive do
      {:EXIT, ^streamer, reason} ->
        if reason == :normal do
          # Streamer terminated normally, so we can return the connection to the pool.
          {:noreply, checkin(state), Driver.connection_keep_time()}
        else
          # Streamer has crashed, so we'll stop the connection. Note that we're not killing the query runner, because
          # at this point, all the data has been streamed, so it can resume with its work.
          {:stop, reason, %{state | query_runner: nil, streamer: nil}}
        end
    after
      :timer.seconds(5) ->
        # Streamer didn't stop after reporting streaming_done (maybe it got stuck?), so we're stopping the connection.
        {:stop, :streamer_timeout, %{state | query_runner: nil}}
    end
  end

  @impl GenServer
  def handle_info({:EXIT, streamer, reason}, %{streamer: streamer} = state) do
    if reason == :normal do
      # Streamer terminated normally, so we can return the connection to the pool.
      {:noreply, checkin(state), Driver.connection_keep_time()}
    else
      # Streamer has crashed, so we'll send the exit signal to the query runner and stop the connection.
      if is_pid(state.query_runner), do: Process.exit(state.query_runner, reason)
      {:stop, reason, %{state | query_runner: nil, streamer: nil}}
    end
  end

  def handle_info({:EXIT, connection, reason}, %{connection: connection} = state), do: {:stop, reason, state}

  def handle_info(:timeout, state) do
    Pool.remove_connection(state.pool_pid)
    {:noreply, state}
  end

  @impl GenServer
  def terminate(_reason, state) do
    if is_pid(state.streamer), do: Process.exit(state.streamer, :kill)
    if is_pid(state.query_runner), do: Process.exit(state.query_runner, :db_connection_closed)

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
    Logger.metadata(query_id: nil)
    Pool.checkin(state.pool_pid)
    %{state | streamer: nil, query_runner: nil}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(driver, connection_params),
    do: GenServer.start_link(__MODULE__, {self(), driver, connection_params})
end
