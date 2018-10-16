defmodule Cloak.DataSource.Connection do
  @moduledoc "Powers a process which owns the database connection."

  use GenServer
  require Logger
  alias Cloak.DataSource
  alias Cloak.DataSource.{Connection.Pool, Driver, Streamer}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Invokes the given lambda on a pooled connection.

  This function will checkout the connection from the pool, and then invoke the lambda, passing it the underlying
  driver connection. After the lambda is done (or if it crashes), the connection is returned to the pool.
  """
  @spec execute!(DataSource.t(), (Driver.connection() -> result), Pool.checkout_options()) :: result when result: var
  def execute!(data_source, fun, checkout_opts \\ []) do
    connection = Pool.checkout(data_source, checkout_opts)

    with {:ok, driver_connection} <- GenServer.call(connection, :start_using) do
      try do
        {:ok, fun.(driver_connection)}
      after
        GenServer.cast(connection, :stop_using)
      end
    end
    |> case do
      {:ok, result} -> result
      {:error, reason} -> raise Cloak.Query.ExecutionError, message: reason
    end
  end

  @doc "Starts the streamer process as the child of the given connection."
  @spec start_streamer(pid, String.t(), Cloak.Sql.Query.t(), Streamer.reporter()) :: {:ok, pid} | {:error, String.t()}
  def start_streamer(connection, query_id, query, reporter),
    do: GenServer.call(connection, {:start_streamer, query_id, query, reporter})

  @doc "Invoked by the streamer process when it has sent all the rows to its consumers."
  @spec streaming_done(pid) :: :ok
  def streaming_done(connection), do: GenServer.cast(connection, {:streaming_done, self()})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({pool_pid, driver, connection_params, connect_options}) do
    Process.flag(:trap_exit, true)

    state = %{
      pool_pid: pool_pid,
      driver: driver,
      connection: nil,
      query_runner: nil,
      streamer: nil,
      user_mref: nil,
      connection_error: nil
    }

    {:ok, state, {:continue, {:connect, connection_params, connect_options}}}
  end

  @impl GenServer
  def handle_continue({:connect, connection_params, connect_options}, state) do
    {
      :noreply,
      connect_with_retry(state, connection_params, Keyword.get(connect_options, :retries, 0)),
      Driver.connection_keep_time()
    }
  end

  @impl GenServer
  def handle_call({:start_streamer, query_id, query, reporter}, {query_runner, _} = from, state) do
    if is_nil(state.connection_error) do
      assert_not_used!(state)
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
    else
      {:stop, :shutdown, {:error, state.connection_error}, state}
    end
  end

  def handle_call(:start_using, {user_pid, _}, state) do
    if is_nil(state.connection_error) do
      assert_not_used!(state)
      {:reply, {:ok, state.connection}, %{state | user_mref: Process.monitor(user_pid)}}
    else
      {:stop, :shutdown, {:error, state.connection_error}, state}
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
          checkin(state)
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

  def handle_cast(:stop_using, state) do
    true = is_reference(state.user_mref)
    Process.demonitor(state.user_mref, [:flush])
    checkin(state)
  end

  @impl GenServer
  def handle_info({:EXIT, streamer, reason}, %{streamer: streamer} = state) do
    if reason == :normal do
      # Streamer terminated normally, so we can return the connection to the pool.
      checkin(state)
    else
      # Streamer has crashed, so we'll send the exit signal to the query runner and stop the connection.
      if is_pid(state.query_runner), do: Process.exit(state.query_runner, reason)
      {:stop, reason, %{state | query_runner: nil, streamer: nil}}
    end
  end

  def handle_info({:EXIT, connection, reason}, %{connection: connection} = state), do: {:stop, reason, state}

  def handle_info({:DOWN, user_mref, _, _, _}, %{user_mref: user_mref} = state),
    do: checkin(state)

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

  defp connect_with_retry(state, connection_params, retries) do
    case connect(state, connection_params) do
      {:ok, connection} ->
        %{state | connection: connection}

      {:error, reason} ->
        if retries > 0 do
          Process.sleep(:timer.seconds(1))
          connect_with_retry(state, connection_params, retries - 1)
        else
          %{state | connection_error: reason}
        end
    end
  end

  defp connect(state, connection_params) do
    {:ok, state.driver.connect!(connection_params)}
  rescue
    error in Cloak.Query.ExecutionError -> {:error, error.message}
  catch
    type, error ->
      formatted_reason = Cloak.LoggerTranslator.format_exit({type, error})

      formatted_stacktrace =
        __STACKTRACE__ |> Cloak.LoggerTranslator.filtered_stacktrace() |> Exception.format_stacktrace()

      Logger.error("Error connecting to the database: #{formatted_reason} at \n#{formatted_stacktrace}")

      generic_error =
        "Failed to establish a connection to the database. " <>
          "Please check that the database server is running, is reachable from the " <>
          "Insights Cloak host, and the database credentials are correct."

      {:error, generic_error}
  end

  defp assert_not_used!(state) do
    true = is_nil(state.streamer)
    true = is_nil(state.user_mref)
  end

  defp checkin(state) do
    Logger.debug("Returning the connection to the pool")
    Logger.metadata(query_id: nil)
    Pool.checkin(state.pool_pid)
    new_state = %{state | streamer: nil, query_runner: nil, user_mref: nil}
    {:noreply, new_state, Driver.connection_keep_time()}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(driver, connection_params, connect_options),
    do: GenServer.start_link(__MODULE__, {self(), driver, connection_params, connect_options})
end
