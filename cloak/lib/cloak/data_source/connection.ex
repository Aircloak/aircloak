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
  @spec execute(DataSource.t(), (Driver.connection() -> result), Pool.checkout_options()) ::
          {:ok, result} | {:error, String.t()}
        when result: var
  def execute(data_source, fun, checkout_opts \\ []) do
    connection = Pool.checkout(data_source, checkout_opts)

    with {:ok, driver_connection} <- start_using(connection) do
      try do
        {:ok, fun.(driver_connection)}
      after
        GenServer.cast(connection, :stop_using)
      end
    end
  end

  @doc "Same as `execute/2`, except it raises `Cloak.Query.ExecutionError` on connection error."
  @spec execute!(DataSource.t(), (Driver.connection() -> result), Pool.checkout_options()) :: result when result: var
  def execute!(data_source, fun, checkout_opts \\ []) do
    case execute(data_source, fun, checkout_opts) do
      {:ok, result} -> result
      {:error, reason} -> raise Cloak.Query.ExecutionError, message: reason
    end
  end

  @doc "Starts the streamer process as the child of the given connection."
  @spec start_streamer(pid, String.t(), Cloak.Sql.Query.t()) :: {:ok, pid} | {:error, String.t()}
  def start_streamer(connection, query_id, query) do
    GenServer.call(connection, {:start_streamer, query_id, query}, Driver.connect_timeout())
  catch
    :exit, {:timeout, _} ->
      Process.exit(connection, :kill)
      {:error, "Timeout connecting to the database."}
  end

  @doc "Invoked by the streamer process when it has sent all the rows to its consumers."
  @spec streaming_done(pid) :: :ok
  def streaming_done(connection), do: GenServer.cast(connection, {:streaming_done, self()})

  @doc """
  Invoked by the pool process to set the unique checkout id.

  Checkout id is needed to resolve a subtle race condition where the connection owner process decides to stop the idle
  connection while the pool process checks it out. In this case, the connection owner will request the pool to stop the
  connection, but the checkout id won't match, and the request will be ignored.
  """
  @spec set_checkout_id(pid, reference) :: :ok
  def set_checkout_id(connection, checkout_id), do: GenServer.cast(connection, {:set_checkout_id, checkout_id})

  @doc "Synchronously stops the connection process."
  @spec stop(pid) :: :ok
  def stop(connection) do
    mref = Process.monitor(connection)
    GenServer.cast(connection, :stop)

    receive do
      {:DOWN, ^mref, _, _, _} -> :ok
    after
      :timer.seconds(5) ->
        Process.exit(connection, :kill)

        receive do
          {:DOWN, ^mref, _, _, _} -> :ok
        end
    end
  end

  @doc "Removes all connections associated with the specified data source from the pool."
  @spec cleanup(DataSource.t()) :: :ok
  defdelegate cleanup(data_source), to: Pool

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({pool_pid, driver, connection_params, checkout_id}) do
    Process.flag(:trap_exit, true)

    state = %{
      pool_pid: pool_pid,
      driver: driver,
      connection: nil,
      query_runner: nil,
      query_mref: nil,
      streamer: nil,
      user_mref: nil,
      connection_params: connection_params,
      checkout_id: checkout_id
    }

    {:ok, state, Driver.connection_keep_time()}
  end

  @impl GenServer
  def handle_call({:start_streamer, query_id, query}, {query_runner, _}, state) do
    assert_not_used!(state)

    case ensure_connected(state) do
      {:ok, state} ->
        Logger.metadata(query_id: query_id)
        {:ok, streamer} = Streamer.start_link(state.connection, query_runner, query_id, query)
        query_mref = Process.monitor(query_runner)
        {:reply, {:ok, streamer}, %{state | query_runner: query_runner, streamer: streamer, query_mref: query_mref}}

      {:error, reason} ->
        {:stop, :shutdown, {:error, reason}, state}
    end
  end

  def handle_call(:start_using, {user_pid, _}, state) do
    assert_not_used!(state)

    case ensure_connected(state) do
      {:ok, state} -> {:reply, {:ok, state.connection}, %{state | user_mref: Process.monitor(user_pid)}}
      {:error, reason} -> {:stop, :shutdown, {:error, reason}, state}
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

  def handle_cast(:stop_using, state), do: checkin(state)

  def handle_cast({:set_checkout_id, checkout_id}, state) do
    assert_not_used!(state)
    {:noreply, %{state | checkout_id: checkout_id}, Driver.connection_keep_time()}
  end

  def handle_cast(:stop, state), do: {:stop, :normal, state}

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

  def handle_info({:EXIT, connection, _reason}, %{connection: connection} = state),
    do: {:stop, :normal, %{state | connection: nil}}

  def handle_info({:DOWN, user_mref, _, _, _}, %{user_mref: user_mref} = state), do: checkin(state)
  def handle_info({:DOWN, query_mref, _, _, _}, %{query_mref: query_mref} = state), do: {:stop, :normal, state}

  def handle_info(:timeout, state) do
    Pool.remove_connection(state.pool_pid, state.checkout_id)
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

  defp start_using(connection) do
    GenServer.call(connection, :start_using, Driver.connect_timeout())
  catch
    :exit, {:timeout, _} ->
      Process.exit(connection, :kill)
      {:error, "Timeout connecting to the database."}
  end

  defp ensure_connected(state) do
    if is_nil(state.connection) do
      with {:ok, connection} <- do_connect(state), do: {:ok, %{state | connection: connection}}
    else
      {:ok, state}
    end
  end

  defp do_connect(state) do
    with {:error, reason} <- state.driver.connect(state.connection_params), do: {:error, connection_error(reason)}
  catch
    type, error ->
      log_unknown_error(type, error, __STACKTRACE__)
      {:error, generic_connection_error()}
  end

  defp connection_error(reason),
    do: generic_connection_error() <> " The database driver reported the following exception: `#{to_string(reason)}`"

  defp generic_connection_error() do
    "Failed to establish a connection to the database. " <>
      "Please check that the database server is running, is reachable from the Insights Cloak host, " <>
      "and the database credentials are correct."
  end

  def log_unknown_error(:exit, reason, _client_stacktrace), do: Logger.error(Cloak.LoggerTranslator.format_exit(reason))

  def log_unknown_error(type, reason, stacktrace) do
    formatted_error =
      if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors) do
        Exception.format(type, "filtered error", Cloak.LoggerTranslator.filtered_stacktrace(stacktrace))
      else
        Exception.format(type, reason, stacktrace)
      end

    Logger.error(formatted_error)
  end

  defp assert_not_used!(state) do
    true = is_nil(state.streamer)
    true = is_nil(state.user_mref)
    true = is_nil(state.query_mref)
    true = is_nil(state.query_runner)
  end

  defp checkin(state) do
    Logger.debug("Returning the connection to the pool")
    Logger.metadata(query_id: nil)
    Pool.checkin(state.pool_pid)
    if is_reference(state.query_mref), do: Process.demonitor(state.query_mref, [:flush])
    if is_reference(state.user_mref), do: Process.demonitor(state.user_mref, [:flush])
    new_state = %{state | streamer: nil, query_runner: nil, query_mref: nil, user_mref: nil}
    {:noreply, new_state, Driver.connection_keep_time()}
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(driver, connection_params, checkout_id),
    do: GenServer.start_link(__MODULE__, {self(), driver, connection_params, checkout_id})
end
