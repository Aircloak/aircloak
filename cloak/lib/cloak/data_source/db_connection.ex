defmodule Cloak.DataSource.DbConnection do
  @moduledoc """
  Manager of database connection used when executing database queries.

  This module implements a workaround to a known issue we've seen with postgrex connections, where the process hangs
  when trying to stop a database connection, even if the stop timeout is provided. This led to a query being in a
  hanged state, despite the fact that the query has been completely processed.

  In this module we perform special handling of all connections which are represented by pids, which in practice is
  all connections except ODBC. For such connections, we spawn a separate "terminator" process which will be responsible
  for terminating connections. This means that the query doesn't have to wait for the connection to be terminated, and
  can return its result immediately. The connection monitor process terminates the connection by brutally killing the
  connection process, which should ensure that we terminate connection without hanging.
  """
  use GenServer
  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Opens a database connection, invokes the provided lambda, returns its result, and closes the connection."
  @spec with_connection(Cloak.DataSource.t, ((Cloak.DataSource.Driver.connection) -> result)) :: result when result: var
  def with_connection(data_source, fun) do
    Logger.debug("Connecting to `#{data_source.name}` ...")
    connection = data_source.driver.connect!(data_source.parameters)
    connection_monitor = start_connection_monitor(connection)
    try do
      fun.(connection)
    after
      close_connection(connection_monitor, data_source.driver, connection)
    end
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init({query_id, connection}) do
    Process.flag(:trap_exit, true)
    if query_id != nil, do: Logger.metadata(query_id: query_id)
    {:ok, %{query_id: query_id, connection: connection}}
  end

  @impl GenServer
  # closing here handles abnormal crashes of the owner (query runner) process.
  def terminate(_reason, state), do: {:ok, shutdown_connection(state)}

  @impl GenServer
  def handle_cast(:close_connection, state), do: {:noreply, shutdown_connection(state)}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_connection_monitor(connection) when is_pid(connection) do
    {:ok, connection_monitor} = GenServer.start_link(__MODULE__, {Logger.metadata()[:query_id], connection})
    connection_monitor
  end
  defp start_connection_monitor(_connection) do
    # Connection is not a process, so we can't use a separate terminator process, since the connection is likely owned
    # by the process which created it, and we can't terminate it from another process. An example of such connection is
    # ODBC.
    nil
  end

  defp close_connection(nil, driver, connection), do: driver.disconnect(connection)
  defp close_connection(connection_monitor, _driver, connection) do
    # We unlink the process to avoid propagating exit signals to the query runner process.
    Process.unlink(connection)
    GenServer.cast(connection_monitor, :close_connection)
  end

  defp shutdown_connection(%{connection: nil} = state), do: state
  defp shutdown_connection(state) do
    Logger.debug("Closing database connection ...")

    mref = Process.monitor(state.connection)
    Process.exit(state.connection, :kill)

    receive do
      {:DOWN, ^mref, :process, _pid, _reason} -> Logger.debug("Database connection closed")
    after :timer.seconds(5) ->
      Logger.warn("Database connection didn't terminate in 5 seconds")
    end

    %{state | connection: nil}
  end
end
