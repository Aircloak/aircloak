defmodule Cloak.DataSource.Connection do
  @moduledoc """
  Powers a process which owns the database connection.

  The connection process is responsible for the following:

    - streaming of the data from the database
    - checking in to the connection pool (managed by `Cloak.DataSource.ConnectionPool`) when the connection is no longer
      needed by the client
    - closing the connection if it's been idle for a while

  See chunks/1 for more details.
  """

  require Logger
  alias Cloak.DataSource.Connection.Pool

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns a stream of chunks for the given query.

  This function will take a connection from the connection pool (or create a new one if needed), and stream the data
  from the connection process to the client process. Once the streaming is done, the connection will return itself back
  to the pool. If the connection process has been idle (not streaming to any client), it will terminate.

  The streaming is done in a lookahead fashion. The connection process will try to ensure that the next item is
  available in the client process when the client needs it. Therefore, the extra process hop for each row should not
  affect the query latency, especially not in the cases where the client processing takes more time.
  """
  @spec chunks(Cloak.Sql.Query.t()) :: {:ok, Enumerable.t()} | {:error, String.t()}
  def chunks(query) do
    query_id = Keyword.get(Logger.metadata(), :query_id, nil)
    connection_owner = Pool.checkout(query.data_source)

    Logger.debug("Selecting data ...")

    with :ok <- start_stream(connection_owner, query, query_id) do
      {:ok,
       Stream.resource(
         fn -> Process.monitor(connection_owner) end,
         fn mref ->
           case next_chunk(connection_owner) do
             nil ->
               {:halt, mref}

             chunk ->
               Logger.debug("Processing chunk")
               {[chunk], mref}
           end
         end,
         fn mref -> Process.demonitor(mref, [:flush]) end
       )}
    end
  end

  # -------------------------------------------------------------------
  # Main process loop
  # -------------------------------------------------------------------

  @doc false
  def start_link(driver, connection_params) do
    # This module is powered by the `Task` module, although it behaves as a GenServer (i.e. handles messages).
    # Such implementation allows us to work with `DbConnection` drivers, where we need to atomically open up
    # transaction, and stream the results.

    pool_pid = self()
    Task.start_link(fn -> start_loop({pool_pid, driver, connection_params}) end)
  end

  defp start_loop({pool_pid, driver, connection_params}) do
    loop(%{
      pool_pid: pool_pid,
      driver: driver,
      connection: Cloak.DataSource.connect!(driver, connection_params)
    })
  end

  defp loop(state) do
    receive do
      {:"$gen_call", from, {:start_stream, query, query_id}} -> stream_query(state, query, query_id, from)
      other -> Logger.warn("Unknown message #{inspect(other)}")
    after
      Cloak.DataSource.Driver.connection_keep_time() ->
        # We're asking the pool to remove this connection to avoid possible race condition, where the pool checks out
        # the owner which is stopping.
        Pool.remove_connection(state.pool_pid)
        state
    end

    loop(state)
  end

  # -------------------------------------------------------------------
  # Client-side streaming
  # -------------------------------------------------------------------

  defp start_stream(connection_owner, query, query_id) do
    # For synchronous calls, we're still using `GenServer.call` and matching the message in the owner process.
    # This is a bit hacky, but it simplifies the implementation, since we don't need to manually implement the client
    # side of a synchronous request.
    GenServer.call(
      connection_owner,
      {:start_stream, query, query_id},
      :timer.minutes(1) + Cloak.DataSource.Driver.connection_keep_time()
    )
  catch
    type, error ->
      stacktrace = System.stacktrace()
      raise_client_error(type, error, stacktrace)
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

  defp next_chunk(connection_owner) do
    receive do
      {:chunk, chunk} ->
        # immediately asking for the next chunk, to increase the chance of it being available when we need it
        send(connection_owner, {:next_chunk, self()})
        chunk

      :end_of_chunks ->
        nil

      {:DOWN, _, :process, ^connection_owner, _} ->
        raise("connection owner terminated unexpectedly")
    end
  end

  # -------------------------------------------------------------------
  # Connection-side streaming
  # -------------------------------------------------------------------

  defp stream_query(state, query, query_id, from) do
    with {:error, reason} <- state.driver.select(state.connection, query, &start_query_loop(&1, query_id, from)),
         do: GenServer.reply(from, {:error, reason})

    # at this point, we've streamed all the chunks to the client, so we're returning the connection
    Logger.debug("Returning the connection to the pool", query_id: query_id)
    Pool.checkin(state.pool_pid)
  end

  defp start_query_loop(stream, query_id, from) do
    {client_pid, _} = from
    GenServer.reply(from, :ok)
    client_mref = Process.monitor(client_pid)

    try do
      process_chunks(stream, query_id, client_pid)
      send(client_pid, :end_of_chunks)
    after
      Process.demonitor(client_mref, [:flush])
    end
  end

  defp process_chunks(stream, query_id, client_pid) do
    Enum.reduce_while(
      stream,
      nil,
      fn chunk, nil ->
        Logger.debug("Sending next chunk", query_id: query_id)
        send(client_pid, {:chunk, chunk})

        receive do
          {:next_chunk, ^client_pid} -> {:cont, nil}
          {:DOWN, _mref, :process, ^client_pid, _} -> {:halt, nil}
          other -> raise("invalid message #{inspect(other)} while streaming the query")
        end
      end
    )
  end
end
