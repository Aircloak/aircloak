defmodule Cloak.DataSource.Streamer do
  @moduledoc """
  Query streamer process.

  This process executes the given query and returns the stream of rows The streaming is done in a lookahead fashion.
  The streamer process will try to ensure that the next item is available in the client process when the client needs
  it. Therefore, the extra process hop for each row should not affect the query duration, especially not in the cases
  where the client processing takes more time.
  """

  require Logger
  alias Cloak.DataSource.Connection

  @type reporter :: (reporter_state -> reporter_state) | nil
  @type reporter_state :: any

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns a stream of rows for the given query.

  This function will take a connection from the connection pool (or create a new one if needed), and stream the data
  to the client processes. Once the streaming is done, the connection will be returned to the pool.

  The returned stream can be safely used by multiple concurrent processes.

  If the `reporter` function is provided, it will be invoked from the streamer process on every fetched chunk.
  The initial reporter state is `nil`.
  """
  @spec rows(Cloak.Sql.Query.t(), reporter) :: {:ok, Enumerable.t()} | {:error, String.t()}
  def rows(query, reporter \\ nil) do
    Logger.debug("Selecting data ...")

    query_id = Keyword.get(Logger.metadata(), :query_id, nil)
    connection_owner = Connection.Pool.checkout(query.data_source)

    with {:ok, streamer} <- start_streamer(connection_owner, query_id, query, reporter) do
      start_cleanup_process(streamer)
      {:ok, fetch_rows(streamer)}
    end
  end

  # -------------------------------------------------------------------
  # Client-side functions
  # -------------------------------------------------------------------

  defp start_streamer(connection_owner, query_id, query, reporter),
    do: :proc_lib.start_link(__MODULE__, :stream, [connection_owner, query_id, query, self(), reporter])

  defp fetch_rows(streamer) do
    Stream.resource(
      fn -> {Process.monitor(streamer), request_next_chunk(streamer)} end,
      fn {mref, request_id} ->
        case next_chunk(request_id, streamer) do
          {chunk, next_request_id} -> {chunk, {mref, next_request_id}}
          nil -> {:halt, {mref, nil}}
        end
      end,
      fn {mref, _request_id} -> Process.demonitor(mref, [:flush]) end
    )
  end

  defp next_chunk(request_id, streamer) do
    receive do
      {^request_id, :end_of_chunks} ->
        nil

      {^request_id, chunk} ->
        # immediately asking for the next chunk, to increase the chance of it being available when we need it
        {chunk, request_next_chunk(streamer)}

      {:DOWN, _, :process, ^streamer, reason} ->
        # - If a process exited normally, then all the rows have been read, so we just return nil.
        # - If a process doesn't exist, it means it has streamed to another client before we started monitoring it, so
        #   we can also return nil.
        # - Any other exit reason is abnormal, so we raise in the client process too.
        if reason in [:normal, :noproc], do: nil, else: raise("connection owner terminated unexpectedly")
    end
  end

  defp request_next_chunk(streamer) do
    request_id = make_ref()
    send(streamer, {:next_chunk, request_id, self()})
    request_id
  end

  # -------------------------------------------------------------------
  # Server-side functions
  # -------------------------------------------------------------------

  def stream(connection_owner, query_id, query, client_pid, reporter) do
    Process.flag(:trap_exit, true)
    Logger.metadata(query_id: query_id)

    case Connection.start_streaming(connection_owner) do
      {:error, _} = error ->
        :proc_lib.init_ack(client_pid, error)

      {:ok, connection} ->
        try do
          query.data_source.driver.select(connection, query, &stream_chunks(&1, client_pid, reporter))
          Logger.debug("Terminating streamer process")
        rescue
          error in [Cloak.Query.ExecutionError] -> :proc_lib.init_ack(client_pid, {:error, error.message})
        catch
          type, exception ->
            formatted_error = Cloak.LoggerTranslator.format_exit({type, exception})

            formatted_stacktrace =
              Exception.format_stacktrace(Cloak.LoggerTranslator.filtered_stacktrace(__STACKTRACE__))

            Logger.error("Error starting the query stream: #{formatted_error} at\n#{formatted_stacktrace}")

            :proc_lib.init_ack(client_pid, {:error, "Unknown cloak error"})
        end
    end
  end

  defp stream_chunks(stream, client_pid, reporter) do
    :proc_lib.init_ack(client_pid, {:ok, self()})
    process_chunks(stream, client_pid, reporter)
  end

  defp process_chunks(stream, client_pid, reporter) do
    Enum.reduce_while(
      Stream.reject(stream, &Enum.empty?/1),
      nil,
      fn chunk, reporter_state ->
        receive do
          {:next_chunk, request_id, requester_pid} ->
            send(requester_pid, {request_id, chunk})
            {:cont, invoke_reporter(reporter, reporter_state)}

          {:EXIT, ^client_pid, _reason} ->
            {:halt, reporter_state}

          message ->
            raise("invalid message #{inspect(message)} while streaming the query")
        end
      end
    )
  end

  defp invoke_reporter(nil, _reporter_state), do: nil
  defp invoke_reporter(reporter, reporter_state), do: reporter.(reporter_state)

  # -------------------------------------------------------------------
  # Cleanup process
  # -------------------------------------------------------------------

  defp start_cleanup_process(streamer) do
    # This is a helper process which makes sure that the streamer process is taken down if the client process dies.
    # Normally, this should happen by itself, but there is a remote possibility of a streamer process getting stuck and
    # refusing to die. In this case, this process will forcefully kill the streamer.

    client_pid = self()

    Task.start_link(fn ->
      Process.flag(:trap_exit, true)
      mref = Process.monitor(streamer)

      receive do
        {:DOWN, ^mref, _, _, _} ->
          :ok

        {:EXIT, ^client_pid, _reason} ->
          receive do
            {:DOWN, ^mref, _, _, _} -> :ok
          after
            :timer.seconds(5) ->
              Process.exit(streamer, :kill)
          end
      end
    end)
  end
end
