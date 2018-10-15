defmodule Cloak.DataSource.Streamer do
  @moduledoc """
  Implementation of the query streamer process.

  See `rows/2` for details.
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

  - The returned stream can be iterated only once.
  - The stream can be only consumed while the query runner process (the process which invoked this function) is alive.
  - The stream can be consumed concurrently by multiple consumer processes, none of which has to be the query runner
    process.
  - Streaming is performed on a connection which is taken from the connection pool. As soon as the last row has been
    delivered to some consumer, the connection is returned to the connection pool. Closing errors (e.g closing the
    query or transaction) won't affect the outcome of the query runner process.
  - Streaming is performed in a lookahead fashion to ensure that the next row is available in the consumer process when
    it's needed.
  - If the `reporter` function is provided, it will be invoked from the streamer process on every fetched chunk with the
    reporter state, which is the result of the previous reporter invocation. The initial reporter state is `nil`.
  """
  @spec rows(Cloak.Sql.Query.t(), reporter) :: {:ok, Enumerable.t()} | {:error, String.t()}
  def rows(query, reporter \\ nil) do
    Logger.debug("Selecting data ...")

    query_id = Keyword.get(Logger.metadata(), :query_id, nil)
    connection_owner = Connection.Pool.checkout(query.data_source)

    with {:ok, streamer} <- Connection.start_streamer(connection_owner, query_id, query, reporter),
         do: {:ok, fetch_rows(streamer)}
  end

  @doc "Invoked by the connection owner to start the streamer process."
  @spec start_link(Cloak.DataSource.Driver.connection(), pid, String.t(), Cloak.Sql.Query.t(), reporter) ::
          {:ok, pid} | {:error, pid, String.t()}
  def start_link(driver_connection, query_runner, query_id, query, reporter) do
    # Since streaming from the datasource has to be done in a lambda, we can't implement the streamer as a GenServer.
    # Instead, we're starting an OTP compliant process via `:proc_lib`, and receive messages manually while streaming
    # from the database.
    :proc_lib.start_link(__MODULE__, :stream, [self(), driver_connection, query_runner, query_id, query, reporter])
  end

  # -------------------------------------------------------------------
  # Client-side functions
  # -------------------------------------------------------------------

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

      {:DOWN, _, :process, ^streamer, _reason} ->
        # If the streamer exited, regardless of the reason, we're interpreting it as the end of the stream. Normally,
        # this happens when the streamer has emitted the last row, and there are simultaneous consumers.
        # If the streamer crashed, the connection owner process will send an exit signal to the runner process,
        # which will ensure that the query execution is taken down.
        nil
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

  @doc false
  def stream(connection_owner, driver_connection, query_runner, query_id, query, reporter) do
    Logger.metadata(query_id: query_id)

    try do
      query.data_source.driver.select(
        driver_connection,
        query,
        &stream_chunks(&1, connection_owner, query_runner, reporter)
      )

      Logger.debug("Terminating streamer process")
    rescue
      error in [Cloak.Query.ExecutionError] -> :proc_lib.init_ack(connection_owner, {:error, self(), error.message})
    end
  end

  defp stream_chunks(chunks, connection_owner, query_runner, reporter) do
    :proc_lib.init_ack(connection_owner, {:ok, self()})
    process_chunks(chunks, query_runner, reporter)

    # We're issuing this notification here, before we're closing the resources (e.g. query or transaction). This ensures
    # that the query runner won't be taken down if closing of resources fails.
    Connection.streaming_done(connection_owner)
  end

  defp process_chunks(chunks, query_runner, reporter) do
    query_runner_mref = Process.monitor(query_runner)

    Enum.reduce_while(
      Stream.reject(chunks, &Enum.empty?/1),
      nil,
      fn chunk, reporter_state ->
        receive do
          {:next_chunk, request_id, requester_pid} ->
            send(requester_pid, {request_id, chunk})
            {:cont, invoke_reporter(reporter, reporter_state)}

          {:DOWN, ^query_runner_mref, :process, ^query_runner, _reason} ->
            {:halt, reporter_state}

          message ->
            raise("invalid message #{inspect(message)} while streaming the query")
        end
      end
    )
  end

  defp invoke_reporter(nil, _reporter_state), do: nil
  defp invoke_reporter(reporter, reporter_state), do: reporter.(reporter_state)
end
