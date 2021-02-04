defmodule Cloak.DataSource.Streamer do
  @moduledoc """
  Implementation of the query streamer process.

  See `rows/2` for details.
  """

  require Logger
  alias Cloak.DataSource.Connection

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
  """
  @spec rows(Cloak.Sql.Query.t()) :: {:ok, Enumerable.t()} | {:error, String.t()}
  def rows(query) do
    Logger.debug("Selecting data ...")

    query_id = Keyword.get(Logger.metadata(), :query_id, nil)
    connection_owner = Connection.Pool.checkout(query.data_source)

    with {:ok, streamer} <- Connection.start_streamer(connection_owner, query_id, query),
         do: {:ok, fetch_rows(streamer)}
  end

  @doc "Invoked by the connection owner to start the streamer process."
  @spec start_link(Cloak.DataSource.Driver.connection(), pid, String.t(), Cloak.Sql.Query.t()) :: {:ok, pid}
  def start_link(driver_connection, query_runner, query_id, query) do
    # Since streaming from the datasource has to be done in a lambda, we can't implement the streamer as a GenServer.
    # Instead, we're starting a task, and receive messages manually while streaming from the database.
    Task.start_link(fn -> stream(self(), driver_connection, query_runner, query_id, query) end)
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

      {^request_id, {:error, reason}} ->
        raise Cloak.Query.ExecutionError, message: reason

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

  defp stream(connection_owner, driver_connection, query_runner, query_id, query) do
    Logger.metadata(query_id: query_id)

    try do
      with {:error, error} <-
             query.data_source.driver.select(
               driver_connection,
               query,
               &stream_chunks(&1, connection_owner, query_runner)
             ),
           do: stream_error(query_runner, sanitize_database_error(error))

      Logger.debug("Terminating streamer process")
    rescue
      exception in [Cloak.Query.ExecutionError] -> stream_error(query_runner, Exception.message(exception))
      exception -> stream_error(query_runner, exception |> Exception.message() |> sanitize_database_error())
    catch
      :exit, {:timeout, _} ->
        stream_error(query_runner, "Database request timed out.")
    end
  end

  defp sanitize_database_error(message) do
    if Aircloak.DeployConfig.override_app_env!(:cloak, :sanitize_otp_errors),
      do: "Error fetching data from the database.",
      else: message
  end

  defp stream_chunks(chunks, connection_owner, query_runner) do
    process_chunks(chunks, query_runner)

    # We're issuing this notification here, before we're closing the resources (e.g. query or transaction). This ensures
    # that the query runner won't be taken down if closing of resources fails.
    Connection.streaming_done(connection_owner)
  end

  defp process_chunks(chunks, query_runner) do
    query_runner_mref = Process.monitor(query_runner)

    Enum.reduce_while(
      Stream.reject(chunks, &Enum.empty?/1),
      nil,
      fn chunk, _ ->
        receive do
          {:next_chunk, request_id, requester_pid} ->
            send(requester_pid, {request_id, chunk})
            {:cont, nil}

          {:DOWN, ^query_runner_mref, :process, ^query_runner, _reason} ->
            {:halt, nil}

          message ->
            raise("invalid message #{inspect(message)} while streaming the query")
        end
      end
    )
  end

  defp stream_error(query_runner, error) do
    query_runner_mref = Process.monitor(query_runner)

    receive do
      {:next_chunk, request_id, requester_pid} ->
        send(requester_pid, {request_id, {:error, error}})

      {:DOWN, ^query_runner_mref, :process, ^query_runner, _reason} ->
        {:halt, nil}

      message ->
        raise("invalid message #{inspect(message)} while streaming the query")
    end
  end
end
