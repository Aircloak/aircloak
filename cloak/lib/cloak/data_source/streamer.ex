defmodule Cloak.DataSource.Streamer do
  @moduledoc """
  Query streamer process.

  This process executes the given query and returns the stream of chunks, with each chunk containing multiple
  rows. The streaming is done in a lookahead fashion. The streamer process will try to ensure that the next item is
  available in the client process when the client needs it. Therefore, the extra process hop for each row should not
  affect the query duration, especially not in the cases where the client processing takes more time.
  """

  require Logger
  alias Cloak.DataSource.Connection

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns a stream of chunks for the given query.

  This function will take a connection from the connection pool (or create a new one if needed), and stream the data
  to the client process. Once the streaming is done, the connection will be returned to the pool.
  """
  @spec chunks(Cloak.Sql.Query.t()) :: {:ok, Enumerable.t()} | {:error, String.t()}
  def chunks(query) do
    connection_owner = Connection.Pool.checkout(query.data_source)
    Logger.debug("Selecting data ...")

    with {:ok, streamer} <- start_streamer(connection_owner, query) do
      start_cleanup_process(streamer)
      {:ok, fetch_chunks(streamer)}
    end
  end

  # -------------------------------------------------------------------
  # Client-side functions
  # -------------------------------------------------------------------

  defp start_streamer(connection_owner, query),
    do: :proc_lib.start_link(__MODULE__, :stream, [connection_owner, query, self()])

  defp fetch_chunks(streamer) do
    Stream.resource(
      fn -> Process.monitor(streamer) end,
      fn mref ->
        case next_chunk(streamer) do
          nil -> {:halt, mref}
          chunk -> {[chunk], mref}
        end
      end,
      fn mref -> Process.demonitor(mref, [:flush]) end
    )
  end

  defp next_chunk(streamer) do
    receive do
      {:chunk, chunk} ->
        # immediately asking for the next chunk, to increase the chance of it being available when we need it
        send(streamer, {:next_chunk, self()})
        chunk

      :end_of_chunks ->
        nil

      {:DOWN, _, :process, ^streamer, _} ->
        raise("connection owner terminated unexpectedly")
    end
  end

  # -------------------------------------------------------------------
  # Server-side functions
  # -------------------------------------------------------------------

  def stream(connection_owner, query, client_pid) do
    Process.flag(:trap_exit, true)

    case Connection.start_streaming(connection_owner) do
      {:error, _} = error ->
        :proc_lib.init_ack(client_pid, error)

      {:ok, connection} ->
        try do
          query.data_source.driver.select(connection, query, &stream_chunks(&1, connection_owner, client_pid))
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

  defp stream_chunks(stream, connection_owner, client_pid) do
    :proc_lib.init_ack(client_pid, {:ok, self()})
    process_chunks(stream, client_pid)
    Connection.done_streaming(connection_owner)
    send(client_pid, :end_of_chunks)
  end

  defp process_chunks(stream, client_pid) do
    Enum.reduce_while(
      Stream.reject(stream, &Enum.empty?/1),
      nil,
      fn chunk, nil ->
        send(client_pid, {:chunk, chunk})

        receive do
          {:next_chunk, ^client_pid} -> {:cont, nil}
          {:EXIT, ^client_pid, _reason} -> {:halt, nil}
          message -> raise("invalid message #{inspect(message)} while streaming the query")
        end
      end
    )
  end

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
