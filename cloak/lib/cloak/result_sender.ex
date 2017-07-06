defmodule Cloak.ResultSender do
  @moduledoc "Handles returning the result of a query back to the requester"

  require Logger

  @type target :: {:process, pid()} | :air_socket
  @type query_state :: :parsing | :compiling | :awaiting_data | :ingesting_data | :processing | :post_processing

  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  @doc false
  def supervisor_spec do
    import Supervisor.Spec
    supervisor(Supervisor, [
      [worker(__MODULE__, [], restart: :temporary)],
      [name: __MODULE__, strategy: :simple_one_for_one, max_restarts: 10, max_seconds: 10]
    ], [id: __MODULE__])
  end

  @doc false
  def start_link(target, type, payload) do
    Task.start_link(fn -> send_result(target, type, payload) end)
  end


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Sends a query state update to the target. Uses a normal process send if target is `{:process, pid}`.
  Uses the Air <-> Cloak socket if it's :air_socket.
  """
  @spec send_state(target(), String.t, query_state()) :: :ok
  def send_state(target, query_id, query_state) do
    {:ok, _} = Supervisor.start_child(__MODULE__, [target, :state, {query_id, query_state}])
    :ok
  end

  @doc """
  Sends the query result to the target. Uses a normal process send if target is `{:process, pid}`.
  Uses the Air <-> Cloak socket if it's :air_socket.
  """
  @spec send_result(target(), term()) :: :ok
  def send_result(target, result) do
    {:ok, _} = Supervisor.start_child(__MODULE__, [target, :result, result])
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp send_result(:air_socket, :result, result) do
    case send_query_result_with_retry(%{retries: 5, retry_delay_sec: 10}, encode_result(result)) do
      :ok -> :ok
      {:error, error} ->
        Logger.error("Error sending query results to the socket: #{inspect error}", query_id: result.query_id)
        {:error, error}
    end
  end
  defp send_result(:air_socket, :state, {query_id, query_state}), do:
    Elixir.Cloak.AirSocket.send_query_state(query_id, query_state)
  defp send_result({:process, pid}, type, result), do:
    send(pid, {type, result})

  defp send_query_result_with_retry(%{retries: 0}, _query_result), do:
    {:error, :timeout}
  defp send_query_result_with_retry(send_state, query_result) do
    case Elixir.Cloak.AirSocket.send_query_result(query_result) do
      :ok -> :ok
      {:error, :timeout} ->
        Logger.warn("timeout sending a query result", query_id: query_result.query_id)
        :timer.sleep(:timer.seconds(send_state.retry_delay_sec))
        send_query_result_with_retry(
          %{send_state |
            retries: send_state.retries - 1,
            retry_delay_sec: :backoff.increment(send_state.retry_delay_sec, _max_delay = 60)
          },
          query_result
        )
      other ->
        other
    end
  end

  defp encode_result(result) do
    new_result =
      result
      |> Map.take([:query_id, :columns, :features, :error, :info])
      |> Map.put(:chunks, encode_chunks(result))

    Map.put(new_result, :row_count, total_row_count(Map.has_key?(result, :rows), new_result))
  end

  defp total_row_count(true, %{chunks: []}), do:
    0
  defp total_row_count(true, result) do
    last_chunk = List.last(result.chunks)
    last_chunk.offset + last_chunk.row_count
  end
  defp total_row_count(false, _), do:
    nil

  defp encode_chunks(%{rows: rows}), do:
    Aircloak.report_long(:encode_chunks,
      fn ->
        rows
        |> Stream.chunk(1000, 1000, [])
        |> Stream.transform(0, &encode_chunk/2)
        |> Enum.to_list()
      end
    )
  defp encode_chunks(_), do:
    []

  defp encode_chunk(rows, offset) do
    row_count = rows |> Stream.map(&(&1.occurrences)) |> Enum.sum()

    {
      [%{offset: offset, row_count: row_count, encoded_data: rows |> :jiffy.encode([:use_nil]) |> :zlib.gzip()}],
      offset + row_count
    }
  end
end
