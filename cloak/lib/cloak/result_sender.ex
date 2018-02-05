defmodule Cloak.ResultSender do
  @moduledoc "Functions for reporting query execution state and result to the caller."

  require Logger

  @type target :: {:process, pid()} | :air_socket
  @type query_state :: :parsing | :compiling | :awaiting_data | :ingesting_data | :processing | :post_processing


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Sends a query state update to the target. Uses a normal process send if target is `{:process, pid}`.
  Uses the Air <-> Cloak socket if it's :air_socket.
  """
  @spec send_state(target(), String.t, query_state()) :: :ok | {:error, any}
  def send_state(:air_socket, query_id, query_state), do:
    Elixir.Cloak.AirSocket.send_query_state(query_id, query_state)
  def send_state({:process, pid}, query_id, query_state) do
    send(pid, {:state, {query_id, query_state}})
    :ok
  end

  @doc """
  Sends the query result to the target. Uses a normal process send if target is `{:process, pid}`.
  Uses the Air <-> Cloak socket if it's :air_socket.
  """
  @spec send_result(target(), term()) :: :ok | {:error, :encoding_error} | {:error, any}
  def send_result(:air_socket, result) do
    with {:ok, encoded} <- encode_result(result),
      send_query_result_with_retry(%{retries: 5, retry_delay_sec: 10}, encoded)
    do
      :ok
    else
      {:error, error} ->
        Logger.error("Error sending query results to the socket: #{inspect error}", query_id: result.query_id)
        {:error, error}
    end
  end
  def send_result({:process_encoded, pid}, result) do
    with {:ok, encoded} <- encode_result(result) do
      send(pid, encoded)
    else
      {:error, error} -> send(pid, {:error, error})
    end
    :ok
  end
  def send_result({:process, pid}, result) do
    send(pid, {:result, result})
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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
    {:ok,
      result
      |> Map.take([:query_id, :columns, :features, :error, :cancelled, :info, :execution_time])
      |> Map.put(:chunks, encode_chunks(result))
      |> Map.put(:row_count, row_count(result[:rows]))
    }
  rescue
    _ -> {:error, :encoding_error}
  catch
    _ -> {:error, :encoding_error}
  end

  defp row_count(nil), do:
    nil
  defp row_count(rows), do:
    rows
    |> Stream.map(&(&1.occurrences))
    |> Enum.sum()

  defp encode_chunks(%{rows: rows}), do:
    Aircloak.report_long(:encode_chunks,
      fn ->
        rows
        |> Stream.chunk(1000, 1000, [])
        |> Stream.with_index()
        |> Enum.map(&encode_chunk/1)
      end
    )
  defp encode_chunks(_), do:
    []

  defp encode_chunk({rows, index}), do:
    %{
      index: index,
      encoded_data: rows |> :jiffy.encode([:use_nil, :force_utf8]) |> :zlib.gzip()
    }
end
