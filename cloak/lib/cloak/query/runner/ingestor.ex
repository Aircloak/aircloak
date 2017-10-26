defmodule Cloak.Query.Runner.Ingestor do
  @moduledoc """
    Helper module for ingesting data using multiple processes.
    When no additional processes are needed or when the input consists of a single chunk,
    the data is processed sequentially in the current process.
    Otherwise, multiple processes ("ingestors") are created and the input is routed among them.
    After all the data chunks are consumed, the ingestors will be grouped 2 by 2 in order to integrate
    all the results (for performance reasons, as each result can be large and transfering it from one
    process to another will be costly): one ingestor will send its result to the other one, who will
    merge it with its own result, and so on, until only a single ingestor remains, which will send the
    final result back to the parent process.
  """

  require Logger

  @doc "Helper function for ingesting data using multiple processes. See module docs for details."
  @spec ingest(Enumerable.t, non_neg_integer, ((Enumerable.t) -> any), ((any, any) -> any)) :: any
  def ingest(chunks, 0, consumer, _integrator), do: chunks |> Stream.concat() |> consumer.()
  def ingest([chunk], _proc_count, consumer, _integrator), do: consumer.(chunk)
  def ingest(chunks, proc_count, consumer, integrator) when is_integer(proc_count) and proc_count > 0, do:
    proc_count
    |> start_ingestors(consumer, integrator)
    |> dispatch_chunks(chunks)
    |> integrate_results()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_ingestors(count, consumer, integrator) do
    parent = self()
    for _i <- 1..count do
      {:ok, pid} = Task.start_link(fn () ->
        parent
        |> stream_rows()
        |> consumer.()
        |> integrate_result(integrator)
      end)
      pid
    end
  end

  defp dispatch_chunks(pids, chunks) do
    Logger.debug("Ingesting data using #{length(pids)} processes ...")
    Enum.each(chunks, &send_more_reply({:data, &1}))
    for _pid <- pids, do: send_more_reply(:end_of_data)
    Logger.debug("Integrating consumed data ...")
    pids
  end

  defp send_more_reply(answer) do
    receive do
      {:send_more, destination} -> send(destination, answer)
    end
  end

  defp stream_rows(source) do
    Stream.resource(
      fn () -> send(source, {:send_more, self()}) end,
      fn (_) ->
        receive do
          :end_of_data ->
            {:halt, :ok}
          {:data, chunk} ->
            send(source, {:send_more, self()})
            {chunk, :ok}
        end
      end,
      fn (_) -> :ok end
    )
  end

  def transfer_result(from, to) do
    send(from, {:transfer, to, self()})
    receive do :transfer_complete -> :ok end
  end

  defp integrate_result(this_result, integrator) do
    receive do
      {:transfer, pid, caller} ->
        send(pid, {:add, this_result})
        send(caller, :transfer_complete)
      {:add, other_result} ->
        this_result
        |> integrator.(other_result)
        |> integrate_result(integrator)
    end
  end

  defp integrate_results([pid]) do
    transfer_result(pid, self())
    receive do {:add, result} -> result end
  end
  defp integrate_results(pids) do
    pids
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [pid1, pid2] ->
        transfer_result(pid1, pid2)
        pid2
      [pid] -> pid
    end)
    |> integrate_results()
  end
end
