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
  def ingest(chunks, 0, consumer, _state_merger), do: chunks |> Stream.concat() |> consumer.()
  def ingest([chunk], _proc_count, consumer, _state_merger), do: consumer.(chunk)
  def ingest(chunks, proc_count, consumer, state_merger) when is_integer(proc_count) and proc_count > 0, do:
    proc_count
    |> start_workers(consumer)
    |> dispatch_chunks(chunks)
    |> integrate_results(state_merger)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defmodule Worker do
    @moduledoc "Internal module for managing workers during parallel processing."

    use GenServer
    @behaviour GenServer

    @impl GenServer
    def init(job) do
      GenServer.cast(self(), {:execute, job})
      {:ok, nil}
    end

    @impl GenServer
    def handle_cast({:execute, job}, nil), do: {:noreply, job.()}
    def handle_cast({:merge, from, state_merger}, result), do:
      {:noreply, state_merger.(result, report!(from))}

    @impl GenServer
    def handle_call(:report, _from, result), do: {:stop, :normal, result, nil}

    @doc "Reports the result of the job to the caller and stops the worker."
    @spec report!(pid) :: any
    def report!(worker), do: GenServer.call(worker, :report, :infinity)

    @doc "Merges the job result of the first worker into the job result of the second one. Stops the first worker."
    @spec merge(pid, pid, ((any, any) -> any)) :: pid
    def merge(from, to, state_merger) do
      :ok = GenServer.cast(to, {:merge, from, state_merger})
      to
    end
  end

  defp start_workers(count, consumer) do
    parent = self()
    consume_job = fn () ->
      parent |> stream_rows() |> consumer.()
    end
    for _i <- 1..count do
      {:ok, worker} = GenServer.start_link(Worker, consume_job)
      worker
    end
  end

  defp dispatch_chunks(workers, chunks) do
    Logger.debug("Ingesting data using #{length(workers)} processes ...")
    Enum.each(chunks, &send_more_reply({:data, &1}))
    for _worker <- workers, do: send_more_reply(:end_of_data)
    Logger.debug("Integrating consumed data ...")
    workers
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

  defp integrate_results([worker], _state_merger), do: Worker.report!(worker)
  defp integrate_results(workers, state_merger) do
    workers
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [worker1, worker2] -> Worker.merge(worker1, worker2, state_merger)
      [worker] -> worker
    end)
    |> integrate_results(state_merger)
  end
end
