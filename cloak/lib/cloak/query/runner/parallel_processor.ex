defmodule Cloak.Query.Runner.ParallelProcessor do
  @moduledoc """
    Helper module for parallel processing of a chunked data stream.

    When no additional processes are needed or when the input consists of a single chunk,
    the data is processed sequentially in the current process.
    Otherwise, multiple workers are created and the input is routed among them.
    After all the data chunks are consumed, the workers' partial states are merged into one
    using the supplied `state_merger` function.
  """

  alias __MODULE__.Worker
  require Logger

  @doc "Helper function for parallel processing of a chunked data stream. See module docs for details."
  @spec execute(Enumerable.t, non_neg_integer, ((Enumerable.t) -> any), ((any, any) -> any)) :: any
  def execute(chunks, 0, processor, _state_merger), do: chunks |> Stream.concat() |> processor.()
  def execute([chunk], _proc_count, processor, _state_merger), do: processor.(chunk)
  def execute(chunks, proc_count, processor, state_merger) when is_integer(proc_count) and proc_count > 0, do:
    proc_count
    |> start_workers(processor)
    |> dispatch_chunks(chunks)
    |> merge_results(state_merger)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_workers(count, processor) do
    parent = self()
    job = fn () ->
      parent |> stream_rows() |> processor.()
    end
    for _i <- 1..count, do: Worker.start_link!(job)
  end

  defp dispatch_chunks(workers, chunks) do
    Logger.debug("Processing data using #{length(workers)} processes ...")
    Enum.each(chunks, &send_more_reply({:data, &1}))
    for _worker <- workers, do: send_more_reply(:end_of_data)
    Logger.debug("Integrating partial results ...")
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

  # For performance reasons, the workers will be grouped 2 by 2 in order to integrate all the results;
  # as each partial state can be large, transfering it from one process to another will be costly.
  # One worker will ask another one for its state, merge it with its own partial state, and so on,
  # until only a single worker remains, which will send the final result back to the parent process.
  # Once a worker reports a result, it will automatically exit. Each worker will report exactly once.
  defp merge_results([worker], _state_merger), do: Worker.report!(worker)
  defp merge_results(workers, state_merger) do
    workers
    |> Enum.chunk_every(2)
    |> Enum.map(fn
      [worker1, worker2] -> Worker.merge(worker1, worker2, state_merger)
      [worker] -> worker
    end)
    |> merge_results(state_merger)
  end


  # -------------------------------------------------------------------
  # Worker implementation
  # -------------------------------------------------------------------

  defmodule Worker do
    @moduledoc false

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

    def start_link!(job) do
      {:ok, worker} = GenServer.start_link(__MODULE__, job)
      worker
    end

    # Reports the result of the job to the caller and stops the worker.
    def report!(worker), do: GenServer.call(worker, :report, :infinity)

    # Merges the job result of the first worker into the job result of the second one. Stops the first worker.
    def merge(from, to, state_merger) do
      :ok = GenServer.cast(to, {:merge, from, state_merger})
      to
    end
  end
end
