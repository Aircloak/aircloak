defmodule Cloak.Query.Runner.ParallelProcessor do
  @moduledoc "Helper module for processing of a chunked data stream."

  alias __MODULE__.Worker
  require Logger

  @doc """
  Helper function for processing of a chunked data stream.

  When no additional processes are needed, the data is processed sequentially in the current process.
  Otherwise, multiple workers are created and the input is routed among them.
  After all the data chunks are consumed, the workers' partial states are merged into one
  using the supplied `state_merger` function.
  """
  @spec execute(Enumerable.t(), non_neg_integer, (Enumerable.t() -> any), (any, any -> any)) :: any
  def execute(chunks, proc_count, processor, state_merger) do
    if proc_count <= 1,
      do: chunks |> Stream.concat() |> processor.(),
      else: proc_count |> start_workers(chunks, processor) |> merge_results(state_merger)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_workers(count, chunks, processor) do
    for _i <- 1..count, do: Worker.start_link!(chunks, processor)
  end

  # For performance reasons, the workers will be grouped 2 by 2 in order to integrate all the results;
  # as each partial state can be large, transfering it from one process to another will be costly.
  # One worker will ask another one for its state, merge it with its own partial state, and so on,
  # until only a single worker remains, which will send the final result back to the parent process.
  # Once a worker reports a result, it will automatically exit. Each worker will report exactly once.
  defp merge_results([worker], _state_merger), do: Worker.report!(worker)

  defp merge_results([worker1, worker2], state_merger) do
    state1 = Worker.report!(worker1)
    state2 = Worker.report!(worker2)
    state_merger.(state1, state2)
  end

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

    def start_link!(chunks, processor) do
      {:ok, worker} = GenServer.start_link(__MODULE__, {chunks, processor})
      worker
    end

    # Reports the result of the job to the caller and stops the worker.
    def report!(worker), do: GenServer.call(worker, :report, :infinity)

    # Merges the job result of the first worker into the job result of the second one. Stops the first worker.
    def merge(from, to, state_merger) do
      :ok = GenServer.cast(to, {:merge, from, state_merger})
      to
    end

    @impl GenServer
    def init({chunks, processor}), do: {:ok, nil, {:continue, {:process_chunks, chunks, processor}}}

    @impl GenServer
    def handle_continue({:process_chunks, chunks, processor}, nil), do: {:noreply, processor.(Stream.concat(chunks))}

    @impl GenServer
    def handle_cast({:merge, from, state_merger}, result), do: {:noreply, state_merger.(result, report!(from))}

    @impl GenServer
    def handle_call(:report, _from, result), do: {:stop, :normal, result, nil}
  end
end
