defmodule Test.StaticAnalysisReport do
  use GenServer

  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def report(expression, input_bounds, output_bounds, actual_safe?, method, method_safe?, restriction_safe?) do
    GenServer.call(
      __MODULE__,
      {:report, {expression, input_bounds, output_bounds, actual_safe?, method, method_safe?, restriction_safe?}}
    )
  end

  def finalize() do
    GenServer.call(__MODULE__, :finalize)
  end

  @impl GenServer
  def init(_), do: {:ok, []}

  @impl GenServer
  def handle_call({:report, report}, _from, state) do
    {:reply, :ok, [report | state]}
  end

  def handle_call(:finalize, _from, state) do
    print_report(state)
    {:stop, :normal, :ok, state}
  end

  defp print_report([]), do: :ok

  defp print_report(state) do
    IO.puts("\nSTATIC ANALYSIS REPORT\n")
    IO.inspect(state)
  end
end
