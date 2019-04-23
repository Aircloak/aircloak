defmodule Test.StaticAnalysisReport do
  use GenServer

  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  def report(expression, input_bounds, output_bounds, actual_safe?, method, method_safe?, restriction_safe?) do
    GenServer.call(
      __MODULE__,
      {:report,
       %{
         expression: expression,
         input_bounds: input_bounds,
         output_bounds: output_bounds,
         actual_safe?: actual_safe?,
         method: method,
         method_safe?: method_safe?,
         restriction_safe?: restriction_safe?
       }}
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

    state
    |> Enum.group_by(&Map.take(&1, [:expression, :input_bounds, :output_bounds, :actual_safe?, :restriction_safe?]))
    |> Enum.sort()
    |> Enum.each(fn {%{
                       expression: expression,
                       input_bounds: input_bounds,
                       output_bounds: output_bounds,
                       actual_safe?: actual_safe?,
                       restriction_safe?: restriction_safe?
                     }, runs} ->
      IO.puts("\n#{expression} on #{inspect(input_bounds)} restricted to #{inspect(output_bounds)}\n")
      IO.puts("Safe?: #{actual_safe?}")
      IO.puts("Rejected by restritions?: #{!restriction_safe?}")
      IO.puts("Rejected by static analysis methods: #{count(runs, &(!&1.method_safe?))}")
    end)
  end

  defp count(all, fun) do
    total = length(all)
    positive = Enum.count(all, fun)

    "#{positive}/#{total} (#{round(100 * positive / total)}%)"
  end
end
