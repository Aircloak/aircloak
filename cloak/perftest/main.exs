defmodule PerfTest do
  def run(query) do
    Enum.map(1..5, fn (_) -> run_query(query) end)
    |> stats()
  end

  defp stats(timings) do
    avg = Enum.sum(timings) / Enum.count(timings)
    variances = Enum.map(timings, &(&1 - avg) * (&1 - avg))
    stddev = :math.sqrt(Enum.sum(variances) / Enum.count(variances))

    {avg, stddev}
  end

  defp run_query(statement) do
    {:ok, data_source} = Cloak.DataSource.fetch("data_source_name")
    {duration, result} = :timer.tc(fn () ->
      :ok = Cloak.Query.Runner.start("1", data_source, statement, [], %{}, {:process, self()})
      receive do
        {:result, %{rows: rows}} -> rows |> Enum.map(&"#{&1.occurrences} x #{inspect &1.row}") |> Enum.join(", ")
        {:result, %{error: error}} -> raise "Query failed with error: #{error}."
      end
    end)
    duration = duration / 1_000_000
    IO.puts(">>> Query finished with result: #{result} in #{duration} seconds.")
    duration
  end
end

for {key, query} <- %{
  count_notes: "SELECT COUNT(*) FROM notes",
  count_drafts_changes: "SELECT COUNT(*) FROM drafts_changes",
  count_drafts_changes_encoded: "SELECT COUNT(*) FROM drafts_changes_encoded",
} do
  IO.puts(">>> Started performance test ...")
  IO.puts(">>> Testing query '#{query}' ...")

  {avg, stdev} = PerfTest.run(query)

  %{status_code: 204} = HTTPoison.post!(
    "http://localhost:8086/write?db=performance",
    "performance,query=#{key} time_avg=#{avg},time_stddev=#{stdev}"
  )

  IO.puts("\n>>> Performance test ended: AVERAGE duration: #{avg} seconds, STDDEV: #{stdev} seconds.\n\n")
end
