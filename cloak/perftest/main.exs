defmodule PerfTest do
  def run(query) do
    IO.puts(">>> Started performance test ...")
    IO.puts ">>> Testing query '#{query}' ..."
    timings = Enum.map(1..5, fn (_) -> run_query(query) end)
    {avg, stddev} = stats(timings)
    IO.puts "\n>>> Performance test ended: AVERAGE duration: #{avg} seconds, STDDEV: #{stddev} seconds.\n\n"
  end

  defp stats(timings) do
    avg = Enum.sum(timings) / Enum.count(timings)
    variances = Enum.map(timings, &(&1 - avg) * (&1 - avg))
    stddev = :math.sqrt(Enum.sum(variances) / Enum.count(variances))
    {avg |> Float.round(3), stddev |> Float.round(3)}
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
    duration = (duration / 1_000_000) |> Float.round(3)
    IO.puts ">>> Query finished with result: #{result} in #{duration} seconds."
    duration
  end
end

for query <- [
  "SELECT COUNT(*) FROM notes",
  "SELECT COUNT(*) FROM drafts_changes",
  "SELECT COUNT(*) FROM drafts_changes_encoded",
] do
  PerfTest.run(query)
end
