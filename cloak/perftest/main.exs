defmodule PerfTest do

  @table_name "aircloak_perftest"

  defp data_source_setup() do
    Cloak.DataSource.register_test_table(String.to_atom(@table_name), @table_name, "user_id")
  end

  defp stats(timings) do
    avg = Enum.sum(timings) / Enum.count(timings)
    variances = Enum.map(timings, &(&1 - avg) * (&1 - avg))
    stddev = :math.sqrt(Enum.sum(variances) / Enum.count(variances))
    {avg |> Float.round(3), stddev |> Float.round(3)}
  end

  defp run_query(statement) do
    data_source = Cloak.DataSource.fetch!(:local)
    query = %Cloak.Query{id: "1", statement: statement, data_source: data_source}
    {duration, row} = :timer.tc(fn () ->
      :ok = Cloak.Query.start(query, {:process, self()})
      receive do
        {:reply, %{rows: [%{occurrences: 1, row: row}]}} -> row
        {:reply, %{error: error}} -> raise "Query failed with error: #{error}."
      end
    end)
    duration = (duration / 1_000_000) |> Float.round(3)
    IO.puts ">>> Query finished with result #{inspect row} in #{duration} seconds."
    duration
  end

  def run() do
    IO.puts ">>> Started performance test ..."
    data_source_setup()
    query = "SELECT COUNT(item), AVG(price) FROM #{@table_name} WHERE price <> 500"
    IO.puts ">>> Testing query '#{query}' ..."
    timings = Enum.map(1..5, fn (_) -> run_query(query) end)
    {avg, stddev} = stats(timings)
    IO.puts "\n>>> Performance test ended: AVERAGE duration: #{avg} seconds, STDDEV: #{stddev} seconds."
  end
end

PerfTest.run()
