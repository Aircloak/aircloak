defmodule PerfTest do

  @table_name "aircloak_perftest"

  def run(""), do: run("SELECT COUNT(item), AVG(price) FROM #{@table_name} WHERE price <> 500")
  def run(query) do
    IO.puts ">>> Started performance test ..."
    data_source_setup()
    IO.puts ">>> Testing query '#{query}' ..."
    timings = Enum.map(1..5, fn (_) -> run_query(query) end)
    {avg, stddev} = stats(timings)
    IO.puts "\n>>> Performance test ended: AVERAGE duration: #{avg} seconds, STDDEV: #{stddev} seconds."
  end

  defp data_source_setup() do
    Cloak.Test.DB.register_test_table(String.to_atom(@table_name), @table_name)
  end

  defp stats(timings) do
    avg = Enum.sum(timings) / Enum.count(timings)
    variances = Enum.map(timings, &(&1 - avg) * (&1 - avg))
    stddev = :math.sqrt(Enum.sum(variances) / Enum.count(variances))
    {avg |> Float.round(3), stddev |> Float.round(3)}
  end

  defp run_query(statement) do
    data_source = Cloak.DataSource.fetch!("postgres/cloaktest1-native@localhost")
    {duration, row} = :timer.tc(fn () ->
      :ok = Cloak.Query.Runner.start("1", data_source, statement, [], {:process, self()})
      receive do
        {:reply, %{rows: [%{occurrences: 1, row: row}]}} -> row
        {:reply, %{error: error}} -> raise "Query failed with error: #{error}."
      end
    end)
    duration = (duration / 1_000_000) |> Float.round(3)
    IO.puts ">>> Query finished with result #{inspect row} in #{duration} seconds."
    duration
  end
end

query = case System.argv do
  [param] -> param
  _ -> "" # use default query
end
PerfTest.run(query)
