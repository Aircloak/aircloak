defmodule PerfTest do
  def run(data_source_name, query) do
    {:ok, data_source} = Cloak.DataSource.fetch(data_source_name)

    Enum.map(1..5, fn _ -> run_query(query, data_source) end)
    |> stats()
  end

  defp stats(timings) do
    avg = Enum.sum(timings) / Enum.count(timings)
    variances = Enum.map(timings, &((&1 - avg) * (&1 - avg)))
    stddev = :math.sqrt(Enum.sum(variances) / Enum.count(variances))

    {avg, stddev}
  end

  defp run_query(statement, data_source) do
    {duration, _result} =
      :timer.tc(fn ->
        :ok = Cloak.Query.Runner.start("1", data_source, statement, [], %{}, {:process, self()})

        receive do
          {:result, %{rows: rows}} -> rows |> Enum.map(&"#{&1.occurrences} x #{inspect(&1.row)}") |> Enum.join(", ")
          {:result, %{error: error}} -> raise "Query failed with error: #{error}."
        end
      end)

    duration = duration / 1_000_000
    IO.puts(">>> Query finished in #{duration} seconds.")
    duration
  end
end

defmodule InfluxDB do
  def post_result!(key, avg, stdev) do
    %{status_code: 204} =
      HTTPoison.post!(
        "http://localhost:8086/write?db=performance",
        "performance,query=#{key} time_avg=#{avg},time_stddev=#{stdev}"
      )
  end
end

for {key, query} <- %{
      count_notes: "SELECT COUNT(*) FROM notes",
      count_drafts_changes: "SELECT COUNT(*) FROM notes_changes",
      users: "SELECT age, height, name, COUNT(*) FROM users GROUP BY 1, 2, 3"
    } do
  IO.puts(">>> Started performance test ...")
  IO.puts(">>> Testing query '#{query}' ...")

  for data_source_name <- ["data_source_name", "data_source_name_encoded"] do
    IO.puts(">>> On data source '#{data_source_name}' ...")

    {avg, stdev} = PerfTest.run(data_source_name, query)
    InfluxDB.post_result!("#{data_source_name}-#{key}", avg, stdev)

    IO.puts(
      "\n>>> Performance test ended (#{data_source_name}): AVERAGE duration: #{avg} seconds, STDDEV: #{stdev} seconds.\n\n"
    )
  end
end
