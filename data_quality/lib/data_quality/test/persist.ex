defmodule DataQuality.Test.Persist do
  @moduledoc "Persist the results to disk as CSV and graphs"

  alias Aircloak.OutputStatus
  alias DataQuality.Test.{Utility, Logger}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec to_disk(Test.results()) :: Test.results()
  @doc "Persist the query results to disk as CSV. Also generates graphs for inspection of the results"
  def to_disk(results) do
    Logger.banner("Writing raw query results and producing graphs")

    for {class, class_results} <- results do
      OutputStatus.new_line("Persisting data for " <> class, :pending, "saving")

      for {distribution, distribution_result} <- class_results do
        for {dimension, dimension_result} <- distribution_result do
          dir = Path.join(["output", distribution, Utility.name(dimension)])
          :ok = File.mkdir_p(dir)

          for aggregate_result <- dimension_result do
            persist_human_friendly_table(dir, aggregate_result)
            output_graph(dir, aggregate_result)
          end
        end
      end

      OutputStatus.done("Persisting data for " <> class)
    end

    results
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp persist_human_friendly_table(dir, {aggregate_variant, data}) do
    csv_path = Path.join([dir, Utility.name(aggregate_variant) <> ".csv"])

    result_names = data[:raw_data] |> hd() |> Map.keys() |> Enum.sort()
    csv_header_line = Enum.join(result_names, ";") <> "\n"

    File.write(csv_path, csv_header_line, [:write])

    numerical_results_lines =
      data[:raw_data]
      |> Enum.map(fn result_map ->
        result_map
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.map(&elem(&1, 1))
        |> Enum.join(";")
      end)
      |> Enum.join("\n")

    File.write!(csv_path, numerical_results_lines, [:append])
  end

  defp output_graph(dir, {aggregate_variant, data}) do
    # No need creating a graph when there is hardly any data
    if length(data[:raw_data]) > 5 do
      csv_path = Path.join([dir, Utility.name(aggregate_variant) <> "-r.csv"])
      img_path = Path.join([dir, Utility.name(aggregate_variant) <> ".png"])

      csv_header_line = "Index;Source;Result\n"

      row_lines =
        1..length(data[:raw_data])
        |> Enum.zip(data[:raw_data])
        |> Enum.flat_map(fn {index, results} ->
          Enum.map(results, fn {result_name, result_value} ->
            "#{index};#{result_name};#{result_value}"
          end)
        end)
        |> Enum.join("\n")

      File.write(csv_path, csv_header_line, [:write])
      File.write!(csv_path, row_lines, [:append])

      port = Port.open({:spawn, "Rscript graph.r \"#{csv_path}\" \"#{img_path}\""}, [:binary])
      wait_for_port_close(port)
      File.rm!(csv_path)
    end
  end

  defp wait_for_port_close(port) do
    Port.monitor(port)

    receive do
      {:DOWN, _, :port, ^port, :normal} -> :ok
    end
  end
end
