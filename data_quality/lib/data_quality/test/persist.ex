defmodule DataQuality.Test.Persist do
  @moduledoc "Persist the results to disk as CSV and graphs"

  alias Aircloak.OutputStatus
  alias DataQuality.Test.{Utility, Logger}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec to_disk([Test.result()]) :: [Test.result()]
  @doc "Persist the query results to disk as CSV. Also generates graphs for inspection of the results"
  def to_disk(all_results) do
    Logger.banner("Writing raw query results and producing graphs")

    Utility.process_across_dimensions(all_results, %{}, [:dimension, :distribution, :aggregate], fn segment_values,
                                                                                                    selected_dimensions ->
      dir = Path.join(["output", selected_dimensions[:distribution], Utility.name(selected_dimensions[:dimension])])
      :ok = File.mkdir_p(dir)

      sources =
        segment_values
        |> Enum.map(& &1[:source])
        |> Enum.uniq()
        |> Enum.sort()

      {csv_data, graph_data} =
        segment_values
        |> Enum.map(& &1[:dimension_value])
        |> Enum.uniq()
        |> Enum.sort()
        |> Enum.map(fn dimension_value ->
          relevant_results = Enum.filter(segment_values, &(&1[:dimension_value] == dimension_value))

          values =
            relevant_results
            |> Enum.map(&{&1[:source], &1[:anonymized_value]})
            |> Enum.into(%{})

          real_value = relevant_results |> Enum.at(0) |> Map.get(:real_value)
          row_for_csv = [dimension_value, real_value | sources |> Enum.map(&Map.get(values, &1))]
          data_for_graph = %{real_value: real_value, dimension_value: dimension_value, source_values: values}
          {row_for_csv, data_for_graph}
        end)
        |> Enum.unzip()

      persist_human_friendly_table(dir, selected_dimensions[:aggregate], sources, csv_data)
      output_graph(dir, selected_dimensions[:aggregate], graph_data)
    end)

    all_results
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp persist_human_friendly_table(dir, aggregate, sources, rows) do
    csv_path = Path.join([dir, Utility.name(aggregate) <> ".csv"])
    csv_headers = ["Dimension", "Raw"] ++ Enum.map(sources, &String.capitalize/1)
    csv_header_line = Enum.join(csv_headers, ";") <> "\n"

    File.write(csv_path, csv_header_line, [:write])

    csv_data =
      rows
      |> Enum.map(&Enum.join(&1, ";"))
      |> Enum.join("\n")

    File.write!(csv_path, csv_data, [:append])
  end

  defp output_graph(dir, aggregate, data) do
    # No need creating a graph when there is hardly any data
    if Enum.count(data) > 5 do
      csv_path = Path.join([dir, Utility.name(aggregate) <> "-r.csv"])
      img_path = Path.join([dir, Utility.name(aggregate) <> ".png"])

      csv_header_line = "Dimension;Source;Result\n"

      row_data =
        data
        |> Enum.flat_map(fn dimension_data ->
          dimension = dimension_data[:dimension_value]

          [[dimension, "Raw", dimension_data[:real_value]]] ++
            Enum.map(dimension_data[:source_values], fn {name, value} ->
              [dimension, name, value]
            end)
        end)
        |> Enum.map(fn row -> row |> Enum.map(&to_string/1) |> Enum.join(";") end)
        |> Enum.join("\n")

      File.write!(csv_path, csv_header_line <> row_data, [:write])
      :os.cmd(String.to_charlist("Rscript graph.r \"#{csv_path}\" \"#{img_path}\""))
      File.rm!(csv_path)
    end
  end
end
