defmodule DataQuality.Test.Persist do
  @moduledoc "Persist the results to disk as CSV and graphs"

  alias DataQuality.Test.{Utility, Logger}

  @min_threshold_for_graph 5
  @processing_timeout :timer.minutes(10)

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec to_disk([Test.result()]) :: :ok
  @doc "Persist the query results to disk as CSV. Also generates graphs for inspection of the results"
  def to_disk(all_results) do
    Logger.banner("Writing raw query results and producing graphs")

    all_results
    |> Utility.partition([:dimension, :distribution, :aggregate])
    |> Task.async_stream(&persist/1, timeout: @processing_timeout)
    |> Enum.map(fn {:ok, val} -> val end)

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp persist({selected_dimensions, segment_values}) do
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
      |> Enum.map(&process_segment_values(&1, segment_values, sources))
      |> Enum.unzip()

    persist_human_friendly_table(dir, selected_dimensions[:aggregate], sources, csv_data)
    output_graph(dir, selected_dimensions[:aggregate], graph_data)
  end

  defp process_segment_values(dimension_value, segment_values, sources) do
    relevant_results = Enum.filter(segment_values, &(&1[:dimension_value] == dimension_value))

    values =
      relevant_results
      |> Enum.map(&{&1[:source], &1})
      |> Enum.into(%{})

    real_value = relevant_results |> List.first() |> Map.get(:real_value)
    row_for_csv = [dimension_value, real_value | sources |> Enum.map(&Map.get(values, &1)[:value])]
    data_for_graph = %{real_value: real_value, dimension_value: dimension_value, source_values: values}
    {row_for_csv, data_for_graph}
  end

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
    if Enum.count(data) > @min_threshold_for_graph do
      graph_for(dir, aggregate, :anonymized_value, data, include_raw: true)
      graph_for(dir, aggregate, :error, data, include_raw: false)
      graph_for(dir, aggregate, :relative_error, data, include_raw: false)
    end
  end

  defp graph_for(dir, aggregate, property, data, options) do
    csv_path = Path.join([dir, Utility.name(aggregate) <> "-#{property}-r.csv"])
    img_path = Path.join([dir, Utility.name(aggregate) <> "-#{property}.png"])

    csv_header_line = "Dimension;Source;Result\n"
    row_data = row_data_for_property(data, property, options)

    File.write!(csv_path, csv_header_line <> row_data, [:write])
    :os.cmd(String.to_charlist("Rscript graph.r \"#{csv_path}\" \"#{img_path}\""))
    File.rm!(csv_path)
  end

  defp row_data_for_property(data, property, options),
    do:
      data
      |> Enum.flat_map(fn dimension_data ->
        dimension = dimension_data[:dimension_value]

        if(Keyword.get(options, :include_raw), do: [[dimension, "Raw", dimension_data[:real_value]]], else: []) ++
          Enum.map(dimension_data[:source_values], fn {name, values} ->
            [dimension, name, values[property]]
          end)
      end)
      |> Enum.map(fn row -> row |> Enum.map(&to_string/1) |> Enum.join(";") end)
      |> Enum.join("\n")
end
