defmodule Compliance.Runtime do
  @moduledoc "Records how long query executions take for different data sources and outputs stats."

  use GenServer

  @graph_width 60
  @min_measurements 50


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link(), do:
    GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc "Record an execution time for a data source"
  def record(data_source, time), do:
    GenServer.cast(__MODULE__, {:record, data_source_name(data_source), time})

  @doc "Prints the time results and halts the server"
  def finalize(), do:
    GenServer.call(__MODULE__, :finalize)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  def init(_), do:
    {:ok, %{}}

  def handle_cast({:record, data_source, time}, state), do:
    {:noreply, Map.update(state, data_source, [time], & [time | &1])}

  def handle_call(:finalize, _from, state) do
    output_stats(state)
    {:stop, :normal, :ok, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp output_stats(state) do
    {measurements, small_data_sources} = Enum.map(state, fn({data_source, measurements}) ->
      {data_source, Enum.sort(measurements)}
    end)
    |> Enum.split_with(fn({_, measurements}) -> length(measurements) > @min_measurements end)

    max_value = Enum.reduce(measurements, 1, fn({_data_source, dm}, acc) -> max(acc, Enum.max(dm)) end)
    scale_fn = fn(val) -> trunc(val * @graph_width / max_value) end

    IO.puts ""
    print_header(max_value)

    measurements
    |> Enum.map(& stats_for_data_source(&1, scale_fn))
    |> Enum.sort_by(& {&1[:median], &1[:graph_lower_bound], &1[:graph_upper_bound], &1[:title]})
    |> Enum.map(& render_graph/1)
    |> render_stats()

    if length(small_data_sources) > 0 do
      excluded_data_source_names = small_data_sources
      |> Enum.map(& elem(&1, 0))
      |> Aircloak.OxfordComma.join()

      IO.puts "The following data sources were ignored as they had less than #{@min_measurements} " <>
        "measurements: #{excluded_data_source_names}"
    end

    IO.puts ""
  end

  defp print_header(max_value) do
    max_value = to_string(max_value) <> "ms"
    IO.puts "0ms" <> repeat(" ", @graph_width - 3 - String.length(max_value)) <> max_value
  end

  defp stats_for_data_source({data_source_name, measurements}, scale_fn) do
    num_measurements = length(measurements)
    fifth_percentile_index = num_measurements * 5 / 100 |> trunc()
    ninty_fifth_percentile_index = num_measurements * 95 / 100 |> trunc()
    ninty_nine_percentile_index = num_measurements * 99 / 100 |> trunc()
    median_index = num_measurements / 2 |> trunc()

    %{
      # Values for reporting
      fifth_percentile: Enum.at(measurements, fifth_percentile_index),
      median: Enum.at(measurements, median_index),
      ninty_fifth_percentile: Enum.at(measurements, ninty_fifth_percentile_index),
      ninty_nine_percentile: Enum.at(measurements, ninty_nine_percentile_index),
      min: hd(measurements),
      max: List.last(measurements),
      number_measurements: num_measurements,

      # Values for rendering a graph
      graph_lower_bound: Enum.at(measurements, fifth_percentile_index) |> scale_fn.(),
      graph_upper_bound: Enum.at(measurements, ninty_fifth_percentile_index) |> scale_fn.(),
      graph_middle: Enum.at(measurements, median_index) |> scale_fn.(),

      title: data_source_name,
    }
  end

  defp render_graph(data) do
    values =
      (for i <- 0..(@graph_width - 1), do: {i, "-"}) ++
      (for i <- data[:graph_lower_bound]..data[:graph_upper_bound], do: {i, "="}) ++
      [
        {0, "|"},
        {@graph_width - 1, "|"},
        {data[:graph_lower_bound], "<"},
        {data[:graph_upper_bound], ">"},
        {data[:graph_middle], "*"},
      ]

    graph = values
    |> Enum.reduce(:array.new(@graph_width), fn({index, value}, acc) -> :array.set(index, value, acc) end)
    |> :array.to_list()
    |> Enum.join("")

    IO.puts graph <> " #{data[:title]}"

    data
  end

  defp render_stats(data) do
    data_as_rows = Enum.map(data, & [&1[:title], &1[:min], &1[:max], &1[:number_measurements],
      &1[:fifth_percentile], &1[:median], &1[:ninty_fifth_percentile], &1[:ninty_nine_percentile]])
    header = ["", "min", "max", "#", "5th", "50th", "95th", "99th"]
    IO.puts "\n\n" <> Aircloak.AsciiTable.format([header] ++ data_as_rows)
  end

  defp data_source_name(%{driver: driver, name: name}), do:
    List.last(Module.split(driver)) <> " (#{name})"

  defp repeat(_what, times) when times <= 0, do: ""
  defp repeat(what, times), do:
    List.duplicate(what, times) |> Enum.join()
end
