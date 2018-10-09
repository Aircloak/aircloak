defmodule DataQuality.Test.Present do
  @moduledoc "Produces and renders statistic about the query runs"

  alias Aircloak.AsciiTable
  alias DataQuality.Test.{Utility, Logger}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec mse([Test.result()]) :: [Test.result()]
  @doc "Calculates the mean squared error for different sets of attributes and outputs the result to the screen"
  def mse(results) do
    Logger.banner("Data quality results")

    present_per_aggregate(results)
    present_mse_by_categories(results)
    present_mse_by_source(results)

    results
  end

  # -------------------------------------------------------------------
  # Presentations
  # -------------------------------------------------------------------

  defp present_per_aggregate(results),
    do:
      results
      |> Enum.group_by(& &1[:class])
      |> Enum.each(&present_class/1)

  defp present_class({class, values}) do
    Logger.header(class)

    dimensions = [:dimension, :distribution, :aggregate]
    rows = Utility.process_across_dimensions(values, %{}, dimensions, &rows_by_source/2)
    sources = sources_from_rows(rows, dimensions)
    source_names = Enum.map(sources, &(String.capitalize(&1) <> " (mse)"))
    col_headers = ["Distribution", "Dimension", "Aggregate"] ++ source_names

    table_rows =
      rows
      |> Enum.map(fn row ->
        [row[:distribution], row[:dimension], row[:aggregate]] ++ Enum.map(sources, &Map.get(row, &1))
      end)
      |> Enum.sort()
      |> Enum.map(&Enum.map(&1, fn value -> Utility.name(value) end))

    IO.puts(AsciiTable.format([col_headers | table_rows]) <> "\n")
  end

  defp rows_by_source(values, path),
    do:
      values
      |> Enum.group_by(& &1[:source])
      |> Enum.map(fn {source_name, source_values} -> {source_name, produce_mse(source_values)} end)
      |> Enum.into(%{})
      |> Map.merge(path)

  defp sources_from_rows(rows, dimensions),
    do:
      rows
      |> Enum.at(0)
      |> Map.drop(dimensions)
      |> Map.keys()
      |> Enum.sort()

  defp present_mse_by_categories(results), do: present_mse_by_dimensions([:distribution, :class, :dimension], results)

  defp present_mse_by_source(results), do: present_mse_by_dimensions([:source], results)

  defp present_mse_by_dimensions(dimensions, results) do
    dimension_names = Enum.map(dimensions, &(&1 |> to_string() |> String.capitalize()))

    Logger.header("MSE-values by #{Enum.join(dimension_names, ", ")}")
    Logger.log("")

    col_headers = dimension_names ++ ["mse"]

    rows =
      Utility.process_across_dimensions(results, %{}, dimensions, fn values, path ->
        Enum.map(dimensions, &path[&1]) ++ [produce_mse(values)]
      end)
      |> Enum.sort()
      |> Enum.map(&Enum.map(&1, fn val -> Utility.name(val) end))

    IO.puts(AsciiTable.format([col_headers | rows]) <> "\n")
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp produce_mse(values) do
    sum_squared_errors =
      values
      |> Enum.map(&(&1[:error] * &1[:error]))
      |> Enum.sum()

    Float.round(sum_squared_errors / Enum.count(values), 2)
  end
end
