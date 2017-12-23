defmodule Aircloak.AsciiTable do
  @moduledoc "Formats a list of rows into an ASCII table"

  @spacer " | "


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Formats a list of rows as a table. The first row is assumed to be the header"
  def format(all_rows) do
    column_lengths = column_lengths(all_rows)
    [header | rows] = for row <- all_rows, do: format_row(row, column_lengths)
    Enum.join([header] ++ [spacer_row(column_lengths)] ++ rows, "\n")
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp format_row(row, column_lengths) do
    column_lengths
    |> Enum.zip(row)
    |> Enum.map(fn({cell_length, value}) -> String.pad_trailing(to_string(value), cell_length) end)
    |> Enum.join(@spacer)
  end

  defp spacer_row(column_lengths) do
    width = Enum.reduce(column_lengths, & (&1 + &2)) + ((length(column_lengths) - 1) * String.length(@spacer))
    List.duplicate("-", width) |> Enum.join()
  end

  def column_lengths(data), do:
    Enum.reduce(data, initial_row_lengths(data), fn(row, acc) ->
      acc
      |> Enum.zip(row_lengths(row))
      |> Enum.map(fn({a, b}) -> max(a, b) end)
    end)

  defp initial_row_lengths(data), do:
    data
    |> hd()
    |> row_lengths()

  defp row_lengths(row), do: Enum.map(row, & String.length(to_string(&1)))
end
