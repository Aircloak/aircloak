defmodule AirWeb.QueryView do
  @moduledoc false
  use Air.Web, :view

  @banner_length 80
  @banner_initial 20

  defp format_map({name, value}), do:
    "#{name}: #{format_value(value)}"

  defp format_value(values) when is_list(values), do: Enum.join(values, ", ")
  defp format_value(value), do: to_string(value)

  def banner(word, delimeter), do:
    String.duplicate(delimeter, @banner_initial) <> " " <> word <> " " <>
      String.duplicate(delimeter, max(0, @banner_length - @banner_initial - 2 - String.length(word)))

  def print_rows(data) do
    cleaned_data = data
    |> Enum.map(fn(raw_row) ->
      raw_row["row"]
      |> Enum.concat([raw_row["users_count"], raw_row["occurrences"]])
      |> Enum.map(& to_string/1)
    end)

    header = List.duplicate(" ", (cleaned_data |> hd() |> length()) -2) ++ ["user count", "occurrences"]
    all_rows = [header] ++ cleaned_data

    column_lengths = column_lengths(all_rows)
    for row <- all_rows do
      column_lengths
      |> Enum.zip(row)
      |> Enum.map(fn({cell_length, value}) -> String.pad_trailing(to_string(value), cell_length) end)
      |> Enum.join(" | ")
    end
    |> Enum.join("\n")
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

  defp row_lengths(row), do: Enum.map(row, & String.length/1)
end
