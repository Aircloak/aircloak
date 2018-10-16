defmodule AirWeb.QueryView do
  @moduledoc false
  use Air.Web, :view

  @banner_length 80
  @banner_initial 20

  defp format_map({name, value}), do: "#{name}: #{format_value(value)}"

  defp format_value(values) when is_list(values), do: Enum.join(values, ", ")
  defp format_value(value), do: to_string(value)

  defp banner(word, delimeter),
    do:
      String.duplicate(delimeter, @banner_initial) <>
        " " <>
        word <>
        " " <>
        String.duplicate(
          delimeter,
          max(0, @banner_length - @banner_initial - 2 - String.length(word))
        )

  defp print_rows([]), do: ""

  defp print_rows(data) do
    cleaned_data =
      data
      |> Enum.map(fn raw_row ->
        raw_row["row"]
        |> Enum.concat([raw_row["unreliable"], raw_row["occurrences"]])
        |> Enum.map(&to_string/1)
      end)

    header = List.duplicate(" ", (cleaned_data |> hd() |> length()) - 2) ++ ["unreliable", "occurrences"]

    Aircloak.AsciiTable.format([header] ++ cleaned_data)
  end

  defp column_properties(column) do
    [
      column[:type],
      if(column[:user_id], do: "user id column", else: nil),
      "#{column[:shadow_table_size]} frequent values"
    ]
    |> Enum.reject(&is_nil/1)
  end
end
