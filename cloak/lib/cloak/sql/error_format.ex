defmodule Cloak.Sql.ErrorFormat do
  @moduledoc """
  Deals with formatting errors as strings with an indication of where they originated from in the query. For example
  given the query:

  ```sql
  select
  count(*) from purchases
  where price > 10
  and price > 5
  and price > 0
  ```

  and an error with message "Column price from table purchases must be limited to a finite, nonempty range." and
  source_location of `{3, 7}` it will produce the following error message:

  ```
  Expected table name.

  The error was detected at line 3, column 7:


    1:    count(*) from purchases
    2:    where price > 10
                ^
    3:    and price > 5
  ```

  Note that the code snippet in the error message will be indented with tabs, so that it's treated as a code block if
  formatted with markdown.
  """

  @line_number_width 6

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Formats an error as a string in the context of the given query."
  @spec format(String.t(), map()) :: String.t()
  def format(query, error), do: do_format(query, error.message, Map.get(error, :source_location))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_format(_query, message, nil), do: message

  defp do_format(query, message, {line, column}),
    do:
      to_string([
        message,
        "\n\nThe error was detected at line ",
        to_string(line),
        ", column ",
        to_string(column),
        error_indicator(query, line, column)
      ])

  defp error_indicator(query, line, column) do
    platform_independent_newline = ~r/(*ANY)\n/
    context = query |> String.split(platform_independent_newline) |> Enum.drop(max(line - 2, 0))

    [prev_line_no, line_no, next_line_no] =
      [line - 1, line, line + 1] |> Enum.map(&pad_line_number/1)

    filler = String.duplicate(" ", column - 1 + @line_number_width)

    case {line, context} do
      {1, [error_line, next_line | _]} ->
        [":\n\n\t", line_no, error_line, "\n\t", filler, "^\n\t", next_line_no, next_line]

      {1, [error_line | _]} ->
        [":\n\n\t", line_no, error_line, "\n\t", filler, "^\n"]

      {_, [prev_line, error_line]} ->
        [":\n\n\t", prev_line_no, prev_line, "\n\t", line_no, error_line, "\n\t", filler, "^\n"]

      {_, [prev_line, error_line, next_line | _]} ->
        [
          ":\n\n\t",
          prev_line_no,
          prev_line,
          "\n\t",
          line_no,
          error_line,
          "\n\t",
          filler,
          "^\n\t",
          next_line_no,
          next_line
        ]

      _invalid_location ->
        "."
    end
  end

  defp pad_line_number(line), do: String.pad_trailing("#{to_string(line)}:", @line_number_width)
end
