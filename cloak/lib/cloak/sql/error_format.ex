defmodule Cloak.Sql.ErrorFormat do
  @moduledoc """
  Deals with formatting errors as strings with an indication of where they originated from in the query. For example
  given the query:

  ```sql
  select
  count(*) from
  ```

  and an error with message "Expected column definition." and source_location of `{2, 14}` it will produce the following
  error message:

  ```
  Expected table name.

  The error was detected at line 2, column 14:

    select
    count(*) from
                 ^
  ```

  Note that the code snippet in the error message will be indented with tabs, so that it's treated as a code block if
  formatted with markdown.
  """


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Formats an error as a string in the context of the given query."
  @spec format(String.t, map()) :: String.t
  def format(query, error), do: do_format(query, error.message, Map.get(error, :source_location))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_format(_query, message, nil), do: message
  defp do_format(query, message, {line, column}), do:
    to_string([
      message, "\n\nThe error was detected at line ", to_string(line), ", column ", to_string(column),
      error_indicator(query, line, column)
    ])

  @platform_independent_newline ~r/(*ANY)\n/
  defp error_indicator(query, line, column) do
    context = query |> String.split(@platform_independent_newline) |> Enum.drop(max(line - 2, 0))
    filler = String.duplicate(" ", column - 1)

    case {line, context} do
      {1, [error_line, line_after | _]} ->
        [":\n\n\t", error_line, "\n\t", filler, "^\n\t", line_after]
      {1, [error_line | _]} ->
        [":\n\n\t", error_line, "\n\t", filler, "^\n"]
      {_, [line_before, error_line]} ->
        [":\n\n\t", line_before, "\n\t", error_line, "\n\t", filler, "^\n"]
      {_, [line_before, error_line, line_after | _]} ->
        [":\n\n\t", line_before, "\n\t", error_line, "\n\t", filler, "^\n\t", line_after]
      _invalid_location -> "."
    end
  end
end
