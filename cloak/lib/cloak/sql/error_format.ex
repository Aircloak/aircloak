defmodule Cloak.Sql.ErrorFormat do
  def format(query, error), do: do_format(query, error.message, Map.get(error, :source_location))

  defp do_format(_query, message, nil), do: message
  defp do_format(query, message, {line, column}), do:
    to_string([
      message, "\n\nThe error was detected at line ", to_string(line), ", column ", to_string(column),
      error_indicator(query, line, column)
    ])

  @platform_independent_newline ~r/(*ANY)\n/
  defp error_indicator(query, line, column) do
    context = query |> String.split(@platform_independent_newline) |> Enum.drop(max(line - 2, 0))

    case {line, context} do
      {1, [error_line, line_after | _]} ->
        [":\n\n\t", error_line, "\n\t", String.duplicate(" ", column), "^\n\t", line_after]
      {1, [error_line | _]} ->
        [":\n\n\t", error_line, "\n\t", String.duplicate(" ", column), "^\n"]
      {_, [line_before, error_line]} ->
        [":\n\n\t", line_before, "\n\t", error_line, "\n\t", String.duplicate(" ", column), "^\n"]
      {_, [line_before, error_line, line_after | _]} ->
        [":\n\n\t", line_before, "\n\t", error_line, "\n\t", String.duplicate(" ", column), "^\n\t", line_after]
      _invalid_location -> "."
    end
  end
end
