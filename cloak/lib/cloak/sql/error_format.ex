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
      {1, [line_with_error, line_after | _]} ->
        [":\n\n", line_with_error, ?\n, String.duplicate(" ", column), "^ HERE\n", line_after]
      {1, [line_with_error | _]} ->
        [":\n\n", line_with_error, ?\n, String.duplicate(" ", column), "^ HERE"]
      {_, [line_before, line_with_error, line_after | _]} ->
        [":\n\n", line_before, ?\n, line_with_error, ?\n, String.duplicate(" ", column), "^ HERE\n", line_after]
      _invalid_location -> "."
    end
  end
end
