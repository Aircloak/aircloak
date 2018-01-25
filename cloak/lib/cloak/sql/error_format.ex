defmodule Cloak.Sql.ErrorFormat do
  def format(query, error), do: do_format(query, error.message, Map.get(error, :source_location))

  defp do_format(_query, message, nil), do: message
  defp do_format(query, message, {line, column}), do:
    to_string([
      message, "\n\nThe error was detected at line ", to_string(line), ", column ", to_string(column),
      ":\n\n", error_indicator(query, line, column)
    ])

  defp error_indicator(query, line, column), do:
    [query, ?\n, String.duplicate(" ", column), "^ HERE"]
end
