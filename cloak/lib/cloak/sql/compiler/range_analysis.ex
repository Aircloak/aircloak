defmodule Cloak.Sql.Compiler.RangeAnalysis do
  alias Cloak.Sql.{Expression, Query}

  def analyze_expression(expression),
    do: update_in(expression, [Query.Lenses.all_expressions()], &do_analyze_expression/1)

  defp do_analyze_expression(expression = %Expression{type: type, constant?: true, value: value})
       when type in [:integer, :real],
       do: %{expression | range: {value, value}}

  defp do_analyze_expression(expression), do: %{expression | range: :unknown}
end
