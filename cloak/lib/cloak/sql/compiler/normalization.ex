defmodule Cloak.Sql.Compiler.Normalization do
  alias Cloak.Sql.{Expression, Query}

  def normalize(query), do:
    query
    |> expand_not_in()
    |> normalize_constants()

  defp expand_not_in(query), do:
    update_in(query, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:not, {:in, lhs, [exp | exps]}} ->
        Enum.reduce(exps, {:comparison, lhs, :<>, exp}, &{:and, {:comparison, lhs, :<>, &1}, &2})
      other -> other
    end)

  defp normalize_constants(query), do:
    update_in(query, [Query.Lenses.terminals()], &do_normalize_constants/1)

  defp do_normalize_constants(expression = %Expression{function?: true, aggregate?: false}) do
    if Enum.all?(expression.function_args, &Expression.constant?/1) do
      Expression.constant(expression.type, Expression.value(expression, []), expression.parameter_index)
    else
      expression
    end
  end
  defp do_normalize_constants(other), do: other
end
