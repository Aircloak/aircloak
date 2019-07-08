defmodule Cloak.Sql.Parser.DNFNormalization do
  alias Cloak.Sql.Query

  def dnf(formula), do: formula |> push_in_negation() |> push_in_conjunction()

  defp push_in_conjunction(formula) do
    update_in(formula, [Query.Lenses.all_conditions()], fn
      {:and, {:or, a, b}, {:or, c, d}} ->
        {:or, {:or, push_in_conjunction({:and, a, c}), push_in_conjunction({:and, a, d})},
         {:or, push_in_conjunction({:and, b, c}), push_in_conjunction({:and, b, d})}}

      {:and, {:or, a, b}, c} ->
        {:or, push_in_conjunction({:and, a, c}), push_in_conjunction({:and, b, c})}

      {:and, c, {:or, a, b}} ->
        {:or, push_in_conjunction({:and, a, c}), push_in_conjunction({:and, b, c})}

      other ->
        other
    end)
  end

  defp push_in_negation(formula) do
    update_in(formula, [Query.Lenses.all_conditions()], fn
      {:not, expr} -> negate(expr)
      other -> other
    end)
  end

  defp negate({:not, expr}), do: expr
  defp negate({:and, lhs, rhs}), do: {:or, negate(lhs), negate(rhs)}
  defp negate({:or, lhs, rhs}), do: {:and, negate(lhs), negate(rhs)}
  defp negate(expr), do: {:not, expr}
end
