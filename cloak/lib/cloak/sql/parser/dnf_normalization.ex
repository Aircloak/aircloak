defmodule Cloak.Sql.Parser.DNFNormalization do
  @moduledoc """
  Contains functions for converting logical formulae to Disjunctive Normal Form.

  An expression is in Disjunctive Normal Form if it is a disjunction of conjunctions (an OR of ANDs). Any negations
  can only be applied to variables.

  The module uses repeated applications of De Morgan's laws (not (A and B) <=> (not A) or (not B),
  not (A or B) <=> (not A) and (not B)) and distributive law (A and (B or C) <=> (A and B) or (A and C)) to obtain the
  normalized form.
  """

  alias Cloak.Sql.Query

  @type formula :: {:and, formula, formula} | {:or, formula, formula} | {:not, formula} | any

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns an equivalent logical formula in Disjunctive Normal Form."
  @spec dnf(formula) :: formula
  def dnf(formula), do: formula |> push_in_negation() |> push_in_conjunction()

  # -------------------------------------------------------------------
  # Private functions
  # -------------------------------------------------------------------

  defp push_in_conjunction(formula) do
    update_in(formula, [Query.Lenses.all_conditions()], fn
      {:and, {:or, a, b}, c} -> push_in_conjunction({:or, {:and, a, c}, {:and, b, c}})
      {:and, a, {:or, b, c}} -> push_in_conjunction({:or, {:and, a, b}, {:and, a, c}})
      other -> other
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
