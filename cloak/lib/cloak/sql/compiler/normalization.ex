defmodule Cloak.Sql.Compiler.Normalization do
  @moduledoc "Deals with normalizing some expressions so that they are easier to deal with at later stages."

  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.{Expression, Query}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Modifies the query to remove certain expressions without changing semantics. Specifically:

  * Switches NOT IN expressions for an equivalent conjunction of <> expressions
  * Switches complex expressions involving constants (like 1 + 2 + 3) to with their results (6 in this case)
  * Switches upper(x) <> constant to lower(x) <> toggle_case(constant)

  These are useful for noise layers - we want to generate the same layer for semantically identical conditions,
  otherwise we have to fall back to probing.
  """
  @spec normalize(Query.t) :: Query.t
  def normalize(query), do:
    query
    |> Helpers.apply_bottom_up(&expand_not_in/1)
    |> Helpers.apply_bottom_up(&normalize_constants/1)
    |> Helpers.apply_bottom_up(&normalize_upper/1)


  # -------------------------------------------------------------------
  # NOT IN expansion
  # -------------------------------------------------------------------

  defp expand_not_in(query), do:
    update_in(query, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:not, {:in, lhs, [exp | exps]}} ->
        Enum.reduce(exps, {:comparison, lhs, :<>, exp}, &{:and, {:comparison, lhs, :<>, &1}, &2})
      other -> other
    end)


  # -------------------------------------------------------------------
  # Collapsing constant expressions
  # -------------------------------------------------------------------

  defp normalize_constants(query), do:
    update_in(query, [Query.Lenses.terminals()], &do_normalize_constants/1)

  defp do_normalize_constants(expression = %Expression{function?: true, aggregate?: false}) do
    if Enum.all?(expression.function_args, &Expression.constant?/1) do
      Expression.constant(expression.type, Expression.value(expression, []), expression.parameter_index)
      |> put_in([Lens.key(:alias)], expression.alias)
    else
      expression
    end
  end
  defp do_normalize_constants(other), do: other


  # -------------------------------------------------------------------
  # Normalizing upper(x) <> constant
  # -------------------------------------------------------------------

  defp normalize_upper(query), do:
    update_in(query, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:comparison, lhs = %Expression{function: "upper"}, :<>, rhs = %Expression{constant?: true}} ->
        {:comparison, %{lhs | function: "lower"}, :<>, update_in(rhs, [Lens.key(:value)], &toggle_case/1)}
      other -> other
    end)

  defp toggle_case(string), do:
    string
    |> String.graphemes()
    |> Enum.map(&toggle_one/1)
    |> Enum.join()

  defp toggle_one(string) do
    if String.upcase(string) == string do
      String.downcase(string)
    else
      String.upcase(string)
    end
  end
end
