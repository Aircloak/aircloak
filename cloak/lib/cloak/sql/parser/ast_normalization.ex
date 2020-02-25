defmodule Cloak.Sql.Parser.ASTNormalization do
  @moduledoc "Deals with normalizing the query AST so that less cases must be handled downstream."

  alias Cloak.Sql.{Function, Parser, Compiler.Helpers, Query}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Rewrites the query AST, producing one with the same semantics that is simpler to process.

  Performs the following normalizations:
  * Replaces DISTINCT usage in SELECT lists with an equivalent GROUP BY (possibly adding a subquery).
  * Replaces all usages of NOT by converting the involved expressions into an equivalent form (for example using
    De Morgan's laws)
  * Replaces IN (single_element) with = single_element
  * Lowercases the first argument of date_trunc
  * Normalizes function name synonyms, like lcase to lower
  """
  @spec normalize(Parser.parsed_query()) :: Parser.parsed_query()
  def normalize(ast) do
    ast
    |> Helpers.apply_bottom_up(&rewrite_in/1)
    |> Helpers.apply_bottom_up(&rewrite_not_expressions/1)
    |> Helpers.apply_bottom_up(&rewrite_date_trunc/1)
    |> Helpers.apply_bottom_up(&normalize_synonyms/1)
  end

  # -------------------------------------------------------------------
  # function name normalization
  # -------------------------------------------------------------------

  defp normalize_synonyms(ast) do
    update_in(ast, [Query.Lenses.terminals() |> Lens.filter(&Function.function?/1) |> Lens.at(1)], fn name ->
      case Aircloak.Functions.aliases() |> Map.fetch(name) do
        {:ok, canonical_name} -> %{canonical_name: canonical_name, synonym_used: name}
        :error -> name
      end
    end)
  end

  # -------------------------------------------------------------------
  # date_trunc rewriting
  # -------------------------------------------------------------------

  defp rewrite_date_trunc(ast) do
    update_in(ast, [Query.Lenses.terminals()], fn
      {:function, "date_trunc", [{:constant, :string, spec, spec_location}, argument], location} ->
        {:function, "date_trunc", [{:constant, :string, String.downcase(spec), spec_location}, argument], location}

      other ->
        other
    end)
  end

  # -------------------------------------------------------------------
  # IN rewriting
  # -------------------------------------------------------------------

  defp rewrite_in(ast) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&match?({:function, "in", [_, _], _}, &1))
    |> Lens.map(ast, fn {:function, "in", [subject, target], location} ->
      {:function, "=", [subject, target], location}
    end)
  end

  # -------------------------------------------------------------------
  # NOT rewriting
  # -------------------------------------------------------------------

  defp rewrite_not_expressions(ast) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&match?({:function, "not", [_], _}, &1))
    |> Lens.map(ast, &rewrite_not_expression/1)
  end

  defp rewrite_not_expression({:function, "not", [{:function, "not", [expr], _}], _}), do: expr

  defp rewrite_not_expression({:function, "not", [{:function, "and", [lhs, rhs], location_and}], location_not}) do
    lhs = rewrite_not_expression({:function, "not", [lhs], location_not})
    rhs = rewrite_not_expression({:function, "not", [rhs], location_not})
    {:function, "or", [lhs, rhs], location_and}
  end

  defp rewrite_not_expression({:function, "not", [{:function, "or", [lhs, rhs], location_or}], location_not}) do
    lhs = rewrite_not_expression({:function, "not", [lhs], location_not})
    rhs = rewrite_not_expression({:function, "not", [rhs], location_not})
    {:function, "and", [lhs, rhs], location_or}
  end

  defp rewrite_not_expression({:function, "not", [{:function, operator, args, location}], _})
       when operator in ~w(> < = <> >= <=),
       do: {:function, negate_operator(operator), args, location}

  defp rewrite_not_expression(expr), do: expr

  defp negate_operator("="), do: "<>"
  defp negate_operator("<>"), do: "="
  defp negate_operator("<"), do: ">="
  defp negate_operator(">"), do: "<="
  defp negate_operator("<="), do: ">"
  defp negate_operator(">="), do: "<"
end
