defmodule Cloak.Sql.Compiler.ASTNormalization do
  @moduledoc "Deals with normalizing the query AST so that less cases must be handled downstream."

  alias Cloak.Sql.{CompilationError, Function, Parser, Compiler.Helpers, Query}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Rewrites the query AST, producing one with the same semantics that is simpler to process.

  Performs the following normalizations:
  * Replaces DISTINCT usage in SELECT lists with an equivalent GROUP BY (possibly adding a subquery).
  * Replaces NOT IN with an equivalent conjunction of <>
  * Replaces all usages of unary NOT by converting the involved expressions into an equivalent form (for example using
    De Morgan's laws)
  * Replaces IN (single_element) with = single_element
  * Lowercases the first argument of date_trunc
  * Normalizes function name synonyms, like lcase to lower
  """
  @spec normalize(Parser.parsed_query()) :: Parser.parsed_query()
  def normalize(ast) do
    ast
    |> Helpers.apply_bottom_up(&rewrite_distinct/1)
    |> Helpers.apply_bottom_up(&rewrite_not_in/1)
    |> Helpers.apply_bottom_up(&rewrite_not/1)
    |> Helpers.apply_bottom_up(&rewrite_in/1)
    |> Helpers.apply_bottom_up(&rewrite_date_trunc/1)
    |> Helpers.apply_bottom_up(&normalize_synonyms/1)
  end

  # -------------------------------------------------------------------
  # function name normalization
  # -------------------------------------------------------------------

  defp normalize_synonyms(ast) do
    update_in(ast, [Query.Lenses.terminals() |> Lens.filter(&Function.function?/1) |> Lens.at(1)], fn
      "lcase" -> %{canonical_name: "lower", synonym_used: "lcase"}
      "ucase" -> %{canonical_name: "upper", synonym_used: "ucase"}
      "ceiling" -> %{canonical_name: "ceil", synonym_used: "ceiling"}
      "pow" -> %{canonical_name: "^", synonym_used: "pow"}
      "mod" -> %{canonical_name: "%", synonym_used: "mod"}
      "dow" -> %{canonical_name: "weekday", synonym_used: "dow"}
      other -> other
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
    update_in(ast, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:in, lhs, [exp]} -> {:comparison, lhs, :=, exp}
      other -> other
    end)
  end

  # -------------------------------------------------------------------
  # DISTINCT rewriting
  # -------------------------------------------------------------------

  defp rewrite_distinct(%Query{type: :anonymized} = query), do: query

  defp rewrite_distinct(%{distinct?: true, group_by: [_ | _], order_by: [{column, _dir, _nulls} | _]}) do
    raise CompilationError,
      source_location: location(column),
      message:
        "Simultaneous usage of DISTINCT, GROUP BY, and ORDER BY in the same query is not supported." <>
          " Try using a subquery instead."
  end

  defp rewrite_distinct(ast = %{distinct?: true, group_by: [_ | _]}) do
    %{
      command: :select,
      distinct?: false,
      columns: [:*],
      from:
        {:subquery,
         %{
           alias: "__ac_distinct",
           ast:
             Map.merge(ast, %{
               command: :select,
               distinct?: false
             })
         }},
      group_by: grouping_clause(ast.columns)
    }
  end

  defp rewrite_distinct(ast = %{distinct?: true, columns: columns}) do
    if Enum.any?(columns, &aggregator?/1) do
      %{ast | distinct?: false}
    else
      Map.merge(ast, %{distinct?: false, group_by: grouping_clause(columns)})
    end
  end

  defp rewrite_distinct(ast), do: ast

  defp grouping_clause(columns), do: Enum.map(1..length(columns), &{:constant, :integer, &1, _location = nil})

  defp aggregator?({:function, name, args, _location}),
    do: Function.has_attribute?(name, :aggregator) or Enum.any?(args, &aggregator?/1)

  defp aggregator?(_), do: false

  # -------------------------------------------------------------------
  # NOT rewriting
  # -------------------------------------------------------------------

  defp rewrite_not(ast) do
    update_in(ast, [Query.Lenses.filter_clauses() |> Query.Lenses.all_conditions()], fn
      {:not, expr} -> negate(expr)
      other -> other
    end)
  end

  defp negate({:not, expr}), do: expr
  defp negate({:and, lhs, rhs}), do: {:or, negate(lhs), negate(rhs)}
  defp negate({:or, lhs, rhs}), do: {:and, negate(lhs), negate(rhs)}
  defp negate({:comparison, lhs, op, rhs}), do: {:comparison, lhs, negate_operator(op), rhs}
  defp negate(expr), do: {:not, expr}

  defp negate_operator(:=), do: :<>
  defp negate_operator(:<>), do: :=
  defp negate_operator(:<), do: :>=
  defp negate_operator(:>), do: :<=
  defp negate_operator(:<=), do: :>
  defp negate_operator(:>=), do: :<

  # -------------------------------------------------------------------
  # NOT IN rewriting
  # -------------------------------------------------------------------

  defp rewrite_not_in(ast) do
    update_in(ast, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:not, {:in, lhs, exps = [_ | _]}} ->
        [exp | exps] = Enum.reverse(exps)

        Enum.reduce(
          exps,
          {:comparison, lhs, :<>, exp},
          &{:and, {:comparison, lhs, :<>, &1}, &2}
        )

      other ->
        other
    end)
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp location({_, _, _, location}), do: location
  defp location(_), do: nil
end
