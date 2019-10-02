defmodule Cloak.Sql.Parser.ASTNormalization do
  @moduledoc "Deals with normalizing the query AST so that less cases must be handled downstream."

  alias Cloak.Sql.{Function, Parser, Compiler.Helpers, Query}
  alias Cloak.Sql.Parser.DNFNormalization

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
    |> update_in([Lens.keys([:where, :having])], &DNFNormalization.dnf/1)
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
    update_in(ast, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:in, lhs, [exp]} -> {:comparison, lhs, :=, exp}
      other -> other
    end)
  end

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
end
