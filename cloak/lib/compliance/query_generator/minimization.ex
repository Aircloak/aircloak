defmodule Cloak.Compliance.QueryGenerator.Minimization do
  @moduledoc "Implements query minimization."

  use Lens.Macros

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "See `Cloak.Compliance.QueryGenerator.minimize/2`."
  @spec minimize(QueryGenerator.ast(), (QueryGenerator.ast() -> boolean)) :: QueryGenerator.ast()
  def minimize(ast, fun) do
    ast
    |> collapse_subqueries(fun)
    |> drop_clauses(fun)
    |> simplify_expressions(fun)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp collapse_subqueries(query, fun) do
    case Lens.one!(from(), query) do
      {:table, _, _} ->
        query

      {:join, nil, [left, right, _on]} ->
        cond do
          fun.(modified = put_in(query, [from()], left)) -> collapse_subqueries(modified, fun)
          fun.(modified = put_in(query, [from()], right)) -> collapse_subqueries(modified, fun)
          true -> query
        end

      {:as, name, [{:subquery, nil, [subquery]}]} ->
        if fun.(subquery) do
          collapse_subqueries(subquery, fun)
        else
          modified =
            collapse_subqueries(subquery, fn ast ->
              fun.(put_in(query, [from()], {:as, name, [{:subquery, nil, [ast]}]}))
            end)

          put_in(query, [from()], {:as, name, [{:subquery, nil, [modified]}]})
        end
    end
  end

  deflensp from() do
    Lens.index(2) |> Lens.index(1) |> Lens.index(2) |> Lens.at(0)
  end

  defp drop_clauses({type, value, clauses}, fun) do
    clauses
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce({type, value, clauses}, fn {clause, index}, result ->
      cond do
        removable?(type, clause) ->
          removed = remove_at(result, index)
          if fun.(removed), do: removed, else: result

        minimizable?(clause) ->
          minimized =
            drop_clauses(clause, fn ast ->
              fun.(replace_at(result, index, ast))
            end)

          replace_at(result, index, minimized)

        true ->
          result
      end
    end)
  end

  defp simplify_expressions(whole = {type, _, subnodes}, fun) when type in [:and, :or, :not, :function] do
    subnodes
    |> Enum.find(fun)
    |> case do
      nil -> whole
      simpler -> simplify_expressions(simpler, fun)
    end
  end

  defp simplify_expressions({type, value, subnodes}, fun) do
    subnodes
    |> Enum.with_index()
    |> Enum.reduce({type, value, subnodes}, fn {subnode, index}, result ->
      simplified =
        simplify_expressions(subnode, fn ast ->
          fun.(replace_at(result, index, ast))
        end)

      replace_at(result, index, simplified)
    end)
  end

  defp removable?(parent, _) when parent in [:select, :order_by], do: true
  defp removable?(_parent, {type, _, _}), do: type in [:where, :having, :offset, :limit, :order_by, :sample_users]

  defp minimizable?({type, _, _}), do: type in [:query, :subquery, :join, :from, :as, :select, :order_by]

  defp remove_at({type, value, clauses}, index) do
    {type, value, List.delete_at(clauses, index)}
  end

  defp replace_at({type, value, clauses}, index, clause) do
    {type, value, List.replace_at(clauses, index, clause)}
  end
end
