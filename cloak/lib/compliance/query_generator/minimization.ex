defmodule Cloak.Compliance.QueryGenerator.Minimization do
  use Lens.Macros

  def minimize(ast, fun) do
    ast
    |> find_minimal_subquery(fun)
    |> drop_clauses(fun)
    |> simplify_expressions(fun)
  end

  defp drop_clauses({type, value, clauses}, fun) do
    clauses
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce({type, value, clauses}, fn {clause, index}, result ->
      cond do
        removable?(clause) ->
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

  defp removable?({type, _, _}), do: type in [:where, :having, :offset, :limit, :order_by, :sample_users]

  defp minimizable?({type, _, _}), do: type in [:query, :subquery, :join, :from, :as]

  defp remove_at({type, value, clauses}, index) do
    {type, value, List.delete_at(clauses, index)}
  end

  defp replace_at({type, value, clauses}, index, clause) do
    {type, value, List.replace_at(clauses, index, clause)}
  end

  defp find_minimal_subquery(ast, fun) do
    ast
    |> get_in([nodes_of(:query)])
    |> Enum.find(ast, fun)
  end

  deflensp nodes_of(type) do
    all_nodes() |> Lens.filter(&match?({^type, _, _}, &1))
  end

  deflensp all_nodes() do
    Lens.both(subnodes(), Lens.root())
  end

  deflensp subnodes() do
    Lens.index(2) |> Lens.all() |> Lens.recur()
  end
end
