defmodule Cloak.Compliance.QueryGenerator.Minimization do
  use Lens.Macros

  def minimize(ast, fun) do
    ast
    |> find_minimal_subquery(fun)
    |> drop_clauses(fun)
  end

  defp drop_clauses({type, value, clauses}, fun) do
    clauses
    |> Enum.with_index()
    |> Enum.reduce({type, value, clauses}, fn {clause, index}, result ->
      if type == :query and fun.(remove_at(result, index)) do
        remove_at(result, index)
      else
        minimized =
          drop_clauses(clause, fn ast ->
            fun.(replace_at(result, index, ast))
          end)

        replace_at(result, index, minimized)
      end
    end)
  end

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
