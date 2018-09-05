defmodule Cloak.Compliance.QueryGenerator.Minimization do
  use Lens.Macros

  def minimize(ast, fun) do
    ast
    |> find_minimal_subquery(fun)
  end

  defp find_minimal_subquery(ast, fun) do
    ast
    |> get_in([all_nodes() |> Lens.filter(&match?({:query, _, _}, &1))])
    |> Enum.find(ast, fun)
  end

  deflensp all_nodes() do
    Lens.both(subnodes(), Lens.root())
  end

  deflensp subnodes() do
    Lens.index(2) |> Lens.all() |> Lens.recur()
  end
end
