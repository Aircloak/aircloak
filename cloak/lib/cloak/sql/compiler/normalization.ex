defmodule Cloak.Sql.Compiler.Normalization do
  alias Cloak.Sql.Query

  def normalize(query), do:
    query |> expand_not_in()

  defp expand_not_in(query), do:
    update_in(query, [Query.Lenses.filter_clauses()], fn(clauses) -> Enum.flat_map(clauses, &do_expand_not_in/1) end)

  defp do_expand_not_in({:not, {:in, lhs, exps}}), do:
    Enum.map(exps, &{:comparison, lhs, :<>, &1})
  defp do_expand_not_in(clause), do: [clause]
end
