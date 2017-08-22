defmodule Cloak.Sql.Compiler.LowCountCheck do
  alias Cloak.Sql.{Query, Condition, LowCountCheck}

  def compile(query), do:
    %{query | low_count_checks: low_count_checks(query)}

  defp low_count_checks(query) do
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.like?/1)
    |> Lens.to_list(query)
    |> Enum.map(fn ({type, lhs, _rhs}) -> LowCountCheck.new(type, [lhs]) end)
  end
end
