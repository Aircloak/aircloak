defmodule Cloak.Sql.Compiler.LowCountCheck do
  alias Cloak.Sql.{Query, Condition, LowCountCheck}
  alias Cloak.Sql.Compiler.Helpers

  def compile(query), do:
    query
    |> Helpers.apply_bottom_up(&compute_basic_checks/1)
    |> Helpers.apply_bottom_up(&float_checks/1)

  defp float_checks(query) do
    floated_checks = get_in(query, [Query.Lenses.subquery_low_count_checks()])
    %{query | low_count_checks: query.low_count_checks ++ floated_checks}
  end

  defp compute_basic_checks(query), do:
    %{query | low_count_checks: basic_checks(query)}

  defp basic_checks(query) do
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.like?/1)
    |> Lens.to_list(query)
    |> Enum.map(fn ({type, lhs, _rhs}) -> LowCountCheck.new(type, [lhs]) end)
  end
end
