defmodule Cloak.Sql.Compiler.LowCountCheck do
  alias Cloak.Sql.{Query, Condition, LowCountCheck}
  alias Cloak.Sql.Compiler.Helpers

  def compile(query), do:
    query
    |> Helpers.apply_bottom_up(&compute_basic_checks/1)
    |> Helpers.apply_bottom_up(&float_checks/1)

  defp float_checks(query) do
    %{query | low_count_checks: query.low_count_checks ++ floated_checks(query)}
    |> add_db_columns()
  end

  defp floated_checks(query), do:
    Query.Lenses.direct_subqueries()
    |> Lens.to_list(query)
    |> Enum.flat_map(fn (%{ast: subquery, alias: alias}) ->
      subquery_table = Enum.find(query.selected_tables, & &1.name == alias)
      true = subquery_table != nil

      Lens.all() |> Lens.key(:expressions) |> Lens.all()
      |> Lens.map(subquery.low_count_checks, &Helpers.reference_aliased(&1, subquery, subquery_table))
    end)

  defp add_db_columns(query) do
    to_add = Enum.flat_map(query.low_count_checks, & &1.expressions)
    {query, to_add} = Helpers.drop_redundant_floated_columns(query, query.db_columns, to_add)
    Enum.reduce(to_add, query, &Query.add_db_column(&2, &1))
  end

  defp compute_basic_checks(query), do:
    %{query | low_count_checks: basic_checks(query)}

  defp basic_checks(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.like?/1)
    |> Lens.to_list(query)
    |> Enum.map(fn ({type, lhs, _rhs}) -> LowCountCheck.new(type, [Helpers.set_unique_alias(lhs)]) end)
end
