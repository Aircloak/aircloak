defmodule Cloak.DataSource.Shadows do
  alias Cloak.Sql
  alias __MODULE__

  def safe?(condition, query) do
    subject = condition |> Sql.Condition.subject() |> expand_expression(query)
    value = Sql.Condition.value(condition)
    shadow = Shadows.Query.build_shadow(query.data_source, subject.table.name, subject.name)
    Shadows.Lookup.any?(subject, value, shadow)
  end

  defp expand_expression(expression, query) do
    update_in(expression, [Sql.Query.Lenses.leaf_expressions()], fn expression ->
      case Sql.Query.resolve_subquery_column(expression, query) do
        :database_column -> expression
        {column, subquery} -> expand_expression(column, subquery)
      end
    end)
  end
end
