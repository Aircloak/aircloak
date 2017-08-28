defmodule Cloak.Sql.Compiler.LowCountChecks do
  @moduledoc "Contains functions related to compilation of low count checks."

  alias Cloak.Sql.{Query, Condition, LowCountCheck, Expression}
  alias Cloak.Sql.Compiler.Helpers


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Fills in the low_count_checks for the given query. Furthermore, it modifies the query to float any data that will be
  needed to compute those checks to the top level.
  """
  @spec compile(Query.t) :: Query.t
  def compile(query), do:
    query
    |> Helpers.apply_bottom_up(&compute_basic_checks/1)
    |> Helpers.apply_bottom_up(&float_checks/1)


  # -------------------------------------------------------------------
  # Floating checks
  # -------------------------------------------------------------------

  defp float_checks(query), do:
    %{query | low_count_checks: query.low_count_checks ++ floated_checks(query)}
    |> aggregate_if_needed()
    |> add_db_columns()

  defp aggregate_if_needed(query), do:
    if query.subquery? and Helpers.aggregate?(query),
      do: do_aggregate(query),
      else: query

  defp do_aggregate(query), do:
    Lens.key(:low_count_checks) |> Lens.all() |> Lens.map(query, &do_aggregate_check(&1, query))

  defp do_aggregate_check(check = %{expressions: [unaggregated]}, _query), do:
    %{check | expressions:
      [
        Expression.function("min", [Expression.unalias(unaggregated)], unaggregated.type, _aggregate = true),
        Expression.function("max", [Expression.unalias(unaggregated)], unaggregated.type, _aggregate = true),
      ]
      |> Enum.map(&Helpers.set_unique_alias/1)
    }
  defp do_aggregate_check(check = %{expressions: [min, max]}, query), do:
    %{check | expressions:
      [
        Expression.function("min", [Helpers.reference_aliased(min, query)], min.type, _aggregate = true),
        Expression.function("max", [Helpers.reference_aliased(max, query)], max.type, _aggregate = true),
      ]
      |> Enum.map(&Helpers.set_unique_alias/1)
    }

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


  # -------------------------------------------------------------------
  # Basic checks
  # -------------------------------------------------------------------

  defp compute_basic_checks(query), do:
    %{query | low_count_checks: basic_checks(query)}

  defp basic_checks(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.like?/1)
    |> Lens.to_list(query)
    |> Enum.map(fn ({type, lhs, _rhs}) ->
      LowCountCheck.new([lhs |> preprocess(type) |> Helpers.set_unique_alias()])
    end)

  defp preprocess(expression, :like), do: expression
  defp preprocess(expression, :ilike), do: Expression.function("lower", [expression], expression.type)
end
