defmodule Cloak.Sql.Compiler.TypeChecker.Access do
  @moduledoc """
  This module exposes some access functions that TypeChecker uses to navigate the query. They are used by Query.Features
  to compute things related to shadow table/isolator checks. They are made common, so that the two modules agree on the
  definitions.
  """

  use Lens.Macros
  alias Cloak.Sql.{Condition, Query, Expression}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a lens focusing on all queries (sub- and root-) with the type :anonymized."
  deflens anonymized_queries() do
    Query.Lenses.all_queries() |> Lens.filter(&(&1.type == :anonymized))
  end

  @doc "Returns a stream of `{subquery, negative_condition}` pairs that require a shadow table check."
  @spec negative_conditions(Query.t()) :: Stream.t({Query.t(), Query.where_clause()})
  def negative_conditions(query) do
    Query.Lenses.all_queries()
    |> Lens.context(do_negative_conditions())
    |> Lens.to_list(query)
    |> Stream.map(fn {query, condition} -> {query, expand_expressions(condition, query)} end)
    |> Stream.uniq_by(fn {_query, condition} -> column_and_condition(condition) end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  deflensp do_negative_conditions() do
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.filter(&(Condition.not_equals?(&1) or Condition.not_like?(&1)))
  end

  defp expand_expressions(condition, query) do
    update_in(condition, [Query.Lenses.leaf_expressions() |> Lens.filter(&Expression.column?/1)], fn expression ->
      case Query.resolve_subquery_column(expression, query) do
        :database_column -> expression
        {column, subquery} -> expand_expressions(column, subquery)
      end
    end)
  end

  defp column_and_condition(condition) do
    case Condition.targets(condition) do
      [subject, value] ->
        if Expression.column?(subject) and Expression.constant?(value) do
          {subject.name, subject.table, Condition.verb(condition), Expression.const_value(value)}
        else
          :erlang.unique_integer()
        end

      _ ->
        :erlang.unique_integer()
    end
  end
end
