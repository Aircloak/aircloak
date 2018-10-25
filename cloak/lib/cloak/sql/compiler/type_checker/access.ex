defmodule Cloak.Sql.Compiler.TypeChecker.Access do
  @moduledoc """
  This module exposes some access functions that TypeChecker uses to navigate the query. They are used by Query.Features
  to compute things related to shadow table/isolator checks. They are made common, so that the two modules agree on the
  definitions.
  """

  use Lens.Macros
  alias Cloak.Sql.{Condition, Query, Expression, LikePattern, Function}
  alias Cloak.Sql.Compiler.TypeChecker.Type

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a lens focusing on all queries (sub- and root-) with the type :anonymized."
  deflens anonymized_queries() do
    Query.Lenses.all_queries() |> Lens.filter(&(&1.type == :anonymized))
  end

  @doc "Returns a stream of `{subquery, negative_condition}` pairs that require a shadow table check."
  @spec negative_conditions(Query.t()) :: Enumerable.t({Query.t(), Query.where_clause()})
  def negative_conditions(query) do
    Query.Lenses.all_queries()
    |> Lens.context(do_negative_conditions())
    |> Lens.to_list(query)
    |> Stream.map(fn {query, condition} -> {query, expand_expressions(condition, query)} end)
    |> Stream.uniq_by(fn {_query, condition} -> column_and_condition(condition) end)
  end

  @doc "Returns a list of conditions from `query` satisfying `predicate`."
  @spec conditions(Query.t(), (Query.where_clause() -> boolean)) :: [Query.where_clause()]
  def conditions(query, predicate) do
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.filter(predicate)
    |> Lens.to_list(query)
  end

  @doc "Returns a list of conditions that are not allowed on isolating columns in the given query."
  @spec potential_unclear_isolator_usages(Query.t()) :: [Query.where_clause()]
  def potential_unclear_isolator_usages(query) do
    conditions(query, &unclear_isolator_condition?(&1, query)) ++
      group_by_expressions(query, &unclear_isolator_expression?(&1, query))
  end

  # -------------------------------------------------------------------
  # Negative conditions
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

  # -------------------------------------------------------------------
  # Isolators
  # -------------------------------------------------------------------

  defp group_by_expressions(query, predicate) do
    Query.Lenses.all_queries()
    |> Lens.key(:group_by)
    |> Lens.all()
    |> Lens.filter(predicate)
    |> Lens.to_list(query)
  end

  defp unclear_isolator_condition?({:not, condition}, query), do: unclear_isolator_condition?(condition, query)
  defp unclear_isolator_condition?({:in, _, _}, _), do: true
  defp unclear_isolator_condition?({:like, _, pattern}, _), do: not LikePattern.simple?(pattern.value)
  defp unclear_isolator_condition?({:ilike, _, pattern}, _), do: not LikePattern.simple?(pattern.value)

  defp unclear_isolator_condition?(condition, query),
    do: condition |> Condition.targets() |> Enum.any?(&unclear_isolator_expression?(&1, query))

  defp unclear_isolator_expression?(expression, query) do
    not Type.clear_column?(Type.establish_type(expression, query), &allowed_isolator_function?/1)
  end

  @allowed_isolator_functions ~w(lower upper substring trim ltrim rtrim btrim extract_words)
  defp allowed_isolator_function?(function) do
    function in @allowed_isolator_functions or Function.implicit_range?(function)
  end
end
