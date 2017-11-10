defmodule Cloak.Sql.Condition do
  @moduledoc "Contains utility functions for working with conditions."

  alias Cloak.Sql.{Query, Expression, Parser, LikePattern}

  @inequalities [:<, :>, :<=, :>=]

  @type direction :: :< | :>

  @doc "Return true if the given where clause is an inequality (<, >, <=, >=), false otherwise."
  @spec inequality?(Query.where_clause) :: boolean
  def inequality?({:not, comparison}), do: inequality?(comparison)
  def inequality?({:comparison, _, operator, _}), do: Enum.member?(@inequalities, operator)
  def inequality?(_), do: false

  @doc "Returns true if the given where clause is an equality (=), false otherwise."
  @spec equals?(Query.where_clause) :: boolean
  def equals?({:comparison, _, :=, _}), do: true
  def equals?(_), do: false

  @doc "Returns true if the given where clause is a not-equals clause (<>), false otherwise."
  @spec not_equals?(Query.where_clause) :: boolean
  def not_equals?({:comparison, _, :<>, _}), do: true
  def not_equals?(_), do: false

  @doc "Returns true if the given where clause is a NOT LIKE clause, false otherwise."
  @spec not_like?(Query.where_clause) :: boolean
  def not_like?({:not, {:like, _, _}}), do: true
  def not_like?({:not, {:ilike, _, _}}), do: true
  def not_like?(_), do: false

  @doc "Returns true if the given where clause is a LIKE clause, false otherwise."
  @spec like?(Query.where_clause) :: boolean
  def like?({:like, _, _}), do: true
  def like?({:ilike, _, _}), do: true
  def like?(_), do: false

  @doc "Returns true if the given where clause is an IN clause, false otherwise."
  @spec in?(Query.where_clause) :: boolean
  def in?({:in, _, _}), do: true
  def in?(_), do: false

  @doc "Returns the term the given comparison compares against."
  @spec value(Query.where_clause) :: any
  def value({:comparison, _lhs, _, rhs}), do: rhs.value
  def value({:not, comparison}), do: value(comparison)
  def value({:is, _lhs, :null}), do: nil
  def value({:in, _lhs, rhs}), do: Enum.map(rhs, & &1.value)
  def value({:like, _lhs, rhs}), do: rhs.value
  def value({:ilike, _lhs, rhs}), do: rhs.value

  @doc "Returns the term the given comparison acts on."
  @spec subject(Query.where_clause | Parser.where_clause) :: Expression.t | Parser.column
  def subject({:comparison, lhs, _, _rhs}), do: lhs
  def subject({:not, comparison}), do: subject(comparison)
  def subject({:is, lhs, :null}), do: lhs
  def subject({:in, lhs, _rhs}), do: lhs
  def subject({:like, lhs, _rhs}), do: lhs
  def subject({:ilike, lhs, _rhs}), do: lhs

  @doc "Returns the targets of the comparison."
  @spec targets(Query.where_clause | Parser.where_clause) :: [Expression.t | Parser.column]
  def targets({:comparison, lhs, _, rhs}), do: [lhs, rhs]
  def targets({:not, comparison}), do: targets(comparison)
  def targets({:is, lhs, :null}), do: [lhs]
  def targets({:in, lhs, rhs}), do: [lhs | rhs]
  def targets({:like, lhs, rhs}), do: [lhs, rhs]
  def targets({:ilike, lhs, rhs}), do: [lhs, rhs]

  @doc "Returns a representation of the direction of the given inequality as `:<` or `:>`."
  @spec direction(Query.where_clause) :: direction
  def direction({:comparison, _, :<, _}), do: :<
  def direction({:comparison, _, :<=, _}), do: :<
  def direction({:comparison, _, :>, _}), do: :>
  def direction({:comparison, _, :>=, _}), do: :>

  @doc "Converts the given condition to a function that checks a row."
  @spec to_function(Query.where_clause, boolean) :: (any -> boolean)
  def to_function(_condition, _truth \\ true)
  def to_function(nil, _truth), do: nil
  def to_function({:and, lhs, rhs}, truth) do
    lhs_fun = to_function(lhs, truth)
    rhs_fun = to_function(rhs, truth)
    fn(row) -> lhs_fun.(row) and rhs_fun.(row) == truth end
  end
  def to_function({:or, lhs, rhs}, truth) do
    lhs_fun = to_function(lhs, truth)
    rhs_fun = to_function(rhs, truth)
    fn(row) -> lhs_fun.(row) or rhs_fun.(row) == truth end
  end
  def to_function({:not, condition}, truth), do: to_function(condition, not truth)
  def to_function({:comparison, column, operator, value}, truth) do
    fn(row) ->
      lhs = Expression.value(column, row)
      rhs = Expression.value(value, row)
      compare(operator, lhs, rhs) == truth
    end
  end
  def to_function({:like, column, %Expression{type: :like_pattern, value: pattern}}, truth) do
    regex = pattern |> LikePattern.to_regex("ums")
    fn(row) -> compare(:=~, Expression.value(column, row), regex) == truth end
  end
  def to_function({:ilike, column, %Expression{type: :like_pattern, value: pattern}}, truth) do
    regex = pattern |> LikePattern.to_regex("uims")
    fn(row) -> compare(:=~, Expression.value(column, row), regex) == truth end
  end
  def to_function({:is, column, :null}, truth) do
    fn(row) -> (Expression.value(column, row) == nil) == truth end
  end
  def to_function({:in, column, values}, truth) do
    values = for %Expression{constant?: true, value: value} <- values, do: value
    fn(row) -> compare(:in, Expression.value(column, row), values) == truth end
  end

  @doc "Returns the verb of the condition."
  @spec verb(Query.where_clause) :: atom
  def verb({:not, condition}), do: verb(condition)
  def verb({:comparison, _lhs, _, _rhs}), do: :comparison
  def verb({:is, _lhs, :null}), do: :is
  def verb({:in, _lhs, _rhs}), do: :in
  def verb({:like, _lhs, _rhs}), do: :like
  def verb({:ilike, _lhs, _rhs}), do: :ilike

  @doc "Combines two clauses with the given operator."
  @spec combine(:and | :or, Query.where_clause, Query.where_clause) :: Query.where_clause
  def combine(_op, nil, nil), do: nil
  def combine(_op, lhs, nil), do: lhs
  def combine(_op, nil, rhs), do: rhs
  def combine(op, lhs, rhs), do: {op, lhs, rhs}

  @doc "Rejects conditions that match the given function."
  @spec reject(Query.where_clause, (Query.where_clause -> boolean)) :: Query.where_clause
  def reject(nil, _matcher), do: nil
  def reject({operator, lhs, rhs}, matcher) when operator in [:or, :and] do
    case {reject(lhs, matcher), reject(rhs, matcher)} do
      {nil, nil} -> nil
      {nil, rhs} -> rhs
      {lhs, nil} -> lhs
      {lhs, rhs} -> {operator, lhs, rhs}
    end
  end
  def reject(condition, matcher), do:
    if matcher.(condition), do: nil, else: condition

  @doc "Splits the conditions tree into matchning and non-matching trees."
  @spec partition(Query.where_clause, (Query.where_clause -> boolean)) :: {Query.where_clause, Query.where_clause}
  def partition(conditions, matcher) do
    non_matching = reject(conditions, matcher)
    matching = reject(conditions, &not matcher.(&1))
    {matching, non_matching}
  end

  @doc "Negates a condition."
  @spec negate(Query.where_clause) :: Query.where_clause
  def negate({:not, condition}), do: condition
  def negate(condition), do: {:not, condition}

  @doc "Returns an impossible condition (TRUE = FALSE)."
  @spec impossible() :: Query.where_clause
  def impossible(), do:
    {:comparison, Expression.constant(:boolean, true), :=, Expression.constant(:boolean, false)}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compare(_operator, nil, _value), do: nil
  defp compare(:in, target, values) when is_list(values), do: Enum.member?(values, target)
  defp compare(:=, target, value), do: target == value
  defp compare(:<>, target, value), do: target != value
  defp compare(:>, target, value), do: Cloak.Data.gt(target, value)
  defp compare(:<, target, value), do: Cloak.Data.lt(target, value)
  defp compare(:>=, target, value), do: Cloak.Data.gt_eq(target, value)
  defp compare(:<=, target, value), do: Cloak.Data.lt_eq(target, value)
  defp compare(:=~, target, value), do: target =~ value
end
