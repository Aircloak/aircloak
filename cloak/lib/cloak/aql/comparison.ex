defmodule Cloak.Aql.Comparison do
  @moduledoc "Contains utility functions for working with representations of comparisons."

  alias Cloak.Aql.{Query, Expression, Function, Parser}

  @inequalities [:<, :>, :<=, :>=]

  @type direction :: :< | :>

  @doc "Return true if the given where clause is an inequality, false otherwise."
  @spec inequality?(Query.where_clause) :: boolean
  def inequality?({:not, comparison}), do: inequality?(comparison)
  def inequality?({:comparison, _, operator, _}), do: Enum.member?(@inequalities, operator)
  def inequality?(_), do: false

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

  @doc "Returns a representation of the direction of the given inequality as `:<` or `:>`."
  @spec direction(Query.where_clause) :: direction
  def direction({:comparison, _, :<, _}), do: :<
  def direction({:comparison, _, :<=, _}), do: :<
  def direction({:comparison, _, :>, _}), do: :>
  def direction({:comparison, _, :>=, _}), do: :>

  @doc "Converts the given condition to a function that checks a row."
  @spec to_function(Query.where_clause, boolean) :: (any -> boolean)
  def to_function(_condition, _truth \\ true)
  def to_function({:not, condition}, truth), do: to_function(condition, not truth)
  def to_function({:comparison, column, operator, value}, truth) do
    fn(row) ->
      lhs = Function.apply_to_db_row(column, row)
      rhs = Function.apply_to_db_row(value, row)
      compare(operator, lhs, rhs) == truth
    end
  end
  def to_function({:like, column, %Expression{type: :text, value: pattern}}, truth) do
    regex = pattern |> to_regex() |> Regex.compile!("ums")
    fn(row) -> compare(:=~, Function.apply_to_db_row(column, row), regex) == truth end
  end
  def to_function({:ilike, column, %Expression{type: :text, value: pattern}}, truth) do
    regex = pattern |> to_regex() |> Regex.compile!("uims")
    fn(row) -> compare(:=~, Function.apply_to_db_row(column, row), regex) == truth end
  end
  def to_function({:is, column, :null}, truth) do
    fn(row) -> (Function.apply_to_db_row(column, row) == nil) == truth end
  end
  def to_function({:in, column, values}, truth) do
    values = for %Expression{constant?: true, value: value} <- values, do: value
    fn(row) -> compare(:in, Function.apply_to_db_row(column, row), values) == truth end
  end

  @doc "Checks for a negative condition."
  @spec negative?(Query.where_clause) :: boolean
  def negative?({:not, _}), do: true
  def negative?(_), do: false

  @doc "Returns the verb of the condition."
  @spec verb(Query.where_clause) :: atom
  def verb({:not, condition}), do: verb(condition)
  def verb({:comparison, _lhs, _, _rhs}), do: :comparison
  def verb({:is, _lhs, :null}), do: :is
  def verb({:in, _lhs, _rhs}), do: :in
  def verb({:like, _lhs, _rhs}), do: :like
  def verb({:ilike, _lhs, _rhs}), do: :ilike


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp anchor(pattern), do: "^#{pattern}$"

  def to_regex(pattern), do:
    pattern
    |> Regex.escape()
    |> String.replace("%", ".*")
    |> String.replace("_", ".")
    |> String.replace(".*.*", "%") # handle escaped `%` (`%%`)
    |> String.replace(".*.", "_") # handle escaped `_` (`%_`)
    |> anchor()

  defp compare(_operator, nil, _value), do: nil
  defp compare(:in, target, values) when is_list(values), do: Enum.member?(values, target)
  defp compare(:=, target, value), do: target == value
  defp compare(:<>, target, value), do: target != value
  defp compare(:>, target, value), do: target > value
  defp compare(:<, target, value), do: target < value
  defp compare(:>=, target, value), do: target >= value
  defp compare(:<=, target, value), do: target <= value
  defp compare(:=~, target, value), do: target =~ value
end
