defmodule Cloak.Aql.Comparison do
  @moduledoc "Contains utility functions for working with representations of comparisons."

  alias Cloak.Aql.{Query, Column, Function, Parser}

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
  @spec subject(Query.where_clause | Parser.where_clause) :: Column.t | Parser.column
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

  def to_function(_condition, _truth \\ true)
  def to_function({:not, condition}, truth), do: to_function(condition, not truth)
  def to_function({:comparison, column, operator, value}, truth) do
    value = extract_value(value)
    fn(row) -> compare(operator, Function.apply_to_db_row(column, row), value) == truth end
  end
  def to_function({:like, column, %Column{type: :text, value: pattern}}, truth) do
    regex = pattern |> to_regex() |> Regex.compile!("ums")
    fn(row) -> compare(:=~, Function.apply_to_db_row(column, row), regex) == truth end
  end
  def to_function({:ilike, column, %Column{type: :text, value: pattern}}, truth) do
    regex = pattern |> to_regex() |> Regex.compile!("uims")
    fn(row) -> compare(:=~, Function.apply_to_db_row(column, row), regex) == truth end
  end
  def to_function({:is, column, nil}, truth) do
    fn(row) -> (Function.apply_to_db_row(column, row) == nil) == truth end
  end
  def to_function({:in, column, values}, truth) do
    values = for value <- values, do: extract_value(value)
    fn(row) -> compare(:=, Function.apply_to_db_row(column, row), values) == truth end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp anchor(pattern), do: "^#{pattern}$"

  defp extract_value(%Column{value: value}), do: value
  defp extract_value(value), do: value

  def to_regex(pattern), do:
    pattern
    |> Regex.escape()
    |> String.replace("%", ".*")
    |> String.replace("_", ".")
    |> String.replace(".*.*", "%") # handle escaped `%` (`%%`)
    |> String.replace(".*.", "_") # handle escaped `_` (`%_`)
    |> anchor()

  defp compare(_operator, nil, _value), do: nil
  defp compare(:=, target, values) when is_list(values), do: Enum.member?(values, target)
  defp compare(:=, target, value), do: target == value
  defp compare(:<>, target, value), do: target != value
  defp compare(:>, target, value), do: target > value
  defp compare(:<, target, value), do: target < value
  defp compare(:>=, target, value), do: target >= value
  defp compare(:<=, target, value), do: target <= value
  defp compare(:=~, target, value), do: target =~ value
end
