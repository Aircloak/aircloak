defmodule Cloak.Aql.Comparison do
  @moduledoc "Contains utility functions for working with representations of comparisons."

  alias Cloak.Aql.Query

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
  @spec column(Query.where_clause) :: String.t
  def column({:comparison, lhs, _, _rhs}), do: lhs.name
  def column({:not, comparison}), do: column(comparison)
  def column({:is, lhs, :null}), do: lhs.name
  def column({:in, lhs, _rhs}), do: lhs.name
  def column({:like, lhs, _rhs}), do: lhs.name
  def column({:ilike, lhs, _rhs}), do: lhs.name

  @doc "Returns a representation of the direction of the given inequality as `:<` or `:>`."
  @spec direction(Query.where_clause) :: direction
  def direction({:comparison, _, :<, _}), do: :<
  def direction({:comparison, _, :<=, _}), do: :<
  def direction({:comparison, _, :>, _}), do: :>
  def direction({:comparison, _, :>=, _}), do: :>

  @doc "Converts a 'LIKE' pattern string to a regex string."
  @spec to_regex(String.t) :: String.t
  def to_regex(pattern), do:
    pattern
    |> Regex.escape()
    |> String.replace("%", ".*")
    |> String.replace("_", ".")
    |> String.replace(".*.*", "%") # handle escaped `%` (`%%`)
    |> String.replace(".*.", "_") # handle escaped `_` (`%_`)
    |> anchor()


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp anchor(pattern), do: "^#{pattern}$"
end
