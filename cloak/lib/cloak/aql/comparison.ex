defmodule Cloak.Aql.Comparison do
  @moduledoc "Contains utility functions for working with representations of comparisons."

  alias Cloak.Aql.Parser

  @inequalities [:<, :>, :<=, :>=]

  @type direction :: :< | :>

  @doc "Return true if the given where clause is an inequality, false otherwise."
  @spec inequality?(Parser.where_clause) :: boolean
  def inequality?({:not, comparison}), do: inequality?(comparison)
  def inequality?({:comparison, _, operator, _}), do: Enum.member?(@inequalities, operator)
  def inequality?(_), do: false

  @doc "Returns the rhs value the given comparison compares against."
  @spec value(Parser.where_clause) :: any
  def value({:comparison, _, _, rhs}), do: rhs.value

  @doc "Returns a representation of the direction of the given inequality as `:<` or `:>`."
  @spec direction(Parser.where_clause) :: direction
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
