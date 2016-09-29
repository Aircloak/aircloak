defmodule Cloak.Aql.Comparison do
  @inequalities [:<, :>, :<=, :>=]

  def inequality?({:not, comparison}), do: inequality?(comparison)
  def inequality?({:comparison, _, operator, _}), do: Enum.member?(@inequalities, operator)
  def inequality?(_), do: false

  def value({:comparison, _, _, rhs}), do: rhs.value

  def direction({:comparison, _, :<, _}), do: :<
  def direction({:comparison, _, :<=, _}), do: :<
  def direction({:comparison, _, :>, _}), do: :>
  def direction({:comparison, _, :>=, _}), do: :>
end
