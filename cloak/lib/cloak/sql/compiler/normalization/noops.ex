defmodule Cloak.Sql.Compiler.Normalization.Noops do
  @moduledoc "Removes noops from query AST"

  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.{Expression, Query}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Modifies the query to remove expressions that do nothing, like:
  * Casting a value to the same type it already is
  * Rounding/truncating integers
  """
  @spec remove(Query.t()) :: Query.t()
  def remove(query),
    do:
      query
      |> Helpers.apply_bottom_up(&remove_redundant_casts/1)
      |> Helpers.apply_bottom_up(&remove_redundant_rounds/1)

  # -------------------------------------------------------------------
  # Removing useless casts
  # -------------------------------------------------------------------

  defp remove_redundant_casts(query),
    do:
      update_in(query, [Query.Lenses.terminals()], fn
        %Expression{function: {:cast, type}, function_args: [expr = %Expression{type: type}]} ->
          expr

        other ->
          other
      end)

  # -------------------------------------------------------------------
  # Removing useless round/trunc
  # -------------------------------------------------------------------

  @round_funcs ~w/round trunc ceil floor/
  defp remove_redundant_rounds(query),
    do:
      update_in(query, [Query.Lenses.terminals()], fn
        %Expression{function: fun, function_args: [expr = %Expression{type: :integer}]}
        when fun in @round_funcs ->
          expr

        other ->
          other
      end)
end
