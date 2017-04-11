defmodule Cloak.Sql.Optimizer.Helper do
  @moduledoc """
  Utility methods split out into a standalone module
  for easier testing.
  """

  alias Cloak.Sql.{Parser, Function}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Determines whether or not a query is eligible for optimization"
  @spec eligible(Parser.parsed_query) :: boolean
  def eligible(query), do:
    from_single_table(query) and
    has_aggregate_function(query)


  # -------------------------------------------------------------------
  # Internal function
  # -------------------------------------------------------------------

  defp from_single_table(query), do:
    match?({:unquoted, _}, query[:from])

  defp has_aggregate_function(query), do:
    Enum.any?(query[:columns], fn
      ({:function, _, _} = function) ->
        Function.exists?(function) and Function.has_attribute?(function, :aggregator)
      (_) -> false
    end)
end
