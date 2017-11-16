defmodule Cloak.Sql.Compiler.TypeChecker.Narrative do
  @moduledoc """
  Provides functions for creating a narrative around a column expression
  being used in illegal ways.
  """

  alias Cloak.Sql.{Expression, Function, Compiler.TypeChecker}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Constructs an explanation per column about why their usage is not allowed"
  @spec construct([TypeChecker.Type.offense]) :: String.t
  def construct(columns) when is_list(columns), do:
     Enum.map_join(columns, " ", &construct_explanation(&1))

  @doc """
  Extends an existing narrative with additional potential offenses, allowing
  a narrative to be created in case a query usage violation has occurred.
  """
  @spec extend([TypeChecker.Type.t], [{boolean, TypeChecker.Type.offense_type}]) :: [TypeChecker.Type.offense]
  def extend(child_types, potential_offenses), do:
    child_types
    |> Enum.flat_map(&(&1.narrative_breadcrumbs))
    |> Enum.uniq()
    |> Enum.map(fn({expression, breadcrumbs}) ->
      breadcrumbs = Enum.reduce(potential_offenses, breadcrumbs, fn
        ({true, offense}, breadcrumbs_acc) -> [offense | breadcrumbs_acc] |> Enum.uniq()
        ({false, _}, breadcrumbs_acc) -> breadcrumbs_acc
      end)
      {expression, breadcrumbs}
    end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp construct_explanation({expression, offenses}), do:
    "Column #{Expression.display_name(expression)} is processed by #{human_readable_offenses(offenses)}."

  defp human_readable_offenses(offenses), do:
    offenses
    |> Enum.reverse()
    |> Enum.map(fn
      ({:dangerously_discontinuous, "/"}) ->
        "discontinuous function '/' ('/' can behave like a discontinuous function " <>
          "when the divisor is an expression that combines both a column value and " <>
          "a constant value)"
      ({:dangerously_discontinuous, function}) ->
        "discontinuous function '#{Function.readable_name(function)}'"
      ({:dangerous_math, name}) -> "math function '#{name}'"
      ({:potentially_crashing_function, "sqrt"}) -> "function 'sqrt' on a value that could be negative"
      ({:potentially_crashing_function, "/"}) -> "math function '/' with a divisor that could be zero"
    end)
    |> Enum.join(" and ")
end
