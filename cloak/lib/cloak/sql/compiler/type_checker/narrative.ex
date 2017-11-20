defmodule Cloak.Sql.Compiler.TypeChecker.Narrative do
  @moduledoc """
  Provides functions for creating a narrative around a column expression
  being used in illegal ways.
  """

  alias Cloak.Sql.{Expression, Compiler.TypeChecker}
  alias Aircloak.OxfordComma


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Constructs an explanation per column about why their usage is not allowed"
  @spec construct([TypeChecker.Type.offense], [Expression.t]) :: String.t
  def construct(transformations, columns) when is_list(columns), do:
    Enum.join([
      naive_plural("Column", "Columns", length(columns)),
      OxfordComma.join(column_names(columns)),
      naive_plural("is processed by", "are processed by", length(columns)),
      human_readable_offenses(transformations)
    ], " ")


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp naive_plural(singular, _plural, 1), do: singular
  defp naive_plural(_singular, plural, _count), do: plural

  defp column_names(columns), do:
    Enum.map(columns, & Expression.display_name(&1))

  defp human_readable_offenses(transformations), do:
    transformations
    |> Enum.map(fn
      ({:restricted_function, functions}) ->
        Enum.join([
          "restricted",
          naive_plural("function", "functions", length(functions)) <> ". ",
          naive_plural("The function is", "The functions are", length(functions)) <> ": ",
          OxfordComma.join(functions) <> "."
        ], " ")
      ({:potentially_crashing_function, functions}) ->
        Enum.join([
          naive_plural("a function", "functions", length(functions)),
          "that could cause a runtime exception given the current usage.",
          naive_plural("The function is", "The functions are", length(functions)) <> ": ",
          OxfordComma.join(functions) <> "."
        ], " ")
    end)
    |> OxfordComma.join()
end
