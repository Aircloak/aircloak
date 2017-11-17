defmodule Cloak.Sql.Compiler.TypeChecker.Narrative do
  @moduledoc """
  Provides functions for creating a narrative around a column expression
  being used in illegal ways.
  """

  alias Cloak.Sql.{Expression, Compiler.TypeChecker}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Constructs an explanation per column about why their usage is not allowed"
  @spec construct([TypeChecker.Type.offense], [Expression.t]) :: String.t
  def construct(transformations, columns) when is_list(columns), do:
    Enum.join([
      naive_plural("Column", "Columns", length(columns)),
      join_with_and(column_names(columns)),
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

  defp join_with_and([name]), do: name
  defp join_with_and([name1, name2]), do: name1 <> " and " <> name2
  defp join_with_and([name | names]), do: name <> ", " <> join_with_and(names)

  defp human_readable_offenses(transformations), do:
    transformations
    |> Enum.map(fn
      ({:dangerous_function, functions}) ->
        Enum.join([
          "potentially dangerous",
          naive_plural("function", "functions", length(functions)) <> ". ",
          naive_plural("The function is", "The functions are", length(functions)) <> ": ",
          join_with_and(functions) <> "."
        ], " ")
      ({:potentially_crashing_function, functions}) ->
        Enum.join([
          naive_plural("a function", "functions", length(functions)),
          "that could cause a runtime exception given the current usage.",
          naive_plural("The function is", "The functions are", length(functions)) <> ": ",
          join_with_and(functions) <> "."
        ], " ")
    end)
    |> Enum.join(" and ")
end
