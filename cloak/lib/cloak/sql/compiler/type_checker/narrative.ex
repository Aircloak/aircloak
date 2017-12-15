defmodule Cloak.Sql.Compiler.TypeChecker.Narrative do
  @moduledoc """
  Provides functions for creating a narrative around a column expression
  being used in illegal ways.
  """

  alias Cloak.Sql.{Expression, Compiler.TypeChecker, Function}
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

  @doc "Constructs an explanation for illegal usage of implicit ranges"
  @spec construct_implicit_range_narrative([String.t], [String.t], [Expression.t]) :: String.t
  def construct_implicit_range_narrative([], functions, columns) do
    "In your query " <>
    naive_plural("function ", "functions ", length(functions)) <> joined_functions(functions) <>
    naive_plural(" was ", " were ", length(functions)) <> "used in the expression on " <>
    list_columns(columns) <> "."
  end
  def construct_implicit_range_narrative([implicit_range], functions, columns) do
    "Functions that implicitly produces ranges (in this case `#{Function.readable_name(implicit_range)}`), " <>
    "are not allowed to be used in combination with other functions. In your query " <>
    naive_plural("function ", "functions ", length(functions)) <> joined_functions(functions) <>
    naive_plural(" was ", " were ", length(functions)) <> "used in the expression on " <>
    list_columns(columns) <> "."
  end
  def construct_implicit_range_narrative(implicit_ranges, _functions, columns) do
    "Multiple functions which produce a range were used in an expression where only a single " <>
    "one is allowed at a time. In this case the implicit range functions were " <>
    joined_functions(implicit_ranges) <> " used on the " <>
    list_columns(columns) <> "."
  end


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
        "restricted " <> naive_plural("function", "functions", length(functions)) <> ". " <>
        list_problematic_functions(functions)
      ({:potentially_crashing_function, functions}) ->
        naive_plural("a function", "functions", length(functions)) <>
        "that could cause a runtime exception given the current usage. " <>
        list_problematic_functions(functions)
    end)
    |> OxfordComma.join()

  defp list_problematic_functions(functions) do
    naive_plural("The function is", "The functions are", length(functions)) <> ": " <>
    joined_functions(functions) <> "."
  end

  defp list_columns(columns) do
    naive_plural("column ", "columns ", length(columns)) <>
    OxfordComma.join(Enum.map(columns, &Expression.display_name/1))
  end

  defp joined_functions(functions), do:
    functions
    |> Enum.map(& Function.readable_name/1)
    |> Enum.map(& "`#{&1}`")
    |> OxfordComma.join()
end
