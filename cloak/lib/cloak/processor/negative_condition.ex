defmodule Cloak.Processor.NegativeCondition do
  @moduledoc """
  Implements handling of negated LIKE, ILIKE and equality WHERE clauses on the side of the application.
  These need special handling, because a malicious analyst would be able to find out information about
  individuals by adding a condition that would exclude an individual from a result set. Then by comparing
  the result of a query with and without that condition the analyst can find out if that user was in fact
  included in the result set. To avoid this we ignore the condition if it would remove too few users.
  """

  alias Cloak.Processor.Anonymizer
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token

  import Cloak.Type


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Applies or ignore negative conditions in the query to the given rows."
  @spec apply([Property.t], [String.t], Parser.compiled_query) :: [Property.t]
  def apply(rows, columns, %{where_not: clauses}) do
    clauses
    |> Enum.filter(&sufficient_matches?(&1, rows, columns))
    |> Enum.reduce(rows, fn(clause, rows) -> Enum.reject(rows, filter(clause, columns)) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sufficient_matches?(clause, rows, columns) do
    {result, _} =
      rows
      |> user_ids()
      |> Enum.into(MapSet.new())
      |> Anonymizer.new()
      |> Anonymizer.sufficiently_large?(filtered_rows(rows, clause, columns))

    result
  end

  defp filtered_rows(rows, clause, columns) do
    rows
    |> Enum.filter(filter(clause, columns))
    |> Enum.uniq_by(&user_id(&1))
  end

  defp filter({:comparison, column, :=, %Token{value: %{value: value}}}, columns) do
    index = index_with_user_id(column, columns)
    fn(row) -> Enum.at(row, index) == value end
  end
  defp filter({:comparison, column, :=, value}, columns) do
    index = index_with_user_id(column, columns)
    fn(row) -> Enum.at(row, index) == value end
  end
  defp filter({:like, column, %Token{value: %{type: :string, value: pattern}}}, columns) do
    index = index_with_user_id(column, columns)
    regex = to_regex(pattern)
    fn(row) -> Enum.at(row, index) =~ regex end
  end
  defp filter({:ilike, column, %Token{value: %{type: :string, value: pattern}}}, columns) do
    index = index_with_user_id(column, columns)
    regex = to_regex(pattern, [_case_insensitive = "i"])
    fn(row) -> Enum.at(row, index) =~ regex end
  end

  defp to_regex(sql_pattern, options \\ []) do
    options = Enum.join([_unicode = "u" | options])

    sql_pattern
    |> Regex.escape
    |> String.replace("%", ".*")
    |> String.replace("_", ".")
    |> anchor()
    |> Regex.compile!(options)
  end

  defp anchor(pattern), do: "^#{pattern}$"

  defp user_ids(rows), do: Enum.map(rows, &user_id/1)

  defp user_id(row), do: hd(row)

  # Return the index of column, 1-based, since the rows contain the user id as their first value
  defp index_with_user_id(column, columns) do
    Enum.find_index(columns, &(&1 === column)) + 1
  end
end
