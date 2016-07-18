defmodule Cloak.Query.NegativeCondition do
  @moduledoc """
  Implements handling of negated LIKE, ILIKE and equality WHERE clauses on the side of the application.
  These need special handling, because a malicious analyst would be able to find out information about
  individuals by adding a condition that would exclude an individual from a result set. Then by comparing
  the result of a query with and without that condition the analyst can find out if that user was in fact
  included in the result set. To avoid this we ignore the condition if it would remove too few users.
  """

  alias Cloak.DataSource
  alias Cloak.Query.Anonymizer
  alias Cloak.SqlQuery
  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Parsers.Token


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Applies or ignore negative conditions in the query to the given rows."
  @spec apply([DataSource.row], [DataSource.column], Parser.compiled_query) :: [DataSource.row]
  def apply(rows, columns, %{where_not: clauses}) do
    clauses
    |> Enum.filter(&sufficient_matches?(&1, rows, columns))
    |> Enum.reduce(rows, fn(clause, rows) -> Enum.reject(rows, &filter(&1, columns, clause)) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp user_id([user_id | _rest]), do: user_id

  defp sufficient_matches?(clause, rows, columns) do
    real_count =
      rows
      |> filtered_rows(columns, clause)
      |> Enum.count()

    {result, _} =
      rows
      |> Enum.map(&user_id/1)
      |> Enum.into(MapSet.new())
      |> Anonymizer.new()
      |> Anonymizer.sufficiently_large?(real_count)

    result
  end

  defp filtered_rows(rows, columns, clause) do
    rows
    |> Enum.filter(&filter(&1, columns, clause))
    |> Enum.uniq_by(&user_id/1)
  end

  defp filter(row, columns, {:comparison, column, :=, %Token{value: %{value: value}}}) do
    column = SqlQuery.column_name(column)
    DataSource.fetch_value!(row, columns, column) == value
  end
  defp filter(row, columns, {:comparison, column, :=, value}) do
    column = SqlQuery.column_name(column)
    DataSource.fetch_value!(row, columns, column) == value
  end
  defp filter(row, columns, {:like, column, %Token{value: %{type: :string, value: pattern}}}) do
    column = SqlQuery.column_name(column)
    DataSource.fetch_value!(row, columns, column) =~ to_regex(pattern)
  end
  defp filter(row, columns, {:ilike, column, %Token{value: %{type: :string, value: pattern}}}) do
    column = SqlQuery.column_name(column)
    DataSource.fetch_value!(row, columns, column) =~ to_regex(pattern, [_case_insensitive = "i"])
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
end
