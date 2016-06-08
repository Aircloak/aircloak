defmodule Cloak.Processor.NegativeCondition do
  alias Cloak.Query.Columns
  alias Cloak.SqlQuery.Parsers.Token
  alias Cloak.Processor.Noise

  def apply(rows, %{where_not: clauses} = query) do
    clauses
    |> Enum.filter(&sufficient_matches?(&1, rows, query))
    |> Enum.reduce(rows, fn(clause, rows) -> Enum.reject(rows, filter(clause, query)) end)
  end

  def drop_filter_columns(rows, %{filter_columns: filter_columns} = query) do
    anonymizable = Enum.count(Columns.all(query, user_id: true)) - Enum.count(filter_columns)
    Enum.map(rows, &Enum.take(&1, anonymizable))
  end

  defp sufficient_matches?(clause, rows, query) do
    rows
    |> Enum.filter(filter(clause, query))
    |> Enum.count()
    |> Noise.passes_filter?(user_ids(rows))
  end

  defp filter({:comparison, column, :=, %Token{value: %{value: value}}}, query) do
    index = Columns.index(column, query, user_id: true)
    fn(row) -> Enum.at(row, index) == value end
  end
  defp filter({:like, column, %Token{value: %{type: :string, value: pattern}}}, query) do
    index = Columns.index(column, query, user_id: true)
    regex = to_regex(pattern)
    fn(row) -> Enum.at(row, index) =~ regex end
  end
  defp filter({:ilike, column, %Token{value: %{type: :string, value: pattern}}}, query) do
    index = Columns.index(column, query, user_id: true)
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

  defp user_ids(rows), do: Enum.map(rows, &hd/1)
end
