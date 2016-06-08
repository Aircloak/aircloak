defmodule Cloak.Processor.NegativeCondition do
  alias Cloak.Query.Columns
  alias Cloak.SqlQuery.Parsers.Token

  def apply(rows, %{where_not: clauses} = query) do
    clauses
    |> Enum.reduce(rows, fn(clause, rows) -> Enum.reject(rows, filter(clause, query)) end)
  end

  def drop_filter_columns(rows, %{filter_columns: filter_columns} = query) do
    anonymizable = Enum.count(Columns.all(query, user_id: true)) - Enum.count(filter_columns)
    Enum.map(rows, &Enum.take(&1, anonymizable))
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
  defp filter(_, _), do: fn(_) -> false end

  defp to_regex(sql_pattern) do
    sql_pattern
    |> Regex.escape
    |> String.replace("%", ".*")
    |> String.replace("_", ".")
    |> anchor
    |> Regex.compile!(_unicode = "u")
  end

  defp anchor(pattern), do: "^#{pattern}$"
end
