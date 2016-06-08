defmodule Cloak.Processor.NegativeCondition do
  alias Cloak.Query.Columns
  alias Cloak.SqlQuery.Parsers.Token

  def apply(rows, %{where_not: clauses} = query) do
    Enum.reduce(clauses, rows, fn(clause, rows) -> reject(clause, rows, query) end)
  end

  def drop_filter_columns(rows, %{filter_columns: filter_columns} = query) do
    anonymizable = Enum.count(Columns.all(query, user_id: true)) - Enum.count(filter_columns)
    Enum.map(rows, &Enum.take(&1, anonymizable))
  end

  defp reject({:comparison, column, :=, %Token{value: %{value: value}}}, rows, query) do
    index = Columns.index(column, query, user_id: true)

    Enum.reject(rows, &(Enum.at(&1, index) == value))
  end
  defp reject(_, rows, _), do: rows
end
