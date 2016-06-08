defmodule Cloak.Processor.NegativeCondition do
  alias Cloak.Query.Columns

  def apply(rows, %{where_not: clauses} = query) do
    Enum.reduce(clauses, rows, fn(clause, rows) -> reject(clause, rows, query) end)
  end

  defp reject({:comparison, column, :=, value}, rows, query) do
    Enum.reject(rows, fn(row) ->
      Enum.at(row, Columns.index(column, query)) == value
    end)
  end
  defp reject(_, rows, _), do: rows
end
