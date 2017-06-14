defmodule Cloak.Query.RowSplitters do
  @moduledoc """
  Implements functionality for splitting rows.
  If multiple splitters are supplied for the same function, then they are applied
  from the inner most outwards. More specifically:

    `splitter1(splitter2(value))`

  executes `splitter2` on value, and then `splitter1` on each return value of
  `splitter2` individually.

  If two splitters are provided at the same level, then a cross product is produced:

    `function1(splitter1, splitter2)`

  applies `function1` with each combination of the return values of `splitter1` and
  `splitter2`.

  This module also applies functions that are nested within row splitters:

    `splitter(function1(function2(value)))`
  """

  alias Cloak.Sql.{Query, Expression}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Splits individual rows into multiple rows based on applied splitter functions"
  @spec split(Enumerable.t, Query.t) :: Enumerable.t
  def split(rows, %Query{row_splitters: []}), do: rows
  def split(rows, query) do
    splitter_indices = query.row_splitters |> Enum.map(& &1.row_index) |> Enum.sort()
    Stream.flat_map(rows, &split_row(pad(&1, splitter_indices), query))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp split_row(row, query) do
    Enum.reduce(query.row_splitters, [row], fn(splitter, rows_acc) ->
      Enum.flat_map(rows_acc, &apply_top_most_row_splitter(&1, splitter))
    end)
  end

  defp apply_top_most_row_splitter(row, splitter) do
    for cell_value <- Expression.value(splitter.function_spec, row), do:
      List.replace_at(row, splitter.row_index, cell_value)
  end

  defp pad(row, splitter_indices), do:
    Enum.reduce(splitter_indices, row, &List.insert_at(&2, &1, nil))
end
