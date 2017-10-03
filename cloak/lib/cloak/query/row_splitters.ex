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

  alias Cloak.Sql.{Expression, Function, Query, Query.Lenses}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Splits individual rows into multiple rows based on applied splitter functions"
  @spec split(Enumerable.t, Query.t) :: {Enumerable.t, Query.t}
  def split(rows, query), do:
    {
      split_rows(rows, Query.outermost_selected_splitters(query)),
      add_splitters_to_db_columns(query, Query.outermost_selected_splitters(query))
    }


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp add_splitters_to_db_columns(query, splitters), do:
    Enum.reduce(splitters, query, &add_splitter_to_db_columns(&2, &1))

  defp add_splitter_to_db_columns(query, splitter) do
    named_splitter = named_splitter(splitter)

    query
    |> put_in([Lenses.expression_instances(splitter)], named_splitter)
    |> Query.add_db_column(named_splitter)
  end

  defp named_splitter(splitter), do:
    %Expression{splitter |
      name: Enum.join(
        ["splitter", Function.readable_name(splitter.function), :erlang.unique_integer([:positive])],
        "_"
      )
    }

  defp split_rows(rows, []), do:
    # optimization -> no row splitters, so we're just passing the input enumerable
    rows
  defp split_rows(rows, row_splitters), do:
    Stream.flat_map(rows, &split_row(&1, row_splitters))

  defp split_row(row, row_splitters), do:
    row_splitters
    |> splitters_values(row)
    |> Enum.map(&(row ++ &1))

  defp splitters_values([], _row), do: [[]]
  defp splitters_values([splitter | remaining_splitters], row) do
    for \
      splitter_value <- Expression.splitter_values(splitter, row),
      remaining_splitters_values <- splitters_values(remaining_splitters, row)
    do
      [splitter_value | remaining_splitters_values]
    end
  end
end
