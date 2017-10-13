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
  def split(rows, query) do
    query =
      query
      |> name_outermost_selected_splitters()
      |> add_outermost_splitters_to_db_columns()

    {
      split_rows(rows, Query.outermost_selected_splitters(query)),
      query
    }
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name_outermost_selected_splitters(query), do:
    # Note: we need to rename all outermost splitters in the select list first, and only then can we rename the
    # remaining splitters. This ensures that two identical outermost selected splitters will get two different names.
    query
    |> name_outermost_splitters_in_select_list()
    |> sync_references_to_selected_outermost_splitters()

  defp name_outermost_splitters_in_select_list(query), do:
    update_in(query, [Lenses.outermost_selected_splitters()], &name_splitter/1)

  defp sync_references_to_selected_outermost_splitters(query), do:
    Enum.reduce(
      Query.outermost_selected_splitters(query),
      query,
      fn(expression, query) ->
        Query.replace_expression(query, %Expression{expression | name: nil}, expression)
      end
    )

  defp add_outermost_splitters_to_db_columns(query), do:
    Enum.reduce(Query.outermost_selected_splitters(query), query, &Query.add_db_column(&2, &1))

  defp name_splitter(splitter), do:
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
