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
    {row_splitters, query} = convert_row_splitters(query)
    {split_rows(rows, row_splitters), query}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp convert_row_splitters(%Query{} = query) do
    case Query.all_selected_splitters(query) do
      [] ->
        {[], query}

      [innermost_splitter_expression | _] ->
        {new_splitter, query} = convert_row_splitter(query, innermost_splitter_expression)
        {remaining_splitters, query} = convert_row_splitters(query)
        {[new_splitter | remaining_splitters], query}
    end
  end

  defp convert_row_splitter(query, splitter_expression) do
    {splitter_index, query} = Query.next_row_index(query)
    new_splitter = %{function_spec: splitter_expression, row_index: splitter_index}

    expression_to_fetch =
      %Expression{
        Expression.first_argument!(splitter_expression) |
          name: "#{Function.readable_name(splitter_expression.function)}_return_value",
          type: Function.return_type(splitter_expression),
          row_index: splitter_index
      }

    {
      new_splitter,
      put_in(query, [Lenses.expression_instances(splitter_expression)], expression_to_fetch)
    }
  end

  defp split_rows(rows, []), do:
    # optimization -> no row splitters, so we're just passing the input enumerable
    rows
  defp split_rows(rows, row_splitters), do:
    Stream.flat_map(rows, &split_row(&1, row_splitters))

  defp split_row(row, row_splitters) do
    padded_row = row ++ List.duplicate(nil, length(row_splitters))
    Enum.reduce(row_splitters, [padded_row], &expand_rows(&2, &1))
  end

  defp expand_rows(rows, splitter), do:
    Enum.flat_map(rows, &expand_row(&1, splitter))

  defp expand_row(row, splitter), do:
    row
    |> splitter_values(splitter)
    |> Enum.map(&List.replace_at(row, splitter.row_index, &1))

  defp splitter_values(row, splitter), do:
    Expression.value(splitter.function_spec, row)
end
