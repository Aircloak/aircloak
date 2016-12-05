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

  alias Cloak.Aql.{Query, Column, Function}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Splits individual rows into multiple rows based on applied splitter functions"
  @spec split(Enumerable.t, Query.t) :: Enumerable.t
  def split(rows, %Query{row_splitters: []}), do: rows
  def split(rows, query), do: Stream.flat_map(rows, &split_row(&1, query))


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp split_row(row, query) do
    Enum.reduce(query.row_splitters, [row], fn(splitter, rows_acc) ->
      Enum.flat_map(rows_acc, &apply_function(&1, splitter))
    end)
  end

  defp apply_function(row, {:row_splitter, function_spec, index}) do
    row = row ++ List.duplicate(nil, index - (length(row) - 1))
    for cell_value <- apply_function(row, function_spec), do: List.replace_at(row, index, cell_value)
  end
  defp apply_function(row, {:function, _, args} = function_spec) do
    inner_functions_results = for arg <- args, do: apply_function(row, arg)
    inputs = all_input_combinations(inner_functions_results)
    if Function.row_splitting_function?(function_spec) do
      Enum.flat_map(inputs, &Function.apply(&1, function_spec))
    else
      Enum.map(inputs, &Function.apply(&1, function_spec))
    end
  end
  defp apply_function(_row, %Column{constant?: true, value: value}), do: [value]
  defp apply_function(row, %Column{constant?: false, db_row_position: index}), do:
    [Enum.at(row, index)]

  # Given a list of lists, it will create another list of lists where
  # each sublist contains an item from each of the input sublists, in
  # such a way that all possible combinations are produced.
  #
  #   iex(3)> all_input_combinations([[:a], [:b, :c], [true, false]])
  #   [
  #     [:a, :c, false],
  #     [:a, :c, true],
  #     [:a, :b, false],
  #     [:a, :b, true]
  #   ]
  #
  def all_input_combinations(args, acc \\ [], accs \\ [])
  # Done :)
  def all_input_combinations([], [], accs), do: accs
  # We have a completed sequence with one value from each input category, keep it
  def all_input_combinations([], acc, accs), do: [Enum.reverse(acc) | accs]
  # We ran out of options for the first value, hence the acc is incomplete and must be discarded
  def all_input_combinations([[] | _options], _acc, accs), do: accs
  # We attempt once with the first value in the first category, and once skipping the first value
  def all_input_combinations([[option | other_options] | later_options], acc, accs), do:
    all_input_combinations([other_options | later_options], acc,
      all_input_combinations(later_options, [option | acc], accs))
end
