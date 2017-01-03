defmodule Cloak.Query.ShrinkAndDrop do
  @moduledoc """
  Implements the shrink and drop algorithm. For an overview see "Shrink and drop" in docs/anonymization.md.
  """

  alias Cloak.Query.Anonymizer
  alias Cloak.Aql.{FixAlign, Expression, Query, Range}
  alias Cloak.Query.ShrinkAndDrop.Buffer


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Applies the shrink and drop algorithm in a streaming manner."
  @spec apply(Enumerable.t, Query.t) :: Enumerable.t
  def apply(rows, query) do
    query.ranges
    |> Enum.sort_by(fn(range) -> {range.column.name, range.column.table} end)
    |> Enum.reduce(rows, &suppress_outliers/2)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp suppress_outliers(%Range{column: column, interval: {low, high}}, rows) do
    seed_items = MapSet.new(["endpoint-#{low}", "endpoint-#{high}"])
    initial_state = %{next_id: 0, buffer: Buffer.new(buffer_size()), seed_items: seed_items}

    rows
    |> Stream.concat([:done])
    |> Stream.transform(initial_state, &do_suppress_outliers(&1, &2, column))
  end

  defp do_suppress_outliers(:done, %{buffer: buffer, seed_items: seed_items}, _column) do
    if Buffer.empty?(buffer) do
      {[], nil}
    else
      emit_buffer(buffer, seed_items)
    end
  end
  defp do_suppress_outliers(row, %{next_id: next_id, seed_items: seed_items, buffer: buffer}, column) do
    {new_buffer, popped_rows} = Buffer.add(buffer, decorate_row(row, next_id, column))
    new_seed_items = popped_rows |> Enum.map(fn({_, user_id, _, _}) -> user_id end) |> Enum.into(seed_items)

    {Enum.map(popped_rows, &undecorate_row/1), %{next_id: next_id + 1, seed_items: new_seed_items, buffer: new_buffer}}
  end

  defp emit_buffer(buffer, seed_items) do
    {count, _} = seed_items |> Anonymizer.new() |> Anonymizer.noisy_lower_bound()
    interval =
      buffer
      |> Buffer.range_except_extreme(count)
      |> case do
        {x, x} -> {x, Cloak.Data.plus_epsilon(x)}
        interval -> FixAlign.align_interval(interval)
      end

    {buffer |> Buffer.inside(interval) |> Enum.map(&undecorate_row/1), nil}
  end

  defp decorate_row(row, id, column), do: {id, user_id(row), Expression.value(column, row), row}

  defp undecorate_row({_, _, _, row}), do: row

  defp user_id([id | _rest]), do: id

  defp buffer_size do
    # A size that is unlikely to be exceeded by Anonymizer.noisy_lower_bound()
    {mean, sigma} = Anonymizer.config(:low_count_soft_lower_bound)
    round(mean + 5 * sigma + 1)
  end
end
