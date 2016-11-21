defmodule Cloak.Query.ShrinkAndDrop do
  alias Cloak.Query.Anonymizer
  alias Cloak.Aql.{FixAlign, Function}
  alias Cloak.Query.ShrinkAndDrop.Buffer

  def apply(rows, query), do: Enum.reduce(query.ranges, rows, &suppress_outliers/2)

  @supported_types [:integer, :real]
  defp suppress_outliers({column, _range}, rows) do
    if Enum.member?(@supported_types, Function.type(column)) do
      initial_state = %{next_id: 0, buffer: Buffer.new(buffer_size()), user_ids: MapSet.new()}

      rows
      |> Stream.concat([:done])
      |> Stream.transform(initial_state, &do_suppress_outliers(&1, &2, column))
    else
      rows
    end
  end

  defp do_suppress_outliers(:done, %{buffer: buffer, user_ids: user_ids}, _column) do
    if Buffer.empty?(buffer) do
      {[], nil}
    else
      emit_buffer(buffer, user_ids)
    end
  end
  defp do_suppress_outliers(row, %{next_id: next_id, user_ids: user_ids, buffer: buffer}, column) do
    {new_buffer, popped_rows} = row |> decorate_row(next_id, column) |> Buffer.add(buffer)
    new_user_ids = popped_rows |> Enum.map(fn({_, user_id, _, _}) -> user_id end) |> Enum.into(user_ids)

    {Enum.map(popped_rows, &undecorate_row/1), %{next_id: next_id + 1, user_ids: new_user_ids, buffer: new_buffer}}
  end

  defp emit_buffer(buffer, user_ids) do
    {count, _} = user_ids |> Anonymizer.new() |> Anonymizer.noisy_lower_bound()
    interval =
      buffer
      |> Buffer.range_dropping(count)
      |> case do
        {x, x} -> {x, x + epsilon(x)}
        interval -> FixAlign.align_interval(interval)
      end

    {buffer |> Buffer.inside(interval) |> Enum.map(&undecorate_row/1), nil}
  end

  # Very small number such that x + epsilon(x) > x
  defp epsilon(x), do: x / 100000000000000

  defp decorate_row(row, id, column), do: {id, user_id(row), value(row, column), row}

  defp undecorate_row({_, _, _, row}), do: row

  defp user_id([id | _rest]), do: id

  defp value(row, column), do: Function.apply_to_db_row(column, row)

  defp buffer_size do
    # A size that is unlikely to be exceeded by Anonymizer.noisy_lower_bound()
    {mean, _sigma} = Anonymizer.config(:low_count_soft_lower_bound)
    3 * mean + 1
  end
end
