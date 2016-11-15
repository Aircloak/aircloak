defmodule Cloak.Query.ShrinkAndDrop do
  alias Cloak.Query.Anonymizer
  alias Cloak.Aql.{FixAlign, Function}

  def apply(rows, query) do
    for_suppression =
      query.ranges
      |> Stream.map(fn {column, range} -> for_suppression(rows, column, range) end)
      |> Enum.reduce(MapSet.new, &MapSet.union/2)

    Enum.reject(rows, &MapSet.member?(for_suppression, &1))
  end

  defp for_suppression(_rows, _column, {%Time{}, _}), do: MapSet.new
  defp for_suppression(_rows, _column, {%Date{}, _}), do: MapSet.new
  defp for_suppression(_rows, _column, {%NaiveDateTime{}, _}), do: MapSet.new
  defp for_suppression(rows, column, _range) do
    do_for_suppression(rows, column, compact_range(rows, column))
  end

  defp compact_range(rows, column) do
    values = Enum.map(rows, &Function.apply_to_db_row(column, &1))
    case {Enum.min(values), Enum.max(values)} do
      {x, x} -> {x, x + 1}
      {x, y} -> FixAlign.align_interval({x, y})
    end
  end

  defp do_for_suppression(rows, column, range) do
    for {x, y} <- FixAlign.subintervals(range) do
      {in_range, out_of_range} = Enum.partition(rows, fn(row) ->
        row_value = Function.apply_to_db_row(column, row)
        row_value >= x && row_value < y
      end)

      anonymizer = in_range |> Enum.map(&Enum.at(&1, 0)) |> MapSet.new() |> Anonymizer.new()

      case Anonymizer.sufficiently_large?(anonymizer, Enum.count(out_of_range)) do
        {true, _} -> MapSet.new
        {false, _} -> MapSet.new(out_of_range)
      end
    end
    |> Enum.reduce(MapSet.new, &MapSet.union/2)
  end
end
