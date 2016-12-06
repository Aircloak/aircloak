defmodule Cloak.Query.DBEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """

  alias Cloak.Aql.{Query, Comparison, Function}
  alias Cloak.Query.Sorter


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def select(stream, query) do
    stream
    |> filter_rows(query)
    |> select_columns(query)
    |> Sorter.order_rows(query)
    |> offset_rows(query)
    |> limit_rows(query)
  end

  def join(_lhs, _rhs, _join), do: []


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp filter_rows(stream, %Query{where: []}), do: stream
  defp filter_rows(stream, %Query{where: conditions}) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    Stream.filter(stream, &Enum.all?(filters, fn (filter) -> filter.(&1) end))
  end

  defp select_columns(stream, %Query{columns: columns}) do
    Stream.map(stream, fn (row) ->
        Enum.map(columns, &Function.apply_to_db_row(&1, row))
    end)
  end

  defp offset_rows(stream, %Query{offset: 0}), do: stream
  defp offset_rows(stream, %Query{offset: offset}), do: Stream.drop(stream, offset)

  defp limit_rows(stream, %Query{limit: nil}), do: stream
  defp limit_rows(stream, %Query{limit: limit}), do: Stream.take(stream, limit)
end
