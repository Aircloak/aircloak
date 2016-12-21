defmodule Cloak.Query.Filter do
  @moduledoc "Helper functions for row filtering."

  alias Cloak.Aql.{Query, Comparison}

  @doc "Applies all filters to each row and returns a stream of rows passing all the filters."
  @spec apply_query_filters(Enumerable.t, Query.t) :: Enumerable.t
  def apply_query_filters(rows, %Query{where: conditions}) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    apply_filters(rows, filters)
  end

  @doc "Applies all filters to each row and returns a stream of rows passing all the filters."
  @spec apply_filters(Enumerable.t, [(any -> boolean)]) :: Enumerable.t
  def apply_filters(rows, []), do: rows
  def apply_filters(rows, filters), do:
    Stream.filter(rows, &Enum.all?(filters, fn(filter) -> filter.(&1) end))
end
