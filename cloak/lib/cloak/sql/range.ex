defmodule Cloak.Sql.Range do
  @moduledoc "Represents a range the analyst applied in the query that needs to be tracked."

  alias Cloak.Sql.{Expression, FixAlign, Query, Condition}

  @type t :: %__MODULE__{
    column: Expression.t,
    interval: FixAlign.interval(any),
  }

  defstruct [:column, :interval]

  @doc "Returns a Range with the given column and interval."
  @spec new(Expression.t, FixAlign.interval(any)) :: t
  def new(column, interval), do: %__MODULE__{column: column, interval: interval}

  @doc """
  Returns all ranges present in this query.

  Does not take subqueries into account. It assumes the query structure  has already been validated, in particular that
  there are no cases with more than 2 inequalities limiting a single column.
  """
  @spec find_ranges(Query.t) :: [t]
  def find_ranges(query), do:
    inequalities_by_column(query.where)
    |> Enum.map(fn({column, inequalities}) ->
      [{:comparison, _, _, low}, {:comparison, _, _, high}] =
        Enum.sort_by(inequalities, &Condition.direction/1, &Kernel.>/2)
      new(column, {Expression.value(low), Expression.value(high)})
    end)

  defp inequalities_by_column(where_clause), do:
    Query.Lenses.conditions()
    |> Lens.to_list(where_clause)
    |> Enum.filter(&Condition.inequality?/1)
    |> Enum.group_by(&Condition.subject/1)
    |> Enum.into(%{})
end
