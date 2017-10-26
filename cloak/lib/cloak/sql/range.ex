defmodule Cloak.Sql.Range do
  @moduledoc "Represents a range the analyst applied in the query that needs to be tracked."

  alias Cloak.Sql.{Expression, FixAlign, Query, Condition}

  @type t :: %__MODULE__{
    column: Expression.t,
    interval: FixAlign.interval(any),
  }

  defstruct [:column, :interval]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

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
    inequalities_by_column(query)
    |> Enum.map(fn({column, inequalities}) ->
      [{:comparison, _, _, low}, {:comparison, _, _, high}] =
        Enum.sort_by(inequalities, &Condition.direction/1, &Kernel.>/2)
      new(column, {Expression.value(low), Expression.value(high)})
    end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp inequalities_by_column(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.inequality?/1)
    |> Lens.to_list(query)
    |> Enum.group_by(&Condition.subject/1)
end
