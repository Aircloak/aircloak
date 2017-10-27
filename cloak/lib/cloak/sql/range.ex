defmodule Cloak.Sql.Range do
  @moduledoc "Represents a range the analyst applied in the query that needs to be tracked."

  alias Cloak.Sql.{Expression, FixAlign, Query, Condition, Function}

  @type t :: %__MODULE__{
    column: Expression.t,
    interval: FixAlign.interval(any) | :invalid | :implicit,
  }

  defstruct [:column, :interval]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns all ranges present in this query.

  Does not take subqueries into account. It assumes the query structure  has already been validated, in particular that
  there are no cases with more than 2 inequalities limiting a single column.
  """
  @spec find_ranges(Query.t) :: [t]
  def find_ranges(query), do:
    inequality_ranges(query) ++ function_ranges(query)


  # -------------------------------------------------------------------
  # Inequality ranges
  # -------------------------------------------------------------------

  defp inequality_ranges(query), do:
    inequalities_by_column(query)
    |> Enum.map(fn
      ({column, inequalities}) when length(inequalities) == 2 ->
        [{:comparison, _, _, low}, {:comparison, _, _, high}] =
          Enum.sort_by(inequalities, &Condition.direction/1, &Kernel.>/2)
        %__MODULE__{column: column, interval: {Expression.value(low), Expression.value(high)}}
      ({column, _other}) ->
        %__MODULE__{column: column, interval: :invalid}
    end)

  defp inequalities_by_column(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.inequality?/1)
    |> Lens.to_list(query)
    |> Enum.group_by(&Condition.subject/1)


  # -------------------------------------------------------------------
  # Function ranges
  # -------------------------------------------------------------------

  defp function_ranges(query), do:
    Lens.key(:columns)
    |> Query.Lenses.all_expressions()
    |> Lens.satisfy(&Function.has_attribute?(&1, :implicit_range))
    |> Lens.to_list(query)
    |> Enum.map(fn
      (%Expression{function_args: [column]}) ->
        %__MODULE__{column: column, interval: :implicit}
      (%Expression{function: function, function_args: [column, _]}) when function in ["trunc", "round"] ->
        %__MODULE__{column: column, interval: :implicit}
      (%Expression{function: "date_trunc", function_args: [_, column]}) ->
        %__MODULE__{column: column, interval: :implicit}
    end)
end
