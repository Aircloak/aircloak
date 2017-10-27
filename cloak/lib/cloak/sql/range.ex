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
    filter_ranges(query) ++ select_ranges(query)

  defp filter_ranges(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.all_expressions()
    |> Lens.satisfy(&Function.has_attribute?(&1, :implicit_range))
    |> Lens.to_list(query)
    |> Enum.map(&function_range/1)

  defp select_ranges(query), do:
    Lens.key(:columns)
    |> Query.Lenses.all_expressions()
    |> Lens.satisfy(&Function.has_attribute?(&1, :implicit_range))
    |> Lens.satisfy(& not aggregate?(&1))
    |> Lens.to_list(query)
    |> Enum.map(&function_range/1)

  defp aggregate?(%Expression{constant?: true}), do: true
  defp aggregate?(%Expression{aggregate?: true}), do: true
  defp aggregate?(%Expression{function?: true, function_args: args}), do: Enum.all?(args, &aggregate?/1)
  defp aggregate?(_), do: false

  defp function_range(%Expression{function_args: [column]}), do:
    %__MODULE__{column: column, interval: :implicit}
  defp function_range(%Expression{function: fun, function_args: [column, _]}) when fun in ["trunc", "round"], do:
    %__MODULE__{column: column, interval: :implicit}
  defp function_range(%Expression{function: "date_trunc", function_args: [_, column]}), do:
    %__MODULE__{column: column, interval: :implicit}
end
