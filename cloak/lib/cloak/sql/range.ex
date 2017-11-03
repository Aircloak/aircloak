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
    equality_ranges(query) ++ top_level_select_ranges(query)

  defp equality_ranges(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.equals?/1)
    |> Lens.to_list(query)
    |> Enum.map(fn({:comparison, lhs, :=, _}) -> lhs end)
    |> Enum.filter(&implicit_range?(&1, query))
    |> Enum.map(&function_range/1)

  defp top_level_select_ranges(query), do:
     top_level_select(query)
     |> Lens.all()
     |> Lens.satisfy(& not aggregate?(&1))
     |> Lens.to_list(query)
     |> Enum.filter(&implicit_range?(&1, query))
     |> Enum.map(&function_range/1)

  defp top_level_select(%{subquery: true}), do: Lens.empty()
  defp top_level_select(_), do: Lens.key(:columns)

  defp implicit_range?(:*, _query), do: false
  defp implicit_range?(%Expression{constant?: true}, _query), do: false
  defp implicit_range?(function = %Expression{function?: true, function_args: args}, query) do
    if Function.has_attribute?(function, :implicit_range) do
      true
    else
      Enum.any?(args, &implicit_range?(&1, query))
    end
  end
  defp implicit_range?(column, query) do
    Lens.to_list(Query.Lenses.direct_subqueries(), query)
    |> Enum.find(&(&1.alias == column.table.name))
    |> case do
      nil -> false
      %{ast: subquery} ->
        column_index = Enum.find_index(subquery.column_titles, &(&1 == column.name))
        column = Enum.at(subquery.columns, column_index)
        implicit_range?(column, subquery)
    end
  end

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
  defp function_range(column), do:
    %__MODULE__{column: column, interval: :implicit}
end
