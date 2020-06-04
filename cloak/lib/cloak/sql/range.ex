defmodule Cloak.Sql.Range do
  @moduledoc "Represents a range the analyst applied in the query that needs to be tracked."

  alias Cloak.Sql.{Expression, FixAlign, Query, Condition, Function}

  @type t :: %__MODULE__{
          column: Expression.t(),
          interval: FixAlign.interval(any) | :implicit
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
  @spec find_ranges(Query.t()) :: [t]
  def find_ranges(query), do: inequality_ranges(query) ++ function_ranges(query)

  @doc """
  Returns true if the condition is a range in the context of the query, false otherwise.

  "Being a range" here means either being an inequality with a constant and thus part of a range or containing an
  implicit range expression.
  """
  @spec range?(Condition.t(), Query.t()) :: boolean
  def range?(condition, query) do
    cond do
      constant_inequality?(condition) -> true
      condition |> Condition.targets() |> Enum.any?(&implicit_range?(&1, query)) -> true
      true -> false
    end
  end

  @doc "Returns true if the condition is a column-constant inequality condition."
  @spec constant_inequality?(Condition.t()) :: boolean
  def constant_inequality?(condition),
    do: Condition.inequality?(condition) and Enum.any?(Condition.targets(condition), &Expression.constant?/1)

  @doc "Groups column-constant inequality conditions in the given clause by column."
  @spec inequalities_by_column(Lens.t(), Query.t()) :: [{Expression.t(), [Expression.t()]}]
  def inequalities_by_column(clause_lens, query) do
    clause_lens
    |> Query.Lenses.conditions()
    |> Lens.filter(&constant_inequality?/1)
    |> Lens.to_list(query)
    |> Enum.group_by(&(&1 |> Condition.subject() |> Expression.semantic()))
    |> Enum.map(&discard_redundant_inequalities/1)
  end

  # -------------------------------------------------------------------
  # Inequality ranges
  # -------------------------------------------------------------------

  defp inequality_ranges(query) do
    Query.Lenses.pre_anonymization_filter_clauses()
    |> inequalities_by_column(query)
    |> Enum.map(fn
      {column, inequalities} when length(inequalities) == 2 ->
        [bottom, top] = Enum.sort_by(inequalities, &Condition.direction/1, &Kernel.>/2)

        %__MODULE__{column: column, interval: {Condition.value(bottom), Condition.value(top)}}

      {column, [inequality]} ->
        %__MODULE__{
          column: column,
          interval: {String.to_atom(inequality.name), Condition.value(inequality)}
        }
    end)
  end

  defp discard_redundant_inequalities({column, inequalities}) do
    case {bottom, top} = Enum.split_with(inequalities, &(Condition.direction(&1) == :>)) do
      {[], []} ->
        {column, []}

      {_, []} ->
        {column, [Enum.max_by(bottom, &Condition.value/1)]}

      {[], _} ->
        {column, [Enum.min_by(top, &Condition.value/1)]}

      {_, _} ->
        {column, [Enum.max_by(bottom, &Condition.value/1), Enum.min_by(top, &Condition.value/1)]}
    end
  end

  # -------------------------------------------------------------------
  # Function ranges
  # -------------------------------------------------------------------

  defp function_ranges(query), do: equality_ranges(query) ++ select_ranges(query)

  defp equality_ranges(query) do
    Query.Lenses.pre_anonymization_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.filter(&Condition.equals?/1)
    |> Query.Lenses.operands()
    |> Lens.filter(&implicit_range?(&1, query))
    |> Lens.to_list(query)
    |> Enum.map(&function_range/1)
  end

  defp select_ranges(query) do
    Lens.key(:columns)
    |> Lens.all()
    |> Lens.reject(&aggregate?(&1))
    |> Lens.filter(&implicit_range?(&1, query))
    |> Lens.to_list(query)
    |> Enum.map(&function_range/1)
  end

  defp implicit_range?(:*, _query), do: false
  defp implicit_range?({:distinct, expression}, query), do: implicit_range?(expression, query)
  defp implicit_range?(%Expression{kind: :constant}, _query), do: false

  defp implicit_range?(function = %Expression{kind: :function, args: args}, query) do
    if Function.implicit_range?(function) do
      true
    else
      Enum.any?(args, &implicit_range?(&1, query))
    end
  end

  defp implicit_range?(column, query) do
    case Query.resolve_subquery_column(column, query) do
      :database_column -> false
      {column, subquery} -> implicit_range?(column, subquery)
    end
  end

  defp aggregate?(expression),
    do: Function.aggregator?(expression) or Enum.any?(expression.args, &aggregate?/1)

  defp function_range(column), do: %__MODULE__{column: column, interval: :implicit}
end
