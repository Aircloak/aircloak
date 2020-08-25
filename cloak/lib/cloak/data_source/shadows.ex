defmodule Cloak.DataSource.Shadows do
  @moduledoc "Entry point for checking negative conditions against shadow tables."

  alias Cloak.Sql.{Expression, Condition, Query, Function}

  require Aircloak

  @cache_module Aircloak.in_env(test: Cloak.TestShadowCache, else: Cloak.DataSource.Shadows.Cache)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Checks if the given condition matches a value in the shadow table for the appropriate column.
  Returns `:ok` if the check passed successfully.
  Returns `{:error, Expression.t()}` if a target value doesn't match.
  Returns `{:error, :unsafe_aggregator}` if the condition uses unsafe aggregators.
  Raises if the given condition is not a supported condition (=, <>, IN, NOT LIKE, NOT ILIKE).
  """
  @spec check_condition_safety(Query.filter_clause(), Query.t()) :: :ok | {:error, :unsafe_aggregator | Expression.t()}
  def check_condition_safety(condition, query) do
    [subject | targets] = condition |> expand_columns(query) |> Condition.targets()

    aggregators = used_aggregators(subject)
    columns = referenced_columns(subject)

    cond do
      Enum.all?(targets, &(not Expression.constant?(&1))) ->
        :ok

      columns == [] ->
        :ok

      aggregators == ["count"] ->
        :ok

      length(aggregators) > 1 or Enum.at(aggregators, 0) in ~w(sum avg stddev variance) ->
        {:error, :unsafe_aggregator}

      true ->
        [{table_name, column}] = columns
        subject = drop_min_max(subject)

        @cache_module.shadow(query.data_source, table_name, column)
        |> Enum.map(&evaluate(subject, &1))
        |> check_values(condition)
    end
  end

  @doc "Performs a cache lookup for the given column."
  @spec cache_lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, [any]} | {:error, :failed | :pending | :unknown_column}
  def cache_lookup(data_source, table_name, column_name), do: @cache_module.lookup(data_source, table_name, column_name)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp expand_columns(expression, query) do
    Query.Lenses.leaf_expressions()
    |> Lens.filter(&Expression.column?/1)
    |> Lens.map(expression, fn column ->
      case Query.resolve_subquery_column(column, query) do
        :database_column -> column
        {inner_column, subquery} -> expand_columns(inner_column, subquery)
      end
    end)
  end

  defp referenced_columns(expression) do
    expression
    |> get_in([Query.Lenses.leaf_expressions() |> Lens.filter(&Expression.column?/1)])
    |> Enum.map(&{&1.table.initial_name, &1.name})
    |> Enum.uniq()
  end

  defp used_aggregators(expression) do
    Query.Lenses.all_expressions()
    |> Lens.filter(&Function.aggregator?/1)
    |> Lens.to_list(expression)
    |> Enum.map(& &1.name)
  end

  defp check_values(shadow_values, condition) do
    cond do
      Condition.not_equals?(condition) or Condition.equals?(condition) ->
        [_subject, target] = Condition.targets(condition)
        if Expression.const_value(target) in shadow_values, do: :ok, else: {:error, target}

      Condition.in?(condition) ->
        [_subject | targets] = Condition.targets(condition)

        case Enum.find(targets, &(Expression.const_value(&1) not in shadow_values)) do
          nil -> :ok
          target -> {:error, target}
        end

      Condition.not_ilike?(condition) ->
        [_subject, target] = Condition.targets(condition)
        {_pattern, _regex, regex_ci} = Expression.const_value(target)
        if Enum.any?(shadow_values, &(&1 =~ regex_ci)), do: :ok, else: {:error, target}

      Condition.not_like?(condition) ->
        [_subject, target] = Condition.targets(condition)
        {_pattern, regex, _regex_ci} = Expression.const_value(target)
        if Enum.any?(shadow_values, &(&1 =~ regex)), do: :ok, else: {:error, target}

      true ->
        raise "Checking target values for the condition `#{inspect(condition)}` is not supported!"
    end
  end

  defp evaluate(expression, candidate) do
    expression
    |> put_in(
      [Query.Lenses.leaf_expressions() |> Lens.filter(&Expression.column?/1)],
      Expression.constant(:unknown, candidate)
    )
    |> Expression.const_value()
  end

  defp drop_min_max(expression) do
    Query.Lenses.all_expressions()
    |> Lens.filter(&(&1.kind == :function and &1.name in ["min", "max"]))
    |> Lens.map(expression, fn %Expression{args: [arg]} -> arg end)
  end

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(arg) do
    Aircloak.ChildSpec.supervisor(
      [
        # The cache table is owned by a separate process. This is mostly done for testing purposes, but it also improves
        # fault-tolerance. If the cache process crashes, the cache table will survive.
        __MODULE__.PersistentKeyValue,
        {@cache_module, arg}
      ],
      strategy: :rest_for_one
    )
  end
end
