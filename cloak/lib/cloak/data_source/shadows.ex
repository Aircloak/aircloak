defmodule Cloak.DataSource.Shadows do
  @moduledoc "Entry point for checking negative conditions against shadow tables."

  alias Cloak.{Sql, DataSource}

  require Aircloak

  @cache_module Aircloak.in_env(test: Cloak.TestShadowCache, else: Cloak.DataSource.Shadows.Cache)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Checks if the given negative condition matches a value in the shadow table for the appropriate column. Returns
  `{:ok, true | false}` if the check was performed successfully. Returns `{:error, :multiple_columns}` if the condition
  references multiple columns and is not a `column1 <> column2` condition. This should not happen given other
  restrictions on negative conditions. Returns `{:error, :invalid_condition}` if the given condition is not a negative
  condition (<>, NOT LIKE, NOT ILIKE). Note that any leaf nodes in the provided condition should either be constants or
  actual DB columns. No attempt will be made to resolve columns in subqueries.
  """
  @spec safe?(Sql.Query.filter_clause(), DataSource.t()) ::
          {:ok, boolean} | {:error, :multiple_columns} | {:error, :invalid_condition}
  def safe?(condition, data_source) do
    if condition |> Sql.Condition.targets() |> Enum.any?(&Sql.Expression.constant?/1) |> :erlang.not() do
      {:ok, true}
    else
      do_safe?(condition, data_source)
    end
  end

  @doc "Performs a cache lookup for the given column."
  @spec cache_lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, [any]} | {:error, :failed | :pending | :unknown_column}
  def cache_lookup(data_source, table_name, column_name), do: @cache_module.lookup(data_source, table_name, column_name)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_safe?(condition, data_source) do
    expression = Sql.Condition.subject(condition)

    case columns(expression) do
      [] ->
        {:ok, true}

      [{table, column}] ->
        shadow = @cache_module.shadow(data_source, table.name, column) |> Stream.map(&evaluate(expression, &1))
        any?(condition, shadow)

      _ ->
        {:error, :multiple_columns}
    end
  end

  defp columns(expression) do
    expression
    |> get_in([Sql.Query.Lenses.leaf_expressions() |> Lens.filter(&Sql.Expression.column?/1)])
    |> Enum.map(&{&1.table, &1.name})
    |> Enum.uniq()
  end

  defp any?(condition, shadow) do
    cond do
      Sql.Condition.not_equals?(condition) ->
        value = Sql.Condition.value(condition)
        {:ok, Enum.any?(shadow, &(&1 == value))}

      Sql.Condition.not_ilike?(condition) ->
        value = condition |> Sql.Condition.value() |> Sql.LikePattern.to_case_insensitive_regex()
        {:ok, Enum.any?(shadow, &(&1 =~ value))}

      Sql.Condition.not_like?(condition) ->
        value = condition |> Sql.Condition.value() |> Sql.LikePattern.to_regex()
        {:ok, Enum.any?(shadow, &(&1 =~ value))}

      true ->
        {:error, :invalid_condition}
    end
  end

  defp evaluate(expression, candidate) do
    expression
    |> put_in(
      [Sql.Query.Lenses.leaf_expressions() |> Lens.filter(&Sql.Expression.column?/1)],
      Sql.Expression.constant(:unknown, candidate)
    )
    |> Sql.Expression.const_value()
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
