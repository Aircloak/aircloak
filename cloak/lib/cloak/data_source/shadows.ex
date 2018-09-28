defmodule Cloak.DataSource.Shadows do
  alias Cloak.Sql

  require Aircloak

  @cache_module Aircloak.in_env(test: Cloak.TestShadowCache, else: Cloak.DataSource.Shadows.Query)

  def safe?(condition, query) do
    if condition |> Sql.Condition.targets() |> Enum.any?(&Sql.Expression.constant?/1) |> :erlang.not() do
      {:ok, true}
    else
      do_safe?(condition, query)
    end
  end

  defp do_safe?(condition, query) do
    expression = condition |> Sql.Condition.subject() |> expand_expression(query)

    case columns(expression) do
      [] ->
        {:ok, true}

      [{table, column}] ->
        shadow = @cache_module.build_shadow(query.data_source, table, column) |> Stream.map(&evaluate(expression, &1))
        any?(condition, shadow)

      _ ->
        {:error, :multiple_columns}
    end
  end

  defp columns(expression) do
    expression
    |> get_in([Sql.Query.Lenses.leaf_expressions() |> Lens.filter(&Sql.Expression.column?/1)])
    |> Enum.map(&{&1.table.name, &1.name})
    |> Enum.uniq()
  end

  defp expand_expression(expression, query) do
    update_in(expression, [Sql.Query.Lenses.leaf_expressions()], fn expression ->
      case Sql.Query.resolve_subquery_column(expression, query) do
        :database_column -> expression
        {column, subquery} -> expand_expression(column, subquery)
      end
    end)
  end

  def any?(condition, shadow) do
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
      Sql.Expression.constant(:ignored, candidate)
    )
    |> Sql.Expression.const_value()
  end

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_) do
    children = Aircloak.in_env(test: [@cache_module], else: [])
    Aircloak.ChildSpec.supervisor(children, strategy: :one_for_one, name: __MODULE__)
  end
end
