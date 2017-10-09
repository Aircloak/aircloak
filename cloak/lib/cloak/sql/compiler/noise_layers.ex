defmodule Cloak.Sql.Compiler.NoiseLayers do
  @moduledoc "Contains functions related to compilation of noise layers."

  alias Cloak.Sql.{Expression, Query, NoiseLayer, Condition}
  alias Cloak.Sql.Compiler.Helpers

  use Lens.Macros


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Fills in the noise_layers for the given query. Furthermore, it modifies the query to float any data that will be
  needed to compute those noise layers to the top level.
  """
  @spec compile(Query.t) :: Query.t
  def compile(query = %{command: :show}), do: query
  def compile(query), do:
    query
    |> Helpers.apply_bottom_up(&calculate_base_noise_layers/1)
    |> apply_top_down(&push_down_noise_layers/1)
    |> Helpers.apply_bottom_up(&calculate_floated_noise_layers/1)
    |> apply_top_down(&normalize_datasource_case/1)
    |> update_uid_expressions()
    |> add_uid_layer()


  # -------------------------------------------------------------------
  # Pushing layers into subqueries
  # -------------------------------------------------------------------

  defp push_down_noise_layers(query), do:
    Enum.reduce(query.noise_layers, query, fn(noise_layer, query) ->
      case subquery_for_noise_layer(noise_layer) |> Lens.to_list(query) do
        [] -> query
        [_] ->
          query
          |> update_in([subquery_for_noise_layer(noise_layer)], &push_noise_layer(&1, noise_layer))
          |> update_in([Lens.key(:noise_layers)], &(&1 -- [noise_layer]))
      end
    end)

  defp subquery_for_noise_layer(%{base: {table, _column, _extras}}), do:
    Query.Lenses.direct_subqueries()
    |> Lens.satisfy(&(&1.alias == table))
    |> Lens.key(:ast)
    |> Lens.satisfy(&(not &1.projected?))

  defp push_noise_layer(query, %{base: {_table, column, extras}}) do
    {:ok, expression} = find_column(column, query)

    layers =
      raw_columns(expression)
      |> Enum.map(&build_noise_layer(&1, extras))

    update_in(query, [Lens.key(:noise_layers)], &(&1 ++ layers))
  end


  # -------------------------------------------------------------------
  # Floating noise layers and columns
  # -------------------------------------------------------------------

  defp calculate_floated_noise_layers(query), do:
    query
    |> add_floated_noise_layers()
    |> add_floated_db_columns()
    |> float_noise_layers_columns()

  defp add_floated_noise_layers(query), do:
    if query.subquery? && Helpers.aggregate?(query),
      do: %{query | noise_layers: float_noise_layers(query.noise_layers ++ floated_noise_layers(query), query)},
      else: %{query | noise_layers: query.noise_layers ++ floated_noise_layers(query)}

  defp float_noise_layers(layers, query), do:
    Enum.map(layers, &float_noise_layer(&1, query))

  defp float_noise_layers_columns(query = %{subquery?: true}) do
    noise_columns =
      non_uid_expressions()
      |> Lens.to_list(query.noise_layers)
      |> Enum.reject(& &1 in query.columns)
    %{
      query |
      columns: query.columns ++ noise_columns,
      column_titles: query.column_titles ++ Enum.map(noise_columns, &(&1.alias || &1.name)),
      aggregators: query.aggregators ++ Enum.filter(noise_columns, &(&1.aggregate?)),
    }
  end
  defp float_noise_layers_columns(query), do: query

  defp add_floated_db_columns(query), do:
    add_db_columns(query, noise_layer_columns(query))

  defp noise_layer_columns(%{noise_layers: noise_layers, emulated?: true}), do:
    non_uid_expressions()
    |> Lens.to_list(noise_layers)
    |> Enum.map(fn
      %{aggregate?: true, function_args: [aggregated]} -> aggregated
      column -> column
    end)
  defp noise_layer_columns(%{noise_layers: noise_layers}), do:
    Lens.to_list(non_uid_expressions(), noise_layers)

  defp floated_noise_layers(query), do:
    Query.Lenses.direct_subqueries()
    |> Lens.to_list(query)
    |> Enum.flat_map(fn (%{ast: subquery, alias: alias}) ->
      subquery_table = Enum.find(query.selected_tables, & &1.name == alias)
      true = subquery_table != nil

      Lens.map(non_uid_expressions(), subquery.noise_layers, &Helpers.reference_aliased(&1, subquery, subquery_table))
    end)

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count]}, query) do
    %{noise_layer | expressions:
      [
        Expression.function("min", [Helpers.reference_aliased(min, query)], min.type, _aggregate = true),
        Expression.function("max", [Helpers.reference_aliased(max, query)], max.type, _aggregate = true),
        Expression.function("sum", [Helpers.reference_aliased(count, query)], :integer, _aggregate = true),
      ]
      |> Enum.map(&Helpers.set_unique_alias/1)
    }
  end
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count, user_id]}, query) do
    %{noise_layer | expressions:
      [
        Expression.function("min", [Helpers.reference_aliased(min, query)], min.type, _aggregate = true),
        Expression.function("max", [Helpers.reference_aliased(max, query)], max.type, _aggregate = true),
        Expression.function("sum", [Helpers.reference_aliased(count, query)], :integer, _aggregate = true),
        user_id,
      ]
      |> Enum.map(&Helpers.set_unique_alias/1)
    }
  end
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [expression]}, query) do
    if not Helpers.aggregated_column?(query, Expression.unalias(expression)) do
      %{noise_layer | expressions:
        [
          # The point of this unalias is to not generate invalid SQL like `min(foo AS carry_1234)`
          Expression.function("min", [expression], expression.type, _aggregate = true),
          Expression.function("max", [expression], expression.type, _aggregate = true),
          Expression.function("count", [expression], :integer, _aggregate = true),
        ]
        |> Enum.map(&Helpers.set_unique_alias/1)
      }
    else
      noise_layer
    end
  end
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [expression, user_id]}, query) do
    if not Helpers.aggregated_column?(query, Expression.unalias(expression)) do
      %{noise_layer | expressions:
        [
          # The point of this unalias is to not generate invalid SQL like `min(foo AS carry_1234)`
          Expression.function("min", [expression], expression.type, _aggregate = true),
          Expression.function("max", [expression], expression.type, _aggregate = true),
          Expression.function("count", [expression], :integer, _aggregate = true),
          user_id,
        ]
        |> Enum.map(&Helpers.set_unique_alias/1)
      }
    else
      noise_layer
    end
  end


  # -------------------------------------------------------------------
  # Computing base noise layers
  # -------------------------------------------------------------------

  defp calculate_base_noise_layers(query = %{projected?: true}), do: query
  defp calculate_base_noise_layers(query), do:
    %{query |
      noise_layers:
        basic_noise_layers(query) ++
        range_noise_layers(query) ++
        negative_noise_layers(query)
    }

  defp basic_noise_layers(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(& not Condition.inequality?(&1))
    |> Lens.satisfy(& not Condition.not_equals?(&1))
    |> Lens.satisfy(& not Condition.not_like?(&1))
    |> Lens.satisfy(& not fk_pk_condition?(&1))
    |> Lens.both(Lens.key(:group_by))
    |> raw_columns(query)
    |> Enum.flat_map(& [static_noise_layer(&1), uid_noise_layer(&1)])

  defp fk_pk_condition?({:comparison, lhs, :=, rhs}), do:
    Expression.key?(lhs) and Expression.key?(rhs)
  defp fk_pk_condition?(_), do: false

  defp range_noise_layers(%{ranges: ranges}), do:
    Enum.flat_map(ranges, fn(%{column: column, interval: range}) ->
      raw_columns(column)
      |> Enum.map(&build_noise_layer(&1, range))
    end)

  defp negative_noise_layers(query), do:
    conditions_satisfying(&(Condition.not_equals?(&1) or Condition.not_like?(&1)))
    |> raw_columns(query)
    |> Enum.map(&build_noise_layer(&1, :<>))


  # -------------------------------------------------------------------
  # Finalization
  # -------------------------------------------------------------------

  defp add_uid_layer(query = %{noise_layers: []}) do
    id_column = Helpers.id_column(query)
    uid_noise_layer = NoiseLayer.new(nil, [Helpers.id_column(query)])

    %{query | noise_layers: [uid_noise_layer]}
    |> add_db_columns([id_column])
  end
  defp add_uid_layer(query), do: query

  defp update_uid_expressions(query) do
    Lens.key(:noise_layers)
    |> uid_expressions()
    |> Lens.map(query, fn(_) -> Helpers.id_column(query) end)
  end


  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp find_column(name, query) do
    case Enum.find_index(query.column_titles, &(&1 == name)) do
      nil -> :error
      index ->
        true = index < length(query.columns)
        {:ok, Enum.at(query.columns, index)}
    end
  end

  defp apply_top_down(query, function), do:
    query
    |> function.()
    |> update_in([Query.Lenses.direct_subqueries() |> Lens.key(:ast)], &apply_top_down(&1, function))

  defp raw_columns(lens \\ Lens.root(), data), do:
    lens
    |> Query.Lenses.leaf_expressions()
    |> Lens.satisfy(&match?(%Expression{user_id?: false, constant?: false, function?: false}, &1))
    |> Lens.to_list(data)

  defp static_noise_layer(column), do: build_noise_layer(column)

  defp uid_noise_layer(column), do:
    NoiseLayer.new(
      {column.table.name, column.name, _extras = nil},
      [Helpers.set_unique_alias(column), %Expression{user_id?: true}]
    )

  defp build_noise_layer(column, extras \\ nil), do:
    NoiseLayer.new({column.table.name, column.name, extras}, [Helpers.set_unique_alias(column)])

  defp conditions_satisfying(predicate), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(predicate)

  defp normalize_datasource_case(query) do
    Lens.key(:noise_layers)
    |> Lens.all()
    |> Lens.key(:base)
    |> Lens.map(query, fn({table, column, extras}) ->
      {String.downcase(table), String.downcase(column), extras}
    end)
  end

  defp add_db_columns(query, to_add) do
    {query, to_add} = Helpers.drop_redundant_floated_columns(query, query.db_columns, to_add)
    Enum.reduce(to_add, query, &Query.add_db_column(&2, &1))
  end

  deflensp non_uid_expressions() do
    expressions() |> Lens.satisfy(& not &1.user_id?)
  end

  deflensp uid_expressions() do
    expressions() |> Lens.satisfy(& &1.user_id?)
  end

  deflensp expressions() do
    Lens.all()
    |> Lens.key(:expressions)
    |> Lens.all()
  end
end
