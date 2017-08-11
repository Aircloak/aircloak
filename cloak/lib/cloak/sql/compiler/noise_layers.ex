defmodule Cloak.Sql.Compiler.NoiseLayers do
  @moduledoc "Contains functions related to compilation of noise layers."

  alias Cloak.Sql.{Expression, Query, NoiseLayer, Condition, LikePattern}
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
  def compile(query), do:
    query
    |> Helpers.apply_bottom_up(&calculate_base_noise_layers/1)
    |> apply_top_down(&push_down_noise_layers/1)
    |> Helpers.apply_bottom_up(&calculate_floated_noise_layers/1)
    |> apply_top_down(&normalize_noise_layers_base/1)


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
      raw_columns()
      |> Lens.to_list([expression])
      |> Enum.flat_map(&resolve_row_splitter(&1, query))
      |> Enum.map(&build_noise_layer(&1, extras))

    update_in(query, [Lens.key(:noise_layers)], &(&1 ++ layers))
  end


  # -------------------------------------------------------------------
  # Floating noise layers and columns
  # -------------------------------------------------------------------

  defp calculate_floated_noise_layers(query), do:
    query
    |> add_floated_noise_layers()
    |> add_db_columns()
    |> float_noise_layers_columns()

  defp add_floated_noise_layers(query), do:
    if query.subquery? && Helpers.aggregate?(query),
      do: %{query | noise_layers: float_noise_layers(query.noise_layers ++ floated_noise_layers(query), query)},
      else: %{query | noise_layers: query.noise_layers ++ floated_noise_layers(query)}

  defp float_noise_layers(layers, query), do:
    Enum.map(layers, &float_noise_layer(&1, query))

  defp float_noise_layers_columns(query = %{subquery?: true}) do
    noise_columns =
      Lens.all()
      |> Lens.key(:expressions)
      |> Lens.all()
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

  defp add_db_columns(query) do
    noise_columns = noise_layer_columns(query)
    {query, noise_columns} = Helpers.drop_redundant_floated_columns(query, query.db_columns, noise_columns)
    Enum.reduce(noise_columns, query, &Query.add_db_column(&2, &1))
  end

  defp noise_layer_columns(%{noise_layers: noise_layers, emulated?: true}), do:
    Enum.flat_map(noise_layers, &(&1.expressions)) |> Enum.map(fn
      %{aggregate?: true, function_args: [aggregated]} -> aggregated
      column -> column
    end)
  defp noise_layer_columns(%{noise_layers: noise_layers}), do:
    Enum.flat_map(noise_layers, &(&1.expressions))

  defp floated_noise_layers(query), do:
    Query.Lenses.direct_subqueries()
    |> Lens.to_list(query)
    |> Enum.flat_map(fn (%{ast: subquery, alias: alias}) ->
      subquery_table = Enum.find(query.selected_tables, & &1.name == alias)
      true = subquery_table != nil
      Lens.all() |> Lens.key(:expressions) |> Lens.all()
      |> Lens.map(subquery.noise_layers, &reference_aliased(&1, subquery, subquery_table))
    end)

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count]}, query) do
    %{noise_layer | expressions:
      [
        Expression.function("min", [reference_aliased(min, query)], min.type, _aggregate = true),
        Expression.function("max", [reference_aliased(max, query)], max.type, _aggregate = true),
        Expression.function("sum", [reference_aliased(count, query)], :integer, _aggregate = true),
      ]
      |> Enum.map(&Helpers.set_unique_alias/1)
    }
  end
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [expression]}, query) do
    if not Helpers.aggregated_column?(query, Expression.unalias(expression)) do
      %{noise_layer | expressions:
        [
          # The point of this unalias is to not generate invalid SQL like `min(foo AS carry_1234)`
          Expression.function("min", [Expression.unalias(expression)], expression.type, _aggregate = true),
          Expression.function("max", [Expression.unalias(expression)], expression.type, _aggregate = true),
          Expression.function("count", [Expression.unalias(expression)], :integer, _aggregate = true),
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

  defp calculate_base_noise_layers(query = %{projected?: true}), do:
    query
  defp calculate_base_noise_layers(query), do:
    %{query |
      noise_layers:
        basic_noise_layers(query) ++
        range_noise_layers(query) ++
        negative_noise_layers(query) ++
        like_noise_layers(query) ++
        in_noise_layers(query)
    }

  defp basic_noise_layers(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(& not Condition.inequality?(&1))
    |> Lens.satisfy(& not Condition.not_equals?(&1))
    |> Lens.satisfy(& not Condition.not_like?(&1))
    |> Lens.satisfy(& not Condition.like?(&1))
    |> Lens.satisfy(& not Condition.in?(&1))
    |> Lens.satisfy(& not fk_pk_condition?(&1))
    |> Lens.both(Lens.key(:group_by))
    |> raw_columns()
    |> Lens.to_list(query)
    |> Enum.flat_map(&resolve_row_splitter(&1, query))
    |> Enum.map(&build_noise_layer/1)

  defp fk_pk_condition?({:comparison, lhs, :=, rhs}), do:
    Expression.key?(lhs) and Expression.key?(rhs)
  defp fk_pk_condition?(_), do: false

  defp range_noise_layers(%{ranges: ranges}), do:
    Enum.flat_map(ranges, fn(%{column: column, interval: range}) ->
      raw_columns()
      |> Lens.to_list(column)
      |> Enum.map(&build_noise_layer(&1, range))
    end)

  defp negative_noise_layers(query), do:
    conditions_satisfying(&(Condition.not_equals?(&1) or Condition.not_like?(&1)))
    |> raw_columns()
    |> Lens.to_list(query)
    |> Enum.map(&build_noise_layer(&1, :<>))

  defp in_noise_layers(query), do:
    conditions_satisfying(&Condition.in?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(fn({:in, column, constants}) ->
      column
      |> get_in([raw_columns()])
      |> Enum.flat_map(&resolve_row_splitter(&1, query))
      |> Enum.flat_map(fn(column) ->
        [
          build_noise_layer(column) |
          Enum.map(constants, &build_noise_layer(column, {:in, Expression.value(&1, [])}))
        ]
      end)
    end)

  defp like_noise_layers(query), do:
    conditions_satisfying(&Condition.like?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(fn({kind, column, constant}) ->
      columns = Lens.to_list(raw_columns(), column)
      layer_keys = like_layer_keys(constant.value)

      for layer_key <- layer_keys, column <- columns do
        build_noise_layer(column, {kind, layer_key})
      end
    end)

  defp like_layer_keys(like_pattern) do
    len = like_pattern |> LikePattern.graphemes() |> Enum.reject(& &1 == :%) |> length()
    like_layer_keys(LikePattern.graphemes(like_pattern), 0, len)
  end

  defp like_layer_keys([], _, _), do: []
  defp like_layer_keys([:% | rest], n, len), do: [{:%, len, n} | like_layer_keys(rest, n, len)]
  defp like_layer_keys([:_ | rest], n, len), do: [{:_, len, n} | like_layer_keys(rest, n + 1, len)]
  defp like_layer_keys([_ | rest], n, len), do: like_layer_keys(rest, n + 1, len)


  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp reference_aliased(column, query, table \\ :unknown), do:
    %Expression{name: column.alias || find_alias(column, query) || column.name, table: table}

  defp find_column(name, query) do
    case Enum.find_index(query.column_titles, &(&1 == name)) do
      nil -> :error
      index ->
        true = index < length(query.columns)
        {:ok, Enum.at(query.columns, index)}
    end
  end

  defp find_alias(column, query) do
    id = Expression.id(column)
    case Enum.find_index(query.columns, &Expression.id(&1) == id) do
      nil -> nil
      index ->
        true = index < length(query.column_titles)
        Enum.at(query.column_titles, index)
    end
  end

  defp apply_top_down(query, function), do:
    query
    |> function.()
    |> update_in([Query.Lenses.direct_subqueries() |> Lens.key(:ast)], &apply_top_down(&1, function))

  defp resolve_row_splitter(expression, %{row_splitters: row_splitters}) do
    if splitter = Enum.find(row_splitters, &(&1.row_index == expression.row_index)) do
      Lens.to_list(raw_columns(), [splitter.function_spec])
    else
      [expression]
    end
  end

  deflensp raw_columns(), do:
    Query.Lenses.leaf_expressions()
    |> Lens.satisfy(&match?(%Expression{user_id?: false, constant?: false, function?: false}, &1))

  defp build_noise_layer(column, extras \\ nil), do:
    NoiseLayer.new({column.table.name, column.name, extras}, [Helpers.set_unique_alias(column)])

  defp conditions_satisfying(predicate), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(predicate)

  # In order to ensure noise consistency across data sources that have columns with different cases,
  # we normalize the noise layer base by converting the column name to lower case.
  defp normalize_noise_layers_base(query) do
    Lens.key(:noise_layers)
    |> Lens.all()
    |> Lens.key(:base)
    |> Lens.map(query, fn({table, column, extras}) ->
      {String.downcase(table), String.downcase(column), extras}
    end)
  end
end
