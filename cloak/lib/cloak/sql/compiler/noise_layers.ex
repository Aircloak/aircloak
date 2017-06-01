defmodule Cloak.Sql.Compiler.NoiseLayers do
  @moduledoc "Contains functions related to compilation of noise layers."

  alias Cloak.Sql.{Expression, Query, NoiseLayer, Comparison}
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
    |> apply_bottom_up(&calculate_base_noise_layers/1)
    |> apply_top_down(&push_down_noise_layers/1)
    |> apply_bottom_up(&calculate_floated_noise_layers/1)


  # -------------------------------------------------------------------
  # Internal functions
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

  defp push_noise_layer(query, %{base: {_table, column, extras}}) do
    index = Enum.find_index(query.column_titles, &(&1 == column))
    true = index < length(query.columns)
    expression = Enum.at(query.columns, index)

    layers =
      raw_columns()
      |> Lens.to_list([expression])
      |> Enum.flat_map(&resolve_row_splitter(&1, query))
      |> Enum.map(&NoiseLayer.new({&1.table.name, &1.name, extras}, [Helpers.set_unique_alias(&1)]))

    update_in(query, [Lens.key(:noise_layers)], &(&1 ++ layers))
  end

  defp calculate_floated_noise_layers(query), do:
    query
    |> add_floated_noise_layers()
    |> add_db_columns()
    |> float_emulated_noise_layers()

  defp add_floated_noise_layers(query), do:
    if query.subquery? && Helpers.aggregate?(query),
      do: %{query | noise_layers: float_noise_layers(query.noise_layers ++ floated_noise_layers(query), query)},
      else: %{query | noise_layers: query.noise_layers ++ floated_noise_layers(query)}

  defp float_noise_layers(layers, query), do:
    Enum.map(layers, &float_noise_layer(&1, query))

  defp apply_top_down(query, function), do:
    query
    |> function.()
    |> update_in([Query.Lenses.direct_subqueries() |> Lens.key(:ast)], &apply_top_down(&1, function))

  defp apply_bottom_up(query, function), do:
    query
    |> update_in([Query.Lenses.direct_subqueries() |> Lens.key(:ast)], &apply_bottom_up(&1, function))
    |> function.()

  defp float_emulated_noise_layers(query = %{emulated?: true, subquery?: true}) do
    noise_columns = get_in(query.noise_layers, [Lens.all() |> Lens.key(:expressions) |> Lens.all()]) -- query.columns

    %{
      query |
      columns: query.columns ++ noise_columns,
      column_titles: query.column_titles ++ Enum.map(noise_columns, &(&1.alias || &1.name)),
      aggregators: query.aggregators ++ Enum.filter(noise_columns, &(&1.aggregate?)),
    }
  end
  defp float_emulated_noise_layers(query), do: query

  defp add_db_columns(query) do
    noise_columns = noise_layer_columns(query)
    {query, noise_columns} = Helpers.drop_redundant_floated_columns(query, query.db_columns, noise_columns)
    Enum.reduce(noise_columns, query, &Query.add_db_column(&2, &1))
  end

  defp noise_layer_columns(%{noise_layers: noise_layers, emulated?: true, subquery?: true}), do:
    Enum.flat_map(noise_layers, &(&1.expressions)) |> Enum.map(fn
      %{aggregate?: true, function_args: [aggregated]} -> aggregated
      column -> column
    end)
  defp noise_layer_columns(%{noise_layers: noise_layers}), do:
    Enum.flat_map(noise_layers, &(&1.expressions))

  defp calculate_base_noise_layers(query = %{projected?: true}), do:
    query
  defp calculate_base_noise_layers(query), do:
    %{query | noise_layers: range_noise_layers(query) ++ non_range_noise_layers(query)}

  defp non_range_noise_layers(query), do:
    Query.Lenses.filter_clauses()
    |> Lens.both(Lens.key(:group_by))
    |> Lens.all()
    |> Lens.satisfy(& not Comparison.inequality?(&1))
    |> raw_columns()
    |> Lens.to_list(query)
    |> Enum.flat_map(&resolve_row_splitter(&1, query))
    |> Enum.map(&NoiseLayer.new({&1.table.name, &1.name, nil}, [Helpers.set_unique_alias(&1)]))

  defp range_noise_layers(%{ranges: ranges}), do:
    Enum.flat_map(ranges, fn(%{column: column, interval: range}) ->
      raw_columns()
      |> Lens.to_list(column)
      |> Enum.map(&NoiseLayer.new({&1.table.name, &1.name, range}, [Helpers.set_unique_alias(&1)]))
    end)

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

  defp floated_noise_layers(query), do:
    Query.Lenses.subquery_noise_layers()
    |> Lens.to_list(query)
    |> update_in([Lens.all() |> Lens.key(:expressions) |> Lens.all()], &reference_aliased/1)

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count]}, _query) do
    %{noise_layer | expressions:
      [
        Expression.function("min", [reference_aliased(min)], min.type, _aggregate = true),
        Expression.function("max", [reference_aliased(max)], max.type, _aggregate = true),
        Expression.function("sum", [reference_aliased(count)], :integer, _aggregate = true),
      ]
      |> Enum.map(&Helpers.set_unique_alias/1)
    }
  end
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [expression]}, query) do
    if not Helpers.aggregated_column?(query, Expression.unalias(expression)) do
      %{noise_layer | expressions:
        [
          # The point of this unalias is not to generate invalid SQL like `min(foo AS carry_1234)`
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

  defp reference_aliased(column), do: %Expression{name: column.alias || column.name}
end
