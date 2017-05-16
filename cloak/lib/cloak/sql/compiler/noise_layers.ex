defmodule Cloak.Sql.Compiler.NoiseLayers do
  alias Cloak.Sql.{Expression, Query, NoiseLayer}
  alias Cloak.Sql.Compiler.Helpers

  def compile(query, data_source) do
    query
    |> update_in([Query.Lenses.direct_subqueries() |> Lens.key(:ast)], &compile(&1, data_source))
    |> calculate_noise_layers()
    |> add_db_columns()
    |> float_emulated_noise_layers()
  end

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

  defp calculate_noise_layers(query = %{projected?: true}), do: query
  defp calculate_noise_layers(query = %{subquery?: true}), do:
    if Helpers.aggregate?(query),
      do: %{query | noise_layers: query |> noise_layers() |> Enum.map(&float_noise_layer(&1, query))},
      else: %{query | noise_layers: query |> noise_layers()}
  defp calculate_noise_layers(query), do:
    %{query | noise_layers: query |> noise_layers()}

  defp noise_layers(query), do: new_noise_layers(query) ++ floated_noise_layers(query)

  defp new_noise_layers(query), do:
    Query.Lenses.filter_clauses()
    |> Lens.both(Lens.key(:group_by))
    |> Query.Lenses.leaf_expressions()
    |> raw_columns()
    |> Lens.to_list(query)
    |> Enum.flat_map(&resolve_row_splitter(&1, query))
    |> Enum.map(&NoiseLayer.new({&1.table.name, &1.name}, [Helpers.set_unique_alias(&1)]))

  defp resolve_row_splitter(expression, %{row_splitters: row_splitters}) do
    if splitter = Enum.find(row_splitters, &(&1.row_index == expression.row_index)) do
      Query.Lenses.leaf_expressions()
      |> raw_columns()
      |> Lens.to_list([splitter.function_spec])
    else
      [expression]
    end
  end

  defp raw_columns(lens), do:
    Lens.satisfy(lens, &match?(%Expression{user_id?: false, constant?: false, function?: false}, &1))

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
