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
  def compile(query), do:
    query
    |> Helpers.apply_bottom_up(&calculate_base_noise_layers/1)
    |> Helpers.apply_top_down(&push_down_noise_layers/1)
    |> Helpers.apply_bottom_up(&calculate_floated_noise_layers/1)
    |> Helpers.apply_top_down(&normalize_noise_layers_base/1)


  @allowed_not_equals_functions ~w(lower)
  @doc "Returns true if a condition is anonymized by noise layers."
  @spec can_be_anonymized_with_noise_layer?(Query.where_clause, Query.t) :: boolean
  def can_be_anonymized_with_noise_layer?({:not, {like, left, right}}, query) when like in [:like, :ilike], do:
    not processed_column?(left, query) and Expression.constant?(right)
  def can_be_anonymized_with_noise_layer?(
    {:comparison, %Expression{function?: true, function: name, function_args: [arg]}, :<>, right}, query), do:
      not processed_column?(arg, query) and
        Enum.member?(@allowed_not_equals_functions, name) and
        Expression.constant?(right)
  def can_be_anonymized_with_noise_layer?({:comparison, left, :<>, right}, query), do:
    not processed_column?(left, query) and Expression.constant?(right)
  def can_be_anonymized_with_noise_layer?({:not, condition}, _query), do:
    Condition.inequality?(condition) or Condition.verb(condition) == :is
  def can_be_anonymized_with_noise_layer?(_condition, _query), do: true


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
        not_equals_noise_layers(query) ++
        not_like_noise_layers(query) ++
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
    |> Lens.satisfy(&can_be_anonymized_with_noise_layer?(&1, query))
    |> Lens.both(Lens.key(:group_by))
    |> raw_columns()
    |> Lens.to_list(query)
    |> Enum.flat_map(&resolve_row_splitter(&1, query))
    |> Enum.map(&build_noise_layer/1)

  defp range_noise_layers(%{ranges: ranges}), do:
    Enum.flat_map(ranges, fn(%{column: column, interval: range}) ->
      raw_columns()
      |> Lens.to_list(column)
      |> Enum.map(&build_noise_layer(&1, range))
    end)

  defp not_equals_noise_layers(query), do:
    query
    |> conditions_satisfying(&Condition.not_equals?/1)
    |> Enum.map(&not_equals_noise_layer/1)

  defp not_like_noise_layers(query), do:
    query
    |> conditions_satisfying(&Condition.not_like?/1)
    |> Enum.map(fn({:not, {kind, column, constant}}) ->
      build_noise_layer(column, {:not, kind, Expression.value(constant, [])})
    end)

  defp in_noise_layers(query), do:
    query
    |> conditions_satisfying(&Condition.in?/1)
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
    query
    |> conditions_satisfying(&Condition.like?/1)
    |> Enum.flat_map(fn({kind, column, constant}) ->
      columns = Lens.to_list(raw_columns(), column)
      layer_keys = constant |> Expression.value([]) |> like_layer_keys

      for layer_key <- layer_keys, column <- columns do
        build_noise_layer(column, {kind, layer_key})
      end
    end)

  defp like_layer_keys(like_pattern) do
    len = like_pattern |> String.replace("%", "") |> String.length()
    like_layer_keys(String.graphemes(like_pattern), 0, len)
  end

  defp like_layer_keys([], _, _), do: []
  defp like_layer_keys(["%" | rest], n, len), do: [{:%, len, n} | like_layer_keys(rest, n, len)]
  defp like_layer_keys(["_" | rest], n, len), do: [{:_, len, n} | like_layer_keys(rest, n + 1, len)]
  defp like_layer_keys([_ | rest], n, len), do: like_layer_keys(rest, n + 1, len)

  defp not_equals_noise_layer(
    {:comparison, %Expression{function?: true, function: name, function_args: [column]}, :<>, constant}
  ) do
    build_noise_layer(column, {:<>, name, Expression.value(constant, [])})
  end
  defp not_equals_noise_layer({:comparison, column, :<>, constant}) do
    build_noise_layer(column, {:<>, Expression.value(constant, [])})
  end

  defp processed_column?(:*, _query), do: false
  defp processed_column?(%Expression{function?: true}, _query), do: true
  defp processed_column?(_expression, %{from: table}) when is_binary(table), do: false
  defp processed_column?(%Expression{name: name}, query) do
    get_in(query, [Query.Lenses.direct_subqueries()])
    |> Enum.any?(fn(%{ast: subquery}) ->
      case find_column(name, subquery) do
        {:ok, column} -> processed_column?(column, subquery)
        _ -> false
      end
    end)
  end


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

  defp conditions_satisfying(query, predicate), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(predicate)
    |> Lens.satisfy(&can_be_anonymized_with_noise_layer?(&1, query))
    |> Lens.to_list(query)

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
