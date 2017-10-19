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
  def compile(query = %{command: :show}), do: query
  def compile(query) do
    top_level_uid = find_top_level_uid!(query)

    query
    |> Helpers.apply_bottom_up(&calculate_base_noise_layers(&1, top_level_uid))
    |> apply_top_down(&push_down_noise_layers/1)
    |> Helpers.apply_bottom_up(&calculate_floated_noise_layers/1)
    |> apply_top_down(&normalize_datasource_case/1)
    |> add_generic_uid_layer_if_needed(top_level_uid)
  end


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

  defp push_noise_layer(query, %NoiseLayer{base: {_table, column, extras}, expressions: [_ | rest]}) do
    {:ok, expression} = find_column(column, query)

    layers =
      raw_columns(expression)
      |> Enum.map(fn(column) ->
        NoiseLayer.new({column.table.name, column.name, extras}, [set_unique_alias(column) | rest])
      end)

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

  defp add_db_columns(query), do:
    Helpers.add_extra_db_columns(query, noise_layer_columns(query))

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

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count]}, query), do:
    %NoiseLayer{noise_layer | expressions: [re_min(min, query), re_max(max, query), re_count(count, query)]}
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count, user_id]}, query), do:
    %NoiseLayer{noise_layer | expressions: [re_min(min, query), re_max(max, query), re_count(count, query), user_id]}
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [expression]}, query), do:
    if not aggregated_column?(query, expression),
      do: %NoiseLayer{noise_layer | expressions: [min(expression), max(expression), count(expression)]},
      else: noise_layer
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [expression, user_id]}, query), do:
    if not aggregated_column?(query, expression),
      do: %NoiseLayer{noise_layer | expressions: [min(expression), max(expression), count(expression), user_id]},
      else: noise_layer

  defp min(expression), do:
    Expression.function("min", [expression], expression.type, _aggregate = true) |> set_unique_alias()
  defp max(expression), do:
    Expression.function("max", [expression], expression.type, _aggregate = true) |> set_unique_alias()
  defp count(expression), do:
    Expression.function("count", [expression], :integer, _aggregate = true) |> set_unique_alias()

  defp re_min(min, query), do:
    Expression.function("min", [Helpers.reference_aliased(min, query)], min.type, _aggregate = true)
    |> set_unique_alias()
  defp re_max(max, query), do:
    Expression.function("max", [Helpers.reference_aliased(max, query)], max.type, _aggregate = true)
    |> set_unique_alias()
  defp re_count(count, query), do:
    Expression.function("sum", [Helpers.reference_aliased(count, query)], :integer, _aggregate = true)
    |> set_unique_alias()

  defp aggregated_column?(_query, %Expression{constant?: true}), do: true
  defp aggregated_column?(query, expression), do: Helpers.aggregated_column?(query, expression)


  # -------------------------------------------------------------------
  # Computing base noise layers
  # -------------------------------------------------------------------

  defp calculate_base_noise_layers(query = %{projected?: true}, _top_level_uid), do: query
  defp calculate_base_noise_layers(query, top_level_uid), do:
    %{query |
      noise_layers:
        select_noise_layers(query, top_level_uid) ++
        clear_noise_layers(query, top_level_uid) ++
        unclear_noise_layers(query, top_level_uid) ++
        like_noise_layers(query, top_level_uid) ++
        range_noise_layers(query) ++
        negative_noise_layers(query)
    }

  defp select_noise_layers(%{subquery?: true}, _top_level_uid), do: []
  defp select_noise_layers(query, top_level_uid), do:
    Lens.key(:columns)
    |> Lens.all()
    |> Lens.satisfy(& not aggregated_column?(query, &1))
    |> raw_columns(query)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, top_level_uid)])

  defp clear_noise_layers(query, top_level_uid), do:
    clear_conditions()
    |> Lens.to_list(query)
    |> Enum.flat_map(fn({:comparison, column, :=, constant}) ->
      [
        static_noise_layer(column, constant),
        uid_noise_layer(column, constant, top_level_uid),
      ]
    end)

  defp unclear_noise_layers(query, top_level_uid), do:
    unclear_conditions()
    |> raw_columns(query)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, top_level_uid)])

  defp range_noise_layers(%{ranges: ranges}), do:
    Enum.flat_map(ranges, fn(%{column: column, interval: range}) ->
      raw_columns(column)
      |> Enum.map(&static_noise_layer(&1, &1, range))
    end)

  defp negative_noise_layers(query), do:
    conditions_satisfying(&Condition.not_equals?(&1))
    |> raw_columns(query)
    |> Enum.map(&static_noise_layer(&1, &1, :<>))


  # -------------------------------------------------------------------
  # LIKE noise layers
  # -------------------------------------------------------------------

  defp like_noise_layers(query, top_level_uid), do:
    conditions_satisfying(&(Condition.like?(&1) or Condition.not_like?(&1)))
    |> Lens.to_list(query)
    |> Enum.flat_map(&do_like_noise_layers(&1, top_level_uid))

  defp do_like_noise_layers({kind, column, %{constant?: true, value: pattern}}, top_level_uid), do:
    column
    |> raw_columns()
    |> Enum.flat_map(fn(column) ->
      [
        static_noise_layer(column, column) |
        pattern |> like_layer_keys() |> Enum.map(&uid_noise_layer(column, column, top_level_uid, {kind, &1}))
      ]
    end)
  defp do_like_noise_layers({:not, {kind, column, %{constant?: true, value: pattern}}}, top_level_uid), do:
    column
    |> raw_columns()
    |> Enum.flat_map(fn(column) ->
      [
        static_noise_layer(column, column, {:not, kind, without_percents(pattern)}) |
        pattern |> like_layer_keys() |> Enum.map(&uid_noise_layer(column, column, top_level_uid, {:not, kind, &1}))
      ]
    end)

  defp without_percents(like_pattern), do:
    like_pattern |> LikePattern.graphemes() |> Enum.reject(& &1 == :%) |> Enum.join()

  defp like_layer_keys(like_pattern) do
    len = like_pattern |> without_percents() |> String.length()
    like_layer_keys(LikePattern.graphemes(like_pattern), 0, len)
  end

  defp like_layer_keys([], _, _), do: []
  defp like_layer_keys([:% | rest], n, len), do: [{:%, len, n} | like_layer_keys(rest, n, len)]
  defp like_layer_keys([:_ | rest], n, len), do: [{:_, len, n} | like_layer_keys(rest, n + 1, len)]
  defp like_layer_keys([_ | rest], n, len), do: like_layer_keys(rest, n + 1, len)


  # -------------------------------------------------------------------
  # UID handling
  # -------------------------------------------------------------------

  defp add_generic_uid_layer_if_needed(query = %{noise_layers: []}, top_level_uid), do:
    %{query | noise_layers: [NoiseLayer.new(nil, [top_level_uid])]}
  defp add_generic_uid_layer_if_needed(query, _top_level_uid), do: query

  defp find_top_level_uid!(query) do
    id_column = Enum.find(query.db_columns, & &1.user_id?)
    false = is_nil(id_column)
    id_column
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

  defp uid_noise_layer(base_column, layer_expression, top_level_uid, extras \\ nil), do:
    NoiseLayer.new(
      {base_column.table.name, base_column.name, extras},
      [set_unique_alias(layer_expression), top_level_uid]
    )

  defp static_noise_layer(base_column, layer_expression, extras \\ nil), do:
    NoiseLayer.new(
      {base_column.table.name, base_column.name, extras},
      [set_unique_alias(layer_expression)]
    )

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

  deflensp clear_conditions(), do:
    Lens.satisfy(basic_conditions(), &clear_condition?/1)

  deflensp unclear_conditions(), do:
    Lens.satisfy(basic_conditions(), & not clear_condition?(&1))

  deflensp basic_conditions(), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(& not Condition.inequality?(&1))
    |> Lens.satisfy(& not Condition.not_equals?(&1))
    |> Lens.satisfy(& not Condition.not_like?(&1))
    |> Lens.satisfy(& not Condition.like?(&1))
    |> Lens.satisfy(& not fk_pk_condition?(&1))
    |> Lens.both(Lens.key(:group_by))

  deflensp non_uid_expressions(), do:
    Lens.all()
    |> Lens.key(:expressions)
    |> Lens.all()
    |> Lens.satisfy(& not &1.constant?)
    |> Lens.satisfy(& not &1.user_id?)

  defp clear_condition?({:comparison,
    %Expression{user_id?: false, function?: false, constant?: false}, :=, %Expression{constant?: true}}), do: true
  defp clear_condition?(_), do: false

  defp fk_pk_condition?({:comparison, lhs, :=, rhs}), do:
    Expression.key?(lhs) and Expression.key?(rhs)
  defp fk_pk_condition?(_), do: false

  # Modifies the expression to have a globally unique alias. This serves to make sure a column being added to the query
  # doesn't accidentally clash with a column selected by the user or a user-defined alias.
  defp set_unique_alias(column), do: %{column | alias: "__ac_alias__#{System.unique_integer([:positive])}"}
end
