defmodule Cloak.Sql.Compiler.NoiseLayers do
  @moduledoc "Contains functions related to compilation of noise layers."

  alias Cloak.Sql.{Expression, Query, NoiseLayer, Condition, LikePattern, Range}
  alias Cloak.Sql.Compiler.Helpers

  use Lens.Macros

  @noise_layer_alias_fix_part "__ac_nlc__"


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
    top_level_uid = Helpers.id_column(query)

    query
    |> Helpers.apply_bottom_up(&calculate_base_noise_layers(&1, top_level_uid))
    |> Helpers.apply_top_down(&push_down_noise_layers/1)
    |> Helpers.apply_bottom_up(&calculate_floated_noise_layers/1)
    |> Helpers.apply_top_down(&normalize_datasource_case/1)
    |> remove_meaningless_negative_noise_layers()
    |> add_generic_uid_layer_if_needed(top_level_uid)
  end

  @doc "Returns the columns required to compute the noise layers for the specified query."
  @spec noise_layer_columns(Query.t) :: [Expression.t]
  def noise_layer_columns(%{noise_layers: noise_layers, emulated?: true}), do:
    non_uid_expressions()
    |> Lens.to_list(noise_layers)
    |> Enum.map(fn
      %{aggregate?: true, function_args: [aggregated]} -> aggregated
      column -> column
    end)
    |> Enum.uniq_by(&Expression.unalias/1)
  def noise_layer_columns(%{noise_layers: noise_layers}), do:
    non_uid_expressions()
    |> Lens.to_list(noise_layers)
    |> Enum.uniq_by(&Expression.unalias/1)


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
    |> Lens.filter(&(&1.alias == table))
    |> Lens.key(:ast)
    |> Lens.reject(& &1.projected? or &1.virtual_table?)

  defp push_noise_layer(query, %NoiseLayer{base: {_table, column, extras}, expressions: [_min, _max | rest]}) do
    {:ok, expression} = find_column(column, query)

    layers =
      raw_columns(expression)
      |> Enum.map(fn(column) ->
        build_noise_layer(column, extras, [_min = column, _max = column | rest])
      end)
      |> drop_redundant_noise_layers_columns(query.columns)

    update_in(query, [Lens.key(:noise_layers)], &(&1 ++ layers))
  end


  # -------------------------------------------------------------------
  # Floating noise layers and columns
  # -------------------------------------------------------------------

  defp calculate_floated_noise_layers(query), do:
    query
    |> add_floated_noise_layers()
    |> float_noise_layers_columns()

  defp add_floated_noise_layers(query) do
    noise_layers =
      if query.subquery? && Helpers.aggregate?(query),
        do: float_noise_layers(query.noise_layers ++ floated_noise_layers(query), query),
        else: query.noise_layers ++ floated_noise_layers(query)
    %{query | noise_layers: drop_redundant_noise_layers_columns(noise_layers, query.columns)}
  end

  defp float_noise_layers(layers, query), do:
    Enum.map(layers, &float_noise_layer(&1, query))

  defp float_noise_layers_columns(query = %{subquery?: true}) do
    noise_columns =
      non_uid_expressions()
      |> Lens.to_list(query.noise_layers)
      |> Enum.reject(& &1 in query.columns)
      |> Enum.uniq()
    %{
      query |
      columns: query.columns ++ noise_columns,
      column_titles: query.column_titles ++ Enum.map(noise_columns, &(&1.alias || &1.name)),
      aggregators: query.aggregators ++ Enum.filter(noise_columns, &(&1.aggregate?)),
    }
  end
  defp float_noise_layers_columns(query), do: query

  defp floated_noise_layers(query), do:
    Query.Lenses.direct_subqueries()
    |> Lens.to_list(query)
    |> Enum.flat_map(fn (%{ast: subquery, alias: alias}) ->
      subquery_table = Enum.find(query.selected_tables, & &1.name == alias)
      true = subquery_table != nil

      Lens.map(non_uid_expressions(), subquery.noise_layers, &reference_aliased(&1, subquery, subquery_table))
    end)

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count]}, query), do:
    if not needs_aggregation?(query, min),
      do: %{noise_layer | expressions: [min_of_min(min), max_of_max(max), sum_of_count(min, count)]},
      else: noise_layer
  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count, user_id]}, query), do:
    if not needs_aggregation?(query, min),
      do: %{noise_layer | expressions: [min_of_min(min), max_of_max(max), sum_of_count(min, count), user_id]},
      else: noise_layer

  defp cast(expression, type), do: Expression.function({:cast, type}, [expression], type)

  defp min_of_min(%Expression{type: :boolean} = min), do:
    min |> cast(:integer) |> min_of_min() |> cast(:boolean)
  defp min_of_min(min), do:
    Expression.function("min", [Expression.unalias(min)], min.type, _aggregate = true)

  defp max_of_max(%Expression{type: :boolean} = max), do:
    max |> cast(:integer) |> max_of_max() |> cast(:boolean)
  defp max_of_max(max), do:
    Expression.function("max", [Expression.unalias(max)], max.type, _aggregate = true)

  defp sum_of_count(column, %Expression{value: 1}), do:
    Expression.function("count", [Expression.unalias(column)], :integer, _aggregate = true)
  defp sum_of_count(_column, count), do:
    Expression.function("sum", [Expression.unalias(count)], :integer, _aggregate = true)


  # -------------------------------------------------------------------
  # Computing base noise layers
  # -------------------------------------------------------------------

  defp calculate_base_noise_layers(query = %{projected?: true}, _top_level_uid), do: query
  defp calculate_base_noise_layers(query = %{virtual_table?: true}, _top_level_uid), do: query
  defp calculate_base_noise_layers(query, top_level_uid) do
    noise_layers =
      select_noise_layers(query, top_level_uid) ++
      clear_noise_layers(query, top_level_uid) ++
      unclear_noise_layers(query, top_level_uid) ++
      in_noise_layers(query, top_level_uid) ++
      like_noise_layers(query, top_level_uid) ++
      range_noise_layers(query, top_level_uid) ++
      not_equals_noise_layers(query, top_level_uid)
    %Query{query | noise_layers: drop_redundant_noise_layers_columns(noise_layers, query.columns)}
  end

  defp drop_redundant_noise_layers_columns(noise_layers, selected_columns) do
    all_expressions =
      noise_layers
      |> Enum.flat_map(& &1.expressions)
      |> Enum.map(&Expression.unalias/1)
      |> Enum.uniq()
      |> Enum.reject(& &1.constant?)
    Lens.all()
    |> Lens.key(:expressions)
    |> Lens.all()
    |> Lens.reject(& &1.constant?)
    |> Lens.map(noise_layers, &set_noise_layer_expression_alias(&1, all_expressions, selected_columns))
  end

  defp set_noise_layer_expression_alias(expression, all_expressions, selected_columns) do
    expression = Expression.unalias(expression)
    existing_expression = Enum.find(selected_columns, &expression == Expression.unalias(&1))

    case {expression, existing_expression} do
      {%{user_id?: true}, _} -> expression
      {_, nil} ->
        index = Enum.find_index(all_expressions, &expression == &1)
        %Expression{expression | alias: "#{@noise_layer_alias_fix_part}#{index}"}
      {_, _} -> existing_expression
    end
  end

  defp select_noise_layers(%{subquery?: true}, _top_level_uid), do: []
  defp select_noise_layers(query, top_level_uid), do:
    Lens.key(:columns)
    |> Lens.all()
    |> Lens.reject(& needs_aggregation?(query, &1))
    |> raw_columns(query)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, top_level_uid)])

  defp needs_aggregation?(_query, %Expression{constant?: true}), do: true
  defp needs_aggregation?(query, expression), do: Helpers.aggregated_column?(query, expression)

  defp clear_noise_layers(query, top_level_uid), do:
    query
    |> clear_conditions()
    |> Lens.to_list(query)
    |> Enum.flat_map(fn({:comparison, column, :=, constant}) ->
      [
        static_noise_layer(column, constant),
        uid_noise_layer(column, constant, top_level_uid),
      ]
    end)

  defp unclear_noise_layers(query, top_level_uid), do:
    query
    |> unclear_conditions()
    |> raw_columns(query)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, top_level_uid)])

  defp in_noise_layers(query, top_level_uid), do:
    query
    |> conditions_satisfying(&Condition.in?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(fn({:in, column, constants}) ->
      column
      |> raw_columns()
      |> Enum.flat_map(fn(column) ->
        [
          static_noise_layer(column, column) |
          Enum.map(constants, &uid_noise_layer(column, &1, top_level_uid))
        ]
      end)
    end)

  defp range_noise_layers(query, top_level_uid), do:
    query
    |> Range.find_ranges()
    |> Enum.flat_map(fn(%{column: column, interval: range}) ->
      raw_columns(column)
      |> Enum.flat_map(&[
        static_noise_layer(&1, &1, range),
        uid_noise_layer(&1, &1, top_level_uid, range),
      ])
    end)


  # -------------------------------------------------------------------
  # <> noise layers
  # -------------------------------------------------------------------

  defp not_equals_noise_layers(query, top_level_uid), do:
    query
    |> conditions_satisfying(&Condition.not_equals?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(&do_not_equals_noise_layers(&1, top_level_uid))

  defp do_not_equals_noise_layers(
    {:comparison, column, :<>, constant = %Expression{constant?: true, type: :text}}, top_level_uid
  ), do:
    raw_columns(column)
    |> Enum.flat_map(&[
      static_noise_layer(&1, constant, :<>),
      uid_noise_layer(&1, constant, top_level_uid, :<>),
      static_noise_layer(&1, lower(constant), {:<>, :lower}),
    ])
  defp do_not_equals_noise_layers(
    {:comparison, column, :<>, constant = %Expression{constant?: true}}, top_level_uid
  ), do:
    raw_columns(column)
    |> Enum.flat_map(&[
      static_noise_layer(&1, constant, :<>),
      uid_noise_layer(&1, constant, top_level_uid, :<>),
    ])
  defp do_not_equals_noise_layers({:comparison, _column, :<>, _other_column}, _top_level_uid), do: []

  defp lower(%Expression{constant?: true, type: :text, value: value}), do:
    Expression.constant(:text, String.downcase(value))


  # -------------------------------------------------------------------
  # LIKE noise layers
  # -------------------------------------------------------------------

  defp like_noise_layers(query, top_level_uid), do:
    query
    |> conditions_satisfying(&(Condition.like?(&1) or Condition.not_like?(&1)))
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
      case like_layer_keys(pattern) do
        [] -> [
         static_noise_layer(column, column, {:not, kind, without_percents(pattern)}),
         uid_noise_layer(column, column, top_level_uid, {:not, kind, without_percents(pattern)})
        ]
        keys -> [
          static_noise_layer(column, column, {:not, kind, without_percents(pattern)}) |
          Enum.map(keys, &uid_noise_layer(column, column, top_level_uid, {:not, kind, &1}))
        ]
      end
    end)

  defp without_percents(like_pattern), do:
    like_pattern |> LikePattern.graphemes() |> Enum.reject(& &1 == :%) |> Enum.join()

  # See "LIKE pattern seeds" in docs/anonymization for details
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


  # -------------------------------------------------------------------
  # Meaningless noise layers
  # -------------------------------------------------------------------

  defp remove_meaningless_negative_noise_layers(query = %{noise_layers: noise_layers}), do:
    %{query | noise_layers: Enum.map(noise_layers, &override_meaningless(&1, noise_layers))}

  defp override_meaningless(noise_layer, all_layers), do:
    if meaningless?(noise_layer, all_layers), do: do_override_meaningless(noise_layer), else: noise_layer

  defp do_override_meaningless(layer = %{base: {table, column, :<>}}), do:
    %{layer | base: {table, column, {:<>, :override}}}
  defp do_override_meaningless(layer = %{base: {table, column, {:not, kind, pattern}}}), do:
    %{layer | base: {table, column, {:not, kind, pattern, :override}}}

  defp meaningless?(noise_layer, all_layers), do: Enum.any?(all_layers, &positive_equivalent?(noise_layer, &1))

  defp positive_equivalent?(negative_layer, potential_equivalent)
  defp positive_equivalent?(%{base: {table, column, :<>}}, %{base: {table, column, nil}}), do: true
  defp positive_equivalent?(%{base: {table, column, {:not, :like, _}}}, %{base: {table, column, nil}}), do: true
  defp positive_equivalent?(%{base: {table, column, {:not, :ilike, _}}}, %{base: {table, column, nil}}), do: true
  defp positive_equivalent?(_, _), do: false


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

  defp raw_columns(lens \\ Lens.root(), data), do:
    lens
    |> Query.Lenses.leaf_expressions()
    |> Lens.filter(&match?(%Expression{constant?: false, function?: false}, &1))
    |> Lens.to_list(data)

  defp uid_noise_layer(base_column, layer_expression, top_level_uid, extras \\ nil) do
    expressions = [_min = layer_expression, _max = layer_expression, count_of_one(), top_level_uid]
    build_noise_layer(base_column, extras, expressions)
  end

  defp static_noise_layer(base_column, layer_expression, extras \\ nil) do
    expressions = [_min = layer_expression, _max = layer_expression, count_of_one()]
    build_noise_layer(base_column, extras, expressions)
  end

  defp build_noise_layer(base_column, extras, expressions), do:
    NoiseLayer.new({table_name(base_column.table), base_column.name, extras}, expressions)

  defp count_of_one(), do: Expression.constant(:integer, 1)

  defp conditions_satisfying(query, predicate), do:
    query |> non_range_conditions() |> Lens.filter(predicate)

  defp normalize_datasource_case(query) do
    Lens.key(:noise_layers)
    |> Lens.all()
    |> Lens.key(:base)
    |> Lens.map(query, fn({table, column, extras}) ->
      {String.downcase(table), String.downcase(column), extras}
    end)
  end

  deflensp clear_conditions(query), do:
    query |> basic_conditions() |> Lens.filter(&clear_condition?/1)

  deflensp unclear_conditions(query), do:
    query |> basic_conditions() |> Lens.reject(&clear_condition?/1)

  deflensp basic_conditions(query), do:
    query
    |> non_range_conditions()
    |> Lens.reject(&Condition.inequality?/1)
    |> Lens.reject(&Condition.not_equals?/1)
    |> Lens.reject(&Condition.not_like?/1)
    |> Lens.reject(&Condition.in?/1)
    |> Lens.reject(&Condition.like?/1)
    |> Lens.reject(&fk_pk_condition?/1)
    |> Lens.reject(&uid_null_conditions?/1)
    |> Lens.both(non_uid_group_by_clauses())

  deflensp non_uid_group_by_clauses(), do:
    Lens.key(:group_by)
    |> Lens.all()
    |> Lens.filter(& not match?(%Expression{user_id?: true}, &1))

  deflensp non_range_conditions(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.filter(&non_range_condition?(&1, query))

  defp non_range_condition?(condition, query) do
    ranges = query |> Range.find_ranges() |> Enum.map(& &1.column)

    Query.Lenses.conditions_terminals()
    |> Lens.to_list(condition)
    |> Enum.all?(& not &1 in ranges)
  end

  deflensp non_uid_expressions(), do:
    Lens.all()
    |> Lens.key(:expressions)
    |> Lens.all()
    |> Lens.reject(& &1.constant?)
    |> Lens.reject(& &1.user_id?)

  defp clear_condition?({:comparison,
    %Expression{function?: false, constant?: false}, :=, %Expression{constant?: true}}), do: true
  defp clear_condition?(_), do: false

  defp fk_pk_condition?({:comparison, lhs, :=, rhs}), do:
    Expression.key?(lhs) and Expression.key?(rhs)
  defp fk_pk_condition?(_), do: false

  defp uid_null_conditions?({:not, {:is, %Expression{user_id?: true}, :null}}), do: true
  defp uid_null_conditions?({:is, %Expression{user_id?: true}, :null}), do: true
  defp uid_null_conditions?(_), do: false

  defp table_name(_virtual_table = %{db_name: nil, name: name}), do: name
  defp table_name(table), do: table.db_name

  defp reference_aliased(column, subquery, table), do:
    %Expression{name: column.alias || find_alias(column, subquery) || column.name, table: table, type: column.type}

  defp find_alias(column, query) do
    id = Expression.id(column)
    case Enum.find_index(query.columns, &Expression.id(&1) == id) do
      nil -> nil
      index ->
        true = index < length(query.column_titles)
        Enum.at(query.column_titles, index)
    end
  end
end
