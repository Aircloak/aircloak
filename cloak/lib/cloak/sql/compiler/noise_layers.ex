defmodule Cloak.Sql.Compiler.NoiseLayers do
  @moduledoc "Contains functions related to compilation of noise layers."

  alias Cloak.Sql.{Expression, Query, NoiseLayer, Condition, Range, Function}
  alias Cloak.Sql.Compiler.Helpers

  use Lens.Macros

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Fills in the noise_layers for the given query. Furthermore, it modifies the query to float any data that will be
  needed to compute those noise layers to the top level.
  """
  @spec compile(Query.t()) :: Query.t()
  def compile(query = %{command: :select, type: :anonymized}) do
    top_level_uid = Helpers.id_column(query)

    query
    |> compile_analyst_table_columns()
    |> Helpers.apply_bottom_up(&calculate_base_noise_layers(&1, top_level_uid))
    |> Helpers.apply_top_down(&push_down_noise_layers/1)
    |> Helpers.apply_bottom_up(&calculate_floated_noise_layers/1)
    |> Helpers.apply_top_down(&normalize_datasource_case/1)
    |> remove_meaningless_negative_noise_layers()
    |> add_generic_uid_layer_if_needed(top_level_uid)
    |> replace_uid(top_level_uid)
    |> strip_analyst_table_columns()
  end

  def compile(query = %{command: :select, type: :standard}),
    do: Lens.map(Query.Lenses.direct_subqueries() |> Lens.key(:ast), query, &compile/1)

  def compile(query), do: query

  @doc "Returns the columns required to compute the noise layers for the specified query."
  @spec noise_layer_columns(Query.t()) :: [Expression.t()]
  def noise_layer_columns(%{noise_layers: noise_layers}),
    do:
      all_expressions()
      |> non_uid_expressions()
      |> Lens.to_list(noise_layers)
      |> Enum.uniq_by(&Expression.unalias/1)

  @doc "Returns the alias prefix used for noise layer expressions."
  @spec prefix() :: String.t()
  def prefix(), do: "__ac_nlc__"

  # -------------------------------------------------------------------
  # Analyst tables
  # -------------------------------------------------------------------

  defp compile_analyst_table_columns(query) do
    # Recomputing these for every condition becomes far too slow in the presence of nested analyst tables

    update_in(query, [Query.Lenses.all_queries() |> Lens.filter(& &1.analyst_table)], fn query ->
      {:ok, cloak_table, columns} = Cloak.AnalystTable.to_cloak_table_with_columns(query.analyst_table, query.views)
      %{query | analyst_table: {query.analyst_table, cloak_table, columns}}
    end)
  end

  defp strip_analyst_table_columns(query),
    do: update_in(query, [Query.Lenses.all_queries() |> Lens.key(:analyst_table) |> Lens.filter(& &1)], &elem(&1, 0))

  # -------------------------------------------------------------------
  # Cleanup
  # -------------------------------------------------------------------

  # Because UIDs have special handling in many places it's somewhat difficult to instruct the query engine to select
  # them. This makes it so that noise layers are built using the top-level UID in place of any other UID that might
  # appear in expressions.
  defp replace_uid(query, top_level_uid) do
    put_in(query, [Lens.key(:noise_layers) |> all_expressions() |> uid_expressions()], top_level_uid)
  end

  # -------------------------------------------------------------------
  # Pushing layers into subqueries
  # -------------------------------------------------------------------

  defp push_down_noise_layers(query),
    do:
      Enum.reduce(query.noise_layers, query, fn noise_layer, query ->
        case subquery_for_noise_layer(noise_layer) |> Lens.to_list(query) do
          [] ->
            query

          [_] ->
            query
            |> update_in(
              [subquery_for_noise_layer(noise_layer)],
              &push_noise_layer(&1, noise_layer)
            )
            |> update_in([Lens.key(:noise_layers)], &(&1 -- [noise_layer]))
        end
      end)

  defp subquery_for_noise_layer(%{base: {table, _column, _extras}}),
    do:
      Query.Lenses.direct_subqueries()
      |> Lens.filter(&(&1.alias == table))
      |> Lens.key(:ast)
      |> Lens.reject(&(&1.type == :standard))

  defp push_noise_layer(query, %NoiseLayer{
         base: {_table, column, extras},
         expressions: [min, max | rest],
         grouping_set_index: grouping_set_index
       }) do
    {:ok, expression} = find_column(column, query)

    layers =
      raw_columns(expression)
      |> Enum.map(fn column ->
        min = if(Expression.column?(expression) and Expression.constant?(min), do: min, else: column)
        max = if(Expression.column?(expression) and Expression.constant?(max), do: max, else: column)
        build_noise_layer(column, extras, [min, max | rest], grouping_set_index)
      end)
      |> finalize(query)

    update_in(query, [Lens.key(:noise_layers)], &(&1 ++ layers))
  end

  # -------------------------------------------------------------------
  # Floating noise layers and columns
  # -------------------------------------------------------------------

  defp calculate_floated_noise_layers(query),
    do:
      query
      |> add_floated_noise_layers()
      |> float_noise_layers_columns()

  defp add_floated_noise_layers(query) do
    noise_layers =
      if query.type == :restricted && (Helpers.aggregates?(query) or Helpers.group_by?(query)),
        do: float_noise_layers(query.noise_layers ++ floated_noise_layers(query), query),
        else: query.noise_layers ++ floated_noise_layers(query)

    %{query | noise_layers: finalize(noise_layers, query)}
  end

  defp float_noise_layers(layers, query), do: Enum.map(layers, &float_noise_layer(&1, query))

  defp float_noise_layers_columns(query = %Query{type: :restricted}) do
    noise_columns =
      all_expressions()
      |> non_uid_expressions()
      |> Lens.to_list(query.noise_layers)
      |> Enum.reject(&Expression.member?(query.columns, &1))
      |> Enum.uniq()

    %{
      query
      | columns: query.columns ++ noise_columns,
        column_titles: query.column_titles ++ Enum.map(noise_columns, &Expression.title/1),
        aggregators: query.aggregators ++ Enum.filter(noise_columns, &Function.aggregator?/1)
    }
  end

  defp float_noise_layers_columns(query), do: query

  defp floated_noise_layers(query),
    do:
      Query.Lenses.direct_subqueries()
      |> Lens.to_list(query)
      |> Enum.flat_map(fn %{ast: subquery, alias: alias} ->
        subquery_table = Enum.find(query.selected_tables, &(&1.name == alias))
        true = subquery_table != nil

        all_expressions()
        |> non_uid_expressions()
        |> Lens.map(
          subquery.noise_layers,
          &reference_aliased(&1, subquery, subquery_table)
        )
      end)

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count]}, query) do
    if Helpers.aggregates?(query) or Helpers.group_by?(query) do
      min = if grouped_by?(query, min), do: min, else: min_of_min(min)
      max = if grouped_by?(query, max), do: max, else: max_of_max(max)
      %{noise_layer | expressions: [min, max, sum_of_count(count)]}
    else
      noise_layer
    end
  end

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: [min, max, count, user_id]}, query) do
    if Helpers.aggregates?(query) or Helpers.group_by?(query) do
      min = if grouped_by?(query, min), do: min, else: min_of_min(min)
      max = if grouped_by?(query, max), do: max, else: max_of_max(max)
      %{noise_layer | expressions: [min, max, sum_of_count(count), user_id]}
    else
      noise_layer
    end
  end

  defp grouped_by?(query = %{analyst_table: table}, expression) when not is_nil(table) do
    query.group_by
    |> Enum.map(&put_in(&1, [Lens.key(:table)], :unknown))
    |> Enum.any?(&Expression.equals?(&1, %{expression | table: :unknown}))
  end

  defp grouped_by?(query, expression), do: Helpers.grouped_by?(query, expression)

  defp cast(expression, type), do: Expression.function({:cast, type}, [expression], type)

  defp min_of_min(%Expression{constant?: true} = constant), do: constant
  defp min_of_min(%Expression{type: :boolean} = min), do: min |> cast(:integer) |> min_of_min() |> cast(:boolean)
  defp min_of_min(min), do: Expression.function("min", [Expression.unalias(min)], min.type)

  defp max_of_max(%Expression{constant?: true} = constant), do: constant
  defp max_of_max(%Expression{type: :boolean} = max), do: max |> cast(:integer) |> max_of_max() |> cast(:boolean)
  defp max_of_max(max), do: Expression.function("max", [Expression.unalias(max)], max.type)

  defp sum_of_count(%Expression{value: 1}), do: Expression.function("count", [:*], :integer)
  defp sum_of_count(count), do: Expression.function("sum", [Expression.unalias(count)], :integer)

  # -------------------------------------------------------------------
  # Computing base noise layers
  # -------------------------------------------------------------------

  defp calculate_base_noise_layers(query = %{type: :standard}, _top_level_uid), do: query

  defp calculate_base_noise_layers(query, top_level_uid) do
    noise_layers =
      select_noise_layers(query, top_level_uid) ++
        clear_noise_layers(query, top_level_uid) ++
        basic_noise_layers(query, top_level_uid) ++
        group_by_noise_layers(query, top_level_uid) ++
        in_noise_layers(query, top_level_uid) ++
        range_noise_layers(query, top_level_uid) ++
        not_equals_noise_layers(query, top_level_uid) ++ not_like_noise_layers(query, top_level_uid)

    %Query{query | noise_layers: finalize(noise_layers, query)}
  end

  defp finalize(noise_layers, query) do
    noise_layers
    |> adjust_for_analyst_tables(query)
    |> drop_redundant_noise_layers_columns(query)
  end

  defp adjust_for_analyst_tables(noise_layers, %{analyst_table: {_, table, columns}}),
    do: update_in(noise_layers, [all_expressions()], &do_adjust_for_analyst_tables(&1, table, columns))

  defp adjust_for_analyst_tables(noise_layers, _query), do: noise_layers

  defp do_adjust_for_analyst_tables(expression, table, columns) do
    columns
    |> Enum.find_index(&Expression.equals?(&1, expression))
    |> case do
      nil -> expression
      found -> %{Expression.column(table.columns |> Enum.at(found), table) | user_id?: expression.user_id?}
    end
  end

  defp drop_redundant_noise_layers_columns(noise_layers, query) do
    all_expressions =
      noise_layers
      |> Enum.flat_map(& &1.expressions)
      |> Enum.reject(& &1.constant?)
      |> Enum.map(&Expression.unalias/1)
      |> Enum.uniq()

    update_in(noise_layers, [all_expressions()], &set_noise_layer_expression_alias(&1, all_expressions, query))
  end

  defp set_noise_layer_expression_alias(expression, all_expressions, query = %{analyst_table: {_, table, _}}) do
    if Enum.any?(table.columns, &(&1.name == expression.name)) do
      expression
    else
      set_noise_layer_expression_alias(expression, all_expressions, %{query | analyst_table: nil})
    end
  end

  defp set_noise_layer_expression_alias(expression, all_expressions, query) do
    selected_columns = query.columns
    expression_matcher = &Expression.equals?(&1, expression)
    existing_expression = Enum.find(selected_columns, expression_matcher)

    case {expression, existing_expression} do
      {expression, %{user_id?: true}} ->
        expression

      {_, nil} ->
        index = Enum.find_index(all_expressions, expression_matcher)
        true = index != nil
        %Expression{expression | alias: "#{prefix()}#{index}"}

      {_, _} ->
        existing_expression
    end
  end

  defp select_noise_layers(%Query{type: :anonymized} = query, top_level_uid),
    do:
      Lens.key(:columns)
      |> Lens.all()
      |> Lens.reject(&needs_aggregation?(query, &1))
      |> non_synthetic_expressions()
      |> raw_columns(query)
      |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, top_level_uid)])

  defp select_noise_layers(_query, _top_level_uid), do: []

  defp needs_aggregation?(_query, %Expression{constant?: true}), do: true

  defp needs_aggregation?(query, expression),
    do: Helpers.aggregated_column?(expression) or grouped_by?(query, expression)

  defp clear_noise_layers(query, top_level_uid),
    do:
      clear_conditions()
      |> Lens.to_list(query)
      |> Enum.flat_map(fn {:comparison, column, :=, constant} ->
        [
          static_noise_layer(column, constant),
          uid_noise_layer(column, constant, top_level_uid)
        ]
      end)

  defp basic_noise_layers(query, top_level_uid) do
    query
    |> basic_conditions()
    |> Query.Lenses.operands()
    |> non_synthetic_expressions()
    |> raw_columns(query)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, top_level_uid)])
  end

  defp group_by_noise_layers(%Query{type: :anonymized, grouping_sets: grouping_sets} = query, top_level_uid)
       when length(grouping_sets) > 1 do
    grouping_sets
    |> Enum.with_index()
    |> Enum.flat_map(fn {grouping_set, index} ->
      group = Enum.map(grouping_set, &Enum.at(query.group_by, &1))

      Lens.all()
      |> non_uid_expressions()
      |> non_synthetic_expressions()
      |> raw_columns(group)
      |> Enum.flat_map(&[static_noise_layer(&1, &1, nil, index), uid_noise_layer(&1, &1, top_level_uid, nil, index)])
    end)
  end

  defp group_by_noise_layers(query, top_level_uid) do
    Lens.all()
    |> non_uid_expressions()
    |> non_synthetic_expressions()
    |> raw_columns(query.group_by)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, top_level_uid)])
  end

  defp in_noise_layers(query, top_level_uid),
    do:
      conditions_satisfying(&Condition.in?/1)
      |> Lens.to_list(query)
      |> Enum.flat_map(fn {:in, column, constants} ->
        non_synthetic_expressions()
        |> raw_columns(column)
        |> Enum.flat_map(fn column ->
          [
            static_noise_layer(column, column)
            | Enum.map(constants, &uid_noise_layer(column, &1, top_level_uid))
          ]
        end)
      end)

  defp range_noise_layers(query, top_level_uid),
    do:
      query
      |> Range.find_ranges()
      |> Enum.flat_map(fn %{column: column, interval: range} ->
        non_synthetic_expressions()
        |> raw_columns(column)
        |> Enum.flat_map(
          &[
            static_noise_layer(&1, &1, range),
            uid_noise_layer(&1, &1, top_level_uid, range)
          ]
        )
      end)

  # -------------------------------------------------------------------
  # <> noise layers
  # -------------------------------------------------------------------

  defp not_equals_noise_layers(query, top_level_uid),
    do:
      conditions_satisfying(&Condition.not_equals?/1)
      |> Lens.to_list(query)
      |> Enum.flat_map(&do_not_equals_noise_layers(&1, top_level_uid))

  defp do_not_equals_noise_layers(
         {:comparison, column, :<>, constant = %Expression{constant?: true, type: :text}},
         top_level_uid
       ),
       do:
         non_synthetic_expressions()
         |> raw_columns(column)
         |> Enum.flat_map(
           &[
             static_noise_layer(&1, constant, :<>),
             uid_noise_layer(&1, constant, top_level_uid, :<>),
             static_noise_layer(&1, lower(constant), {:<>, :lower})
           ]
         )

  defp do_not_equals_noise_layers(
         {:comparison, column, :<>, constant = %Expression{constant?: true}},
         top_level_uid
       ),
       do:
         non_synthetic_expressions()
         |> raw_columns(column)
         |> Enum.flat_map(
           &[
             static_noise_layer(&1, constant, :<>),
             uid_noise_layer(&1, constant, top_level_uid, :<>)
           ]
         )

  defp do_not_equals_noise_layers({:comparison, _column, :<>, _other_column}, _top_level_uid), do: []

  defp lower(%Expression{constant?: true, type: :text, value: value}),
    do: Expression.constant(:text, String.downcase(value))

  # -------------------------------------------------------------------
  # NOT LIKE noise layers
  # -------------------------------------------------------------------

  defp not_like_noise_layers(query, top_level_uid) do
    conditions_satisfying(&Condition.not_like?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(fn {:not, {_kind, column, _pattern}} ->
      non_synthetic_expressions()
      |> raw_columns(column)
      |> Enum.flat_map(
        &[
          static_noise_layer(&1, &1, :<>),
          uid_noise_layer(&1, &1, top_level_uid, :<>)
        ]
      )
    end)
  end

  # -------------------------------------------------------------------
  # UID handling
  # -------------------------------------------------------------------

  defp add_generic_uid_layer_for_grouping_set(noise_layers, grouping_set_index, top_level_uid) do
    noise_layers
    |> NoiseLayer.filter_layers_for_grouping_set(grouping_set_index)
    |> case do
      [] -> [NoiseLayer.new(nil, [top_level_uid], grouping_set_index) | noise_layers]
      _ -> noise_layers
    end
  end

  defp add_generic_uid_layer_if_needed(query = %{noise_layers: [], grouping_sets: []}, top_level_uid),
    do: %{query | noise_layers: [NoiseLayer.new(nil, [top_level_uid])]}

  defp add_generic_uid_layer_if_needed(query = %{grouping_sets: [_ | _]}, top_level_uid) do
    noise_layers =
      Enum.reduce(
        0..(Enum.count(query.grouping_sets) - 1),
        query.noise_layers,
        &add_generic_uid_layer_for_grouping_set(&2, &1, top_level_uid)
      )

    %{query | noise_layers: noise_layers}
  end

  defp add_generic_uid_layer_if_needed(query, _top_level_uid), do: query

  # -------------------------------------------------------------------
  # Meaningless noise layers
  # -------------------------------------------------------------------

  defp remove_meaningless_negative_noise_layers(query = %{noise_layers: noise_layers}),
    do: %{query | noise_layers: Enum.map(noise_layers, &override_meaningless(&1, noise_layers))}

  defp override_meaningless(noise_layer, all_layers),
    do:
      if(
        meaningless?(noise_layer, all_layers),
        do: do_override_meaningless(noise_layer),
        else: noise_layer
      )

  defp do_override_meaningless(layer = %{base: {table, column, :<>}}),
    do: %{layer | base: {table, column, {:<>, :override}}}

  defp meaningless?(noise_layer, all_layers), do: Enum.any?(all_layers, &positive_equivalent?(noise_layer, &1))

  defp positive_equivalent?(negative_layer, potential_equivalent)
  defp positive_equivalent?(%{base: {table, column, :<>}}, %{base: {table, column, nil}}), do: true
  defp positive_equivalent?(_, _), do: false

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp find_column(name, query) do
    case Enum.find_index(query.column_titles, &(&1 == name)) do
      nil ->
        :error

      index ->
        true = index < length(query.columns)
        {:ok, Enum.at(query.columns, index)}
    end
  end

  defp raw_columns(lens \\ Lens.root(), data),
    do:
      lens
      |> Query.Lenses.leaf_expressions()
      |> Lens.filter(&match?(%Expression{constant?: false, function?: false}, &1))
      |> Lens.to_list(data)
      |> Enum.map(&%Expression{&1 | user_id?: false})

  defp uid_noise_layer(base_column, layer_expression, top_level_uid, extras \\ nil, grouping_set_index \\ nil) do
    expressions = [
      _min = layer_expression,
      _max = layer_expression,
      count_of_one(),
      top_level_uid
    ]

    build_noise_layer(base_column, extras, expressions, grouping_set_index)
  end

  defp static_noise_layer(base_column, layer_expression, extras \\ nil, grouping_set_index \\ nil) do
    expressions = [_min = layer_expression, _max = layer_expression, count_of_one()]
    build_noise_layer(base_column, extras, expressions, grouping_set_index)
  end

  defp build_noise_layer(base_column, extras, expressions, grouping_set_index),
    do: NoiseLayer.new({table_name(base_column.table), base_column.name, extras}, expressions, grouping_set_index)

  defp count_of_one(), do: Expression.constant(:integer, 1)

  defp conditions_satisfying(predicate), do: db_conditions() |> Lens.filter(predicate)

  defp normalize_datasource_case(query) do
    Lens.key(:noise_layers)
    |> Lens.all()
    |> Lens.key(:base)
    |> Lens.map(query, fn {table, column, extras} ->
      {String.downcase(table), String.downcase(column), extras}
    end)
  end

  deflensp non_synthetic_expressions(), do: Lens.filter(&(not &1.synthetic?))

  deflensp clear_conditions(), do: db_conditions() |> Lens.filter(&clear_condition?/1)

  deflensp basic_conditions(query) do
    db_conditions()
    |> Lens.reject(&Range.range?(&1, query))
    |> Lens.reject(&Condition.inequality?/1)
    |> Lens.reject(&Condition.not_equals?/1)
    |> Lens.reject(&Condition.not_like?/1)
    |> Lens.reject(&Condition.in?/1)
    |> Lens.reject(&fk_pk_condition?/1)
    |> Lens.reject(&uid_null_conditions?/1)
    |> Lens.reject(&clear_condition?/1)
  end

  deflensp db_conditions() do
    Query.Lenses.db_filter_clauses() |> Query.Lenses.conditions()
  end

  deflensp non_uid_expressions(), do: Lens.filter(&(not &1.user_id?))

  deflensp uid_expressions(), do: Lens.filter(& &1.user_id?)

  deflensp all_expressions() do
    Lens.all()
    |> Lens.key(:expressions)
    |> Lens.all()
    |> Lens.reject(& &1.constant?)
  end

  defp clear_condition?(
         {:comparison, %Expression{function?: false, constant?: false}, :=, %Expression{constant?: true}}
       ),
       do: true

  defp clear_condition?(_), do: false

  defp fk_pk_condition?({:comparison, lhs, :=, rhs}), do: Expression.key?(lhs) and Expression.key?(rhs)

  defp fk_pk_condition?(_), do: false

  defp uid_null_conditions?({:not, {:is, %Expression{user_id?: true}, :null}}), do: true
  defp uid_null_conditions?({:is, %Expression{user_id?: true}, :null}), do: true
  defp uid_null_conditions?(_), do: false

  defp table_name(_virtual_table = %{db_name: nil, name: name}), do: name
  defp table_name(table), do: table.db_name

  defp reference_aliased(column, subquery, table),
    do: %Expression{
      name: find_alias(column, subquery) || Expression.title(column),
      table: table,
      type: column.type
    }

  defp find_alias(column, query) do
    case Enum.find_index(query.columns, &Expression.equals?(&1, column)) do
      nil ->
        nil

      index ->
        true = index < length(query.column_titles)
        Enum.at(query.column_titles, index)
    end
  end
end
