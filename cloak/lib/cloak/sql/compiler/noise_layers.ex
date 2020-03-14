defmodule Cloak.Sql.Compiler.NoiseLayers do
  @moduledoc "Contains functions related to compilation of noise layers."

  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.{Condition, Expression, Function, NoiseLayer, Query, Range}

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
    query
    |> compile_analyst_table_columns()
    |> Helpers.apply_bottom_up(&calculate_base_noise_layers/1)
    |> Helpers.apply_top_down(&push_down_noise_layers/1)
    |> Helpers.apply_bottom_up(&calculate_floated_noise_layers/1)
    |> Helpers.apply_top_down(&normalize_datasource_case/1)
    |> remove_meaningless_negative_noise_layers()
    |> strip_analyst_table_columns()
  end

  def compile(query = %{command: :select, type: :standard}),
    do: Lens.map(Query.Lenses.direct_subqueries() |> Lens.key(:ast), query, &compile/1)

  def compile(query), do: query

  @doc "Returns the columns required to compute the noise layers for the specified query."
  @spec noise_layer_columns(Query.t()) :: [Expression.t()]
  def noise_layer_columns(%{noise_layers: noise_layers}),
    do:
      noise_layer_expressions()
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
         base: {_table, top_column, extras},
         expressions: top_expressions,
         tag: tag
       }) do
    expression = find_selected_expression_by_name(top_column, query)

    layers =
      non_case_expressions()
      |> raw_columns(expression)
      |> Enum.map(fn column ->
        expressions = push_noise_layer_expressions(expression, column, top_expressions)
        build_noise_layer(column, extras, expressions, tag)
      end)
      |> finalize(query)

    update_in(query, [Lens.key(:noise_layers)], &(&1 ++ layers))
  end

  defp push_noise_layer_expressions(_expression, _column, []), do: []

  defp push_noise_layer_expressions(expression, column, [min, max | rest]) do
    min = if Expression.column?(expression) and Expression.constant?(min), do: min, else: column
    max = if Expression.column?(expression) and Expression.constant?(max), do: max, else: column
    [min, max | rest]
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
      (query.noise_layers ++ floated_noise_layers(query))
      |> float_noise_layers(query)
      |> finalize(query)

    %{query | noise_layers: noise_layers}
  end

  defp float_noise_layers(layers, query), do: Enum.map(layers, &float_noise_layer(&1, query))

  defp float_noise_layers_columns(query = %Query{type: :restricted}) do
    noise_columns =
      noise_layer_expressions()
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
      |> Lens.filter(&(&1.ast.type == :restricted))
      |> Lens.to_list(query)
      |> Enum.flat_map(fn %{ast: subquery, alias: alias} ->
        subquery_table = Enum.find(query.selected_tables, &(&1.name == alias))
        true = subquery_table != nil

        noise_layer_expressions()
        |> non_uid_expressions()
        |> Lens.map(
          subquery.noise_layers,
          &reference_aliased(&1, subquery, subquery_table)
        )
      end)

  defp float_noise_layer(noise_layer = %NoiseLayer{expressions: expressions}, query) do
    if length(expressions) >= 2 and (Helpers.aggregates?(query) or Helpers.group_by?(query)) do
      [min, max | potential_uid_expression] = expressions

      potential_uid_expression = float_potential_uid_expression(query, potential_uid_expression)
      min = if expression_needs_aggregation?(query, min), do: min_of_min(min), else: min
      max = if expression_needs_aggregation?(query, max), do: max_of_max(max), else: max

      %{noise_layer | expressions: [min, max | potential_uid_expression]}
    else
      noise_layer
    end
  end

  defp expression_needs_aggregation?(query, expression),
    do: query.type == :restricted and not Helpers.grouped_by?(query, expression)

  defp cast(expression, type), do: Expression.function({:cast, type}, [expression], type)

  defp min_of_min(%Expression{kind: :constant} = constant), do: constant
  defp min_of_min(%Expression{type: :boolean} = min), do: min |> cast(:integer) |> min_of_min() |> cast(:boolean)
  defp min_of_min(min), do: Expression.function("min", [Expression.unalias(min)], min.type)

  defp max_of_max(%Expression{kind: :constant} = constant), do: constant
  defp max_of_max(%Expression{type: :boolean} = max), do: max |> cast(:integer) |> max_of_max() |> cast(:boolean)
  defp max_of_max(max), do: Expression.function("max", [Expression.unalias(max)], max.type)

  defp float_potential_uid_expression(%Query{anonymization_type: :statistics, type: :restricted}, [
         %Expression{kind: :function, name: "case", args: [condition, uid, _null]}
       ]) do
    casted_condition = Expression.function({:cast, :integer}, [condition], :integer)
    condition_max = Expression.function("max", [casted_condition], :integer)
    aggregated_condition = Expression.function("=", [condition_max, Expression.constant(:integer, 1)], :boolean)
    [Expression.function("case", [aggregated_condition, uid, Expression.constant(nil, nil)], uid.type)]
  end

  defp float_potential_uid_expression(%Query{anonymization_type: :statistics, type: :restricted}, [
         %Expression{user_id?: false} = conditional_uid
       ]) do
    [
      Expression.function("count", [{:distinct, conditional_uid}], :integer),
      Expression.function("min", [conditional_uid], conditional_uid.type),
      Expression.function("max", [conditional_uid], conditional_uid.type)
    ]
  end

  defp float_potential_uid_expression(%Query{anonymization_type: :statistics, type: :anonymized} = query, [
         %Expression{user_id?: true}
       ]) do
    [_grouping_id, min_uid, max_uid | _rest] = query.aggregators
    [Helpers.id_column(query), min_uid, max_uid]
  end

  defp float_potential_uid_expression(query, [%Expression{user_id?: true}]), do: [Helpers.id_column(query)]

  defp float_potential_uid_expression(_query, potential_uid_expression), do: potential_uid_expression

  # -------------------------------------------------------------------
  # Computing base noise layers
  # -------------------------------------------------------------------

  defp calculate_base_noise_layers(query = %{type: :standard}), do: query

  defp calculate_base_noise_layers(query) do
    noise_layers =
      clear_noise_layers(query) ++
        basic_noise_layers(query) ++
        group_noise_layers(query) ++
        in_noise_layers(query) ++
        range_noise_layers(query) ++
        not_equals_noise_layers(query) ++
        not_like_noise_layers(query) ++
        aggregator_noise_layers(query)

    %Query{query | noise_layers: finalize(noise_layers, query)}
  end

  defp finalize(noise_layers, query) do
    noise_layers
    |> adjust_for_analyst_tables(query)
    |> drop_redundant_noise_layers_columns(query)
  end

  defp adjust_for_analyst_tables(noise_layers, %{analyst_table: {_, table, columns}}),
    do: update_in(noise_layers, [noise_layer_expressions()], &do_adjust_for_analyst_tables(&1, table, columns))

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
      |> Enum.reject(&Expression.constant?/1)
      |> Enum.map(&Expression.unalias/1)
      |> Enum.uniq()

    update_in(noise_layers, [noise_layer_expressions()], &set_noise_layer_expression_alias(&1, all_expressions, query))
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

  defp clear_noise_layers(query) do
    uid = Helpers.id_column(query)

    clear_conditions()
    |> Lens.to_list(query)
    |> Enum.flat_map(fn %Expression{kind: :function, name: "=", args: [column, constant]} ->
      [
        static_noise_layer(column, constant),
        uid_noise_layer(column, constant, uid)
      ]
    end)
  end

  defp basic_noise_layers(query) do
    uid = Helpers.id_column(query)

    query
    |> basic_conditions()
    |> Query.Lenses.operands()
    |> non_synthetic_expressions()
    |> raw_columns(query)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, uid)])
  end

  defp group_noise_layers(%Query{type: :anonymized, grouping_sets: grouping_sets} = query)
       when length(grouping_sets) > 1 do
    uid = Helpers.id_column(query)

    grouping_sets
    |> Enum.with_index()
    |> Enum.flat_map(fn {grouping_set, index} ->
      group = Enum.map(grouping_set, &Enum.at(query.group_by, &1))

      Lens.all()
      |> non_uid_expressions()
      |> non_synthetic_expressions()
      |> raw_columns(group)
      |> Enum.flat_map(
        &[
          static_noise_layer(&1, &1, nil, {:grouping_set, index}),
          uid_noise_layer(&1, &1, uid, nil, {:grouping_set, index})
        ]
      )
    end)
  end

  defp group_noise_layers(query) do
    uid = Helpers.id_column(query)

    Query.Lenses.group_expressions()
    |> non_uid_expressions()
    |> non_synthetic_expressions()
    |> non_case_expressions()
    |> raw_columns(query)
    |> Enum.flat_map(&[static_noise_layer(&1, &1), uid_noise_layer(&1, &1, uid)])
  end

  defp in_noise_layers(query) do
    uid = Helpers.id_column(query)

    conditions_satisfying(&Condition.in?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(fn %Expression{kind: :function, name: "in", args: [column | constants]} ->
      non_synthetic_expressions()
      |> raw_columns(column)
      |> Enum.flat_map(fn column ->
        [
          static_noise_layer(column, column)
          | Enum.map(constants, &uid_noise_layer(column, &1, uid))
        ]
      end)
    end)
  end

  defp range_noise_layers(query),
    do:
      query
      |> Range.find_ranges()
      |> Enum.flat_map(fn
        %{column: column, interval: {operator, _value}} when is_atom(operator) ->
          non_synthetic_expressions()
          |> raw_columns(column)
          |> Enum.flat_map(&[build_noise_layer(&1, operator, [])])

        %{column: column, interval: range} ->
          non_synthetic_expressions()
          |> raw_columns(column)
          |> Enum.flat_map(&[static_noise_layer(&1, &1, range)])
      end)

  defp aggregator_noise_layers(query) do
    query.aggregators
    |> Enum.with_index()
    |> Enum.flat_map(fn {aggregator, index} ->
      Query.Lenses.all_expressions()
      |> non_synthetic_expressions()
      |> Query.Lenses.case_when_clauses()
      |> Lens.to_list(aggregator)
      |> Enum.flat_map(fn condition ->
        %Expression{kind: :function, name: "=", args: [column, constant]} = condition
        uid = Helpers.id_column(query)

        conditional_uid = Expression.function("case", [condition, uid, Expression.constant(nil, nil)], uid.type)

        [
          static_noise_layer(column, constant, nil, {:aggregator, index}),
          uid_noise_layer(column, constant, conditional_uid, nil, {:aggregator, index})
        ]
      end)
    end)
  end

  # -------------------------------------------------------------------
  # <> noise layers
  # -------------------------------------------------------------------

  defp not_equals_noise_layers(query) do
    uid = Helpers.id_column(query)

    conditions_satisfying(&Condition.not_equals?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(&do_not_equals_noise_layers(&1, uid))
  end

  defp do_not_equals_noise_layers(
         %Expression{kind: :function, name: "<>", args: [column, %Expression{kind: :constant, type: :text} = constant]},
         uid
       ),
       do:
         non_synthetic_expressions()
         |> raw_columns(column)
         |> Enum.flat_map(
           &[
             static_noise_layer(&1, constant, :<>),
             uid_noise_layer(&1, constant, uid, :<>),
             static_noise_layer(&1, lower(constant), {:<>, :lower})
           ]
         )

  defp do_not_equals_noise_layers(
         %Expression{kind: :function, name: "<>", args: [column, %Expression{kind: :constant} = constant]},
         uid
       ),
       do:
         non_synthetic_expressions()
         |> raw_columns(column)
         |> Enum.flat_map(
           &[
             static_noise_layer(&1, constant, :<>),
             uid_noise_layer(&1, constant, uid, :<>)
           ]
         )

  defp do_not_equals_noise_layers(%Expression{kind: :function, name: "<>"}, _uid), do: []

  defp lower(%Expression{kind: :constant, type: :text, value: value}),
    do: Expression.constant(:text, String.downcase(value))

  # -------------------------------------------------------------------
  # NOT LIKE noise layers
  # -------------------------------------------------------------------

  defp not_like_noise_layers(query) do
    uid = Helpers.id_column(query)

    conditions_satisfying(&Condition.not_like?/1)
    |> Lens.to_list(query)
    |> Enum.flat_map(fn %Expression{
                          kind: :function,
                          name: "not",
                          args: [%Expression{kind: :function, name: verb, args: [column, _pattern]}]
                        }
                        when verb in ~w(like ilike) ->
      non_synthetic_expressions()
      |> raw_columns(column)
      |> Enum.flat_map(
        &[
          static_noise_layer(&1, &1, :<>),
          uid_noise_layer(&1, &1, uid, :<>)
        ]
      )
    end)
  end

  # -------------------------------------------------------------------
  # Meaningless noise layers
  # -------------------------------------------------------------------

  defp remove_meaningless_negative_noise_layers(query = %{noise_layers: noise_layers}),
    do: %{query | noise_layers: noise_layers |> Enum.map(&override_meaningless(&1, noise_layers)) |> Enum.uniq()}

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

  defp find_selected_expression_by_name(name, query) do
    index = Enum.find_index(query.column_titles, &(&1 == name))
    true = index != nil
    true = index < length(query.columns)
    Enum.at(query.columns, index)
  end

  defp raw_columns(lens, data),
    do:
      lens
      |> Query.Lenses.leaf_expressions()
      |> Lens.filter(&Expression.column?/1)
      |> Lens.to_list(data)
      |> Enum.map(&%Expression{&1 | user_id?: false})

  defp uid_noise_layer(base_column, layer_expression, uid_expression, extras \\ nil, tag \\ nil) do
    expressions = [_min = layer_expression, _max = layer_expression, uid_expression]

    build_noise_layer(base_column, extras, expressions, tag)
  end

  defp static_noise_layer(base_column, layer_expression, extras \\ nil, tag \\ nil),
    do: build_noise_layer(base_column, extras, [_min = layer_expression, _max = layer_expression], tag)

  defp build_noise_layer(base_column, extras, expressions, tag \\ nil),
    do: NoiseLayer.new({table_name(base_column.table), base_column.name, extras}, expressions, tag)

  defp conditions_satisfying(predicate), do: pre_anonymization_conditions() |> Lens.filter(predicate)

  defp normalize_datasource_case(query) do
    Lens.key(:noise_layers)
    |> Lens.all()
    |> Lens.key(:base)
    |> Lens.map(query, fn {table, column, extras} ->
      {String.downcase(table), String.downcase(column), extras}
    end)
  end

  deflensp non_synthetic_expressions(), do: Lens.filter(&(not &1.synthetic?))

  deflensp non_case_expressions(), do: Lens.filter(&(not match?(%Expression{kind: :function, name: "case"}, &1)))

  deflensp clear_conditions(), do: pre_anonymization_conditions() |> Lens.filter(&clear_condition?/1)

  deflensp basic_conditions(query) do
    pre_anonymization_conditions()
    |> Lens.reject(&Range.range?(&1, query))
    |> Lens.reject(&Condition.inequality?/1)
    |> Lens.reject(&Condition.not_equals?/1)
    |> Lens.reject(&Condition.not_like?/1)
    |> Lens.reject(&Condition.in?/1)
    |> Lens.reject(&fk_pk_condition?/1)
    |> Lens.reject(&uid_null_conditions?/1)
    |> Lens.reject(&clear_condition?/1)
  end

  deflensp pre_anonymization_conditions() do
    Lens.both(
      Query.Lenses.pre_anonymization_filter_clauses(),
      Query.Lenses.group_expressions() |> Query.Lenses.case_when_clauses()
    )
    |> Query.Lenses.conditions()
  end

  deflensp non_uid_expressions(), do: Lens.filter(&(not &1.user_id?))

  deflensp noise_layer_expressions() do
    Lens.all()
    |> Lens.key(:expressions)
    |> Lens.all()
    |> Lens.reject(&Expression.constant?/1)
  end

  defp clear_condition?(%Expression{
         kind: :function,
         name: "=",
         args: [%Expression{kind: :column}, %Expression{kind: :constant}]
       }),
       do: true

  defp clear_condition?(_), do: false

  defp fk_pk_condition?(%Expression{kind: :function, name: "=", args: [lhs, rhs]}),
    do: Expression.key?(lhs) and Expression.key?(rhs)

  defp fk_pk_condition?(_), do: false

  defp uid_null_conditions?(condition),
    do: Condition.verb(condition) == :is_null and Condition.subject(condition).user_id?

  defp table_name(_virtual_table = %{db_name: nil, name: name}), do: name
  defp table_name(table), do: table.db_name

  defp reference_aliased(column, subquery, table),
    do: %Expression{
      kind: :column,
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
