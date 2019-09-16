defmodule Cloak.DataSource.MongoDB.Pipeline do
  @moduledoc "MongoDB helper functions for mapping a query to an aggregation pipeline."

  alias Cloak.Sql.{Query, Expression, Condition, LikePattern, Function}
  alias Cloak.Query.ExecutionError
  alias Cloak.DataSource.MongoDB.{Schema, Projector}
  alias Cloak.DataSource.Table

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Builds a MongoDB aggregation pipeline from a compiled query."
  @spec build(Query.t()) :: {String.t(), [map]}
  def build(query, top_level? \\ true) do
    {collection, pipeline, conditions} = start_pipeline(query.from, query.selected_tables, query.where)
    {collection, pipeline ++ finish_pipeline(%Query{query | where: conditions}, top_level?)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_pipeline(table_name, selected_tables, conditions) when is_binary(table_name) do
    table = Enum.find(selected_tables, &(&1.name == table_name))
    {complex_conditions, basic_conditions} = extract_basic_conditions(table, conditions)

    pipeline =
      filter_data(basic_conditions) ++
        unwind_arrays(table.array_path) ++
        Projector.project_array_sizes(table) ++
        move_root_to(table_name)

    {table.collection, pipeline, complex_conditions}
  end

  defp start_pipeline({:subquery, subquery}, _selected_tables, conditions) do
    {collection, pipeline} = build(subquery.ast, false)
    pipeline = add_join_timing_protection(pipeline, subquery) ++ move_root_to(subquery.alias)
    {collection, pipeline, conditions}
  end

  defp start_pipeline({:join, %{type: :right_outer_join} = join}, selected_tables, conditions) do
    join = %{join | type: :left_outer_join, lhs: join.rhs, rhs: join.lhs}
    start_pipeline({:join, join}, selected_tables, conditions)
  end

  defp start_pipeline({:join, join}, selected_tables, conditions) do
    {lhs_collection, lhs_pipeline, nil} = start_pipeline(join.lhs, selected_tables, nil)
    {rhs_collection, rhs_pipeline, nil} = start_pipeline(join.rhs, selected_tables, nil)

    lhs_tables = tables_in_branch(join.lhs)
    outer_join? = join.type == :left_outer_join
    pipeline = lhs_pipeline ++ join_pipeline(rhs_collection, rhs_pipeline, join.conditions, lhs_tables, outer_join?)

    {lhs_collection, pipeline, conditions}
  end

  defp finish_pipeline(query, top_level?),
    do: parse_conditions(query.where) ++ parse_query(query, top_level?)

  defp project_top_columns(columns, _top_level? = true),
    do: [%{"$project": %{row: Enum.map(columns, &project_top_column/1), _id: false}}]

  defp project_top_columns(_columns, _top_level? = false), do: []

  defp project_top_column(%Expression{constant?: true} = constant), do: Projector.project_expression(constant)

  defp project_top_column(column), do: "$#{Expression.title(column)}"

  defp parse_query(%Query{subquery?: false} = query, _top_level? = true),
    do:
      (query.db_columns |> Enum.reject(&Expression.constant?/1) |> Projector.project_columns()) ++
        project_top_columns(query.db_columns, true)

  defp parse_query(%Query{subquery?: true} = query, top_level?),
    do:
      query
      |> compile_columns()
      |> aggregate_and_project(top_level?)

  defp filter_data(nil), do: []
  defp filter_data(condition), do: [%{"$match": parse_where_condition(condition)}]

  defp parse_operator(:=), do: :"$eq"
  defp parse_operator(:>), do: :"$gt"
  defp parse_operator(:>=), do: :"$gte"
  defp parse_operator(:<), do: :"$lt"
  defp parse_operator(:<=), do: :"$lte"
  defp parse_operator(:<>), do: :"$ne"

  defp map_column(%Expression{table: :unknown, name: name}) when is_binary(name), do: name
  defp map_column(%Expression{table: %{name: table}, name: name}) when is_binary(name), do: table <> "." <> name

  defp map_column(_),
    do: raise(ExecutionError, message: "Condition on MongoDB data source expects a column as subject.")

  defp parse_where_condition({:and, lhs, rhs}), do: %{"$and": [parse_where_condition(lhs), parse_where_condition(rhs)]}

  defp parse_where_condition({:or, lhs, rhs}), do: %{"$or": [parse_where_condition(lhs), parse_where_condition(rhs)]}

  defp parse_where_condition({:comparison, subject, operator, target}),
    do: %{
      "$expr": %{
        parse_operator(operator) => [Projector.project_expression(subject), Projector.project_expression(target)]
      }
    }

  defp parse_where_condition({:not, {:comparison, subject, :=, target}}),
    do: %{"$expr": %{"$ne": [Projector.project_expression(subject), Projector.project_expression(target)]}}

  defp parse_where_condition({:not, {:comparison, subject, :<>, target}}),
    do: %{"$expr": %{"$eq": [Projector.project_expression(subject), Projector.project_expression(target)]}}

  defp parse_where_condition({:is, subject, :null}),
    do: %{"$expr": %{"$eq": [Projector.project_expression(subject), nil]}}

  defp parse_where_condition({:not, {:is, subject, :null}}),
    do: %{"$expr": %{"$gt": [Projector.project_expression(subject), nil]}}

  defp parse_where_condition({:in, subject, targets}),
    do: %{
      "$expr": %{"$in": [Projector.project_expression(subject), Enum.map(targets, &Projector.project_expression/1)]}
    }

  defp parse_where_condition({:not, {:in, subject, targets}}),
    do: %{
      "$expr": %{"$nin": [Projector.project_expression(subject), Enum.map(targets, &Projector.project_expression/1)]}
    }

  defp parse_where_condition({:like, subject, pattern}), do: %{map_column(subject) => regex(pattern, "ms")}

  defp parse_where_condition({:ilike, subject, pattern}), do: %{map_column(subject) => regex(pattern, "msi")}

  defp parse_where_condition({:not, {:like, subject, pattern}}),
    do: %{map_column(subject) => %{"$not": regex(pattern, "ms")}}

  defp parse_where_condition({:not, {:ilike, subject, pattern}}),
    do: %{map_column(subject) => %{"$not": regex(pattern, "msi")}}

  defp regex(%Expression{constant?: true, value: pattern}, options),
    do: %BSON.Regex{
      pattern: LikePattern.to_regex_pattern(pattern),
      options: options
    }

  defp extract_basic_conditions(table, conditions) do
    {complex_conditions, basic_conditions} = Condition.partition(conditions, complex_filter(table.array_path))

    {table_conditions, other_tables_conditions} =
      Condition.partition(basic_conditions, &(Condition.subject(&1).table.name == table.name))

    table_conditions =
      Query.Lenses.conditions_terminals() |> Lens.map(table_conditions, &%Expression{&1 | table: :unknown})

    {Condition.combine(:and, complex_conditions, other_tables_conditions), table_conditions}
  end

  defp complex_filter([]), do: &complex_condition?(&1, [])
  defp complex_filter([array | _]), do: &complex_condition?(&1, [array <> "."])

  defp complex_condition?(column, complex_name_prefixes) do
    column_name = Condition.subject(column).name

    column_name == nil or Schema.is_array_size?(column_name) or String.starts_with?(column_name, complex_name_prefixes)
  end

  defp extract_columns_from_conditions(conditions) do
    extra_columns =
      Query.Lenses.conditions()
      |> Lens.to_list(conditions)
      |> Enum.flat_map(&Condition.targets/1)
      |> Enum.filter(& &1.function?)
      |> Enum.uniq()

    conditions =
      Query.Lenses.conditions()
      |> Query.Lenses.operands()
      |> Lens.filter(&match?(%Expression{function?: true}, &1))
      |> Lens.map(conditions, fn column ->
        index = Enum.find_index(extra_columns, &(&1 == column))
        %Expression{name: "__condition_#{index}", type: column.type}
      end)

    extra_columns =
      extra_columns
      |> Enum.with_index()
      |> Enum.map(fn {column, index} ->
        %Expression{column | alias: "__condition_#{index}"}
      end)

    {conditions, extra_columns}
  end

  defp parse_conditions(conditions) do
    {conditions, extra_columns} = extract_columns_from_conditions(conditions)
    project_extra_columns(extra_columns) ++ filter_data(conditions)
  end

  defp project_extra_columns([]), do: []

  defp project_extra_columns(extra_columns),
    do: [%{"$addFields": extra_columns |> Enum.map(&Projector.project_column/1) |> Enum.into(%{})}]

  defp unwind_arrays(_path, _path \\ "")
  defp unwind_arrays([], _path), do: []

  defp unwind_arrays([array | rest], path) do
    path = path <> array
    [%{"$unwind": "$" <> path} | unwind_arrays(rest, path)]
  end

  defp order_and_range(query), do: order_rows(query.order_by) ++ offset_rows(query.offset) ++ limit_rows(query.limit)

  @supported_orders [
    {:asc, :nulls_first},
    {:desc, :nulls_last},
    {:asc, :nulls_natural},
    {:desc, :nulls_natural}
  ]

  defp order_rows([]), do: []

  defp order_rows(order_by) do
    order_by =
      for {column, dir, nulls} <- order_by do
        true = {dir, nulls} in @supported_orders

        dir = if dir == :desc, do: -1, else: 1
        {Expression.title(column), dir}
      end

    [%{"$sort": order_by}]
  end

  defp offset_rows(0), do: []
  defp offset_rows(amount), do: [%{"$skip": amount}]

  defp limit_rows(nil), do: []
  defp limit_rows(amount), do: [%{"$limit": amount}]

  defp compile_columns(query) do
    # Complex columns referenced by the `ORDER BY` clause, that are not already selected,
    # need to be named and included in the projected columns list so that the `$sort` step can reference them.
    needed_columns =
      (query.db_columns ++ Query.order_by_expressions(query))
      |> Enum.uniq()
      |> Enum.with_index(1)
      |> Enum.map(fn {column, index} ->
        alias = Expression.title(column) || "__unknown_#{index}"
        %Expression{column | alias: alias}
      end)

    order_by =
      Lens.map(Lens.all() |> Lens.at(0), query.order_by, fn column ->
        Enum.find(needed_columns, column, &(%Expression{&1 | alias: nil} == column))
      end)

    %Query{query | db_columns: needed_columns, order_by: order_by}
  end

  defp extract_aggregator(expression) do
    if Function.aggregator?(expression),
      do: [expression],
      else: Enum.flat_map(expression.function_args, &extract_aggregator/1)
  end

  defp project_properties([]), do: nil

  defp project_properties(properties) do
    properties
    |> Enum.with_index()
    |> Enum.map(fn {column, index} ->
      Projector.project_column(%Expression{column | alias: "property_#{index}"})
    end)
    |> Enum.into(%{})
  end

  defp project_aggregators(aggregators) do
    aggregators
    |> Enum.with_index()
    |> Enum.map(fn {column, index} ->
      Projector.project_column(%Expression{column | alias: "aggregated_#{index}"})
    end)
    |> Enum.into(%{})
  end

  # This extracts the upper part of a column that need to be projected after grouping is done.
  defp extract_column_top(%Expression{constant?: true} = column, _aggregators, _groups), do: column

  defp extract_column_top(
         %Expression{function: "count", function_args: [{:distinct, _}]} = column,
         aggregators,
         _groups
       ) do
    # For distinct count, we gather values into a set and then project the size of the set.
    index = Enum.find_index(aggregators, &Expression.equals?(column, &1))

    %Expression{
      column
      | function?: true,
        function: "size",
        function_args: [%Expression{name: "aggregated_#{index}", table: :unknown, type: :integer}]
    }
  end

  defp extract_column_top(
         %Expression{function_args: [{:distinct, _}]} = column,
         aggregators,
         _groups
       ) do
    # For distinct aggregators, we gather values into a set and then project the aggregator over the set.
    index = Enum.find_index(aggregators, &Expression.equals?(column, &1))

    %Expression{
      column
      | function_args: [%Expression{name: "aggregated_#{index}", table: :unknown}]
    }
  end

  defp extract_column_top(column, aggregators, groups) do
    case Enum.find_index(aggregators, &Expression.equals?(column, &1)) do
      nil ->
        case Enum.find_index(groups, &Expression.equals?(column, &1)) do
          nil ->
            # Has to be a function call since the lookups failed.
            args = Enum.map(column.function_args, &extract_column_top(&1, aggregators, groups))
            %Expression{column | function_args: args}

          index ->
            %Expression{
              name: "_id.property_#{index}",
              table: :unknown,
              alias: Expression.title(column)
            }
        end

      index ->
        aggregator = Enum.at(aggregators, index)
        %Expression{name: "aggregated_#{index}", table: :unknown, alias: column.alias, type: aggregator.type}
    end
  end

  defp extract_column_top_from_conditions(conditions, aggregators, groups),
    do:
      Query.Lenses.conditions()
      |> Query.Lenses.operands()
      |> Lens.map(conditions, &extract_column_top(&1, aggregators, groups))

  defp aggregate_and_project(
         %Query{db_columns: columns, group_by: groups, having: having} = query,
         top_level?
       ) do
    having_columns =
      Query.Lenses.conditions()
      |> Lens.to_list(having)
      |> Enum.map(&Condition.subject/1)

    aggregators =
      (columns ++ having_columns)
      |> Enum.flat_map(&extract_aggregator/1)
      |> Enum.uniq()

    if aggregators == [] and query.grouping_sets == [] do
      Projector.project_columns(columns) ++ order_and_range(query) ++ project_top_columns(columns, top_level?)
    else
      column_tops = Enum.map(columns, &extract_column_top(&1, aggregators, groups))
      properties = project_properties(groups)
      group = aggregators |> project_aggregators() |> Enum.into(%{"_id" => properties})
      having = extract_column_top_from_conditions(having, aggregators, groups)

      [%{"$group": group}] ++
        parse_conditions(having) ++
        Projector.project_columns(column_tops) ++
        order_and_range(query) ++ project_top_columns(column_tops, top_level?)
    end
  end

  defp process_columns_for_lookup(condition, source_tables) do
    Query.Lenses.leaf_expressions()
    |> Lens.key(:table)
    |> Lens.filter(&(is_map(&1) and &1.name in source_tables))
    # MongoDB 3.6 only allows field names starting with lower-case characters in the `$lookup` stage.
    # We prepend a `t` to each table name to avoid errors for invalid names.
    |> Lens.map(condition, fn table -> %{table | name: "$t" <> table.name} end)
  end

  defp join_pipeline(collection, pipeline, conditions, source_tables, outer_join?) do
    on_stage = conditions |> process_columns_for_lookup(source_tables) |> filter_data()
    namespace = "ac_join_accumulator"

    [
      %{
        "$lookup": %{
          from: collection,
          # MongoDB 3.6 only allows field names starting with lower-case characters in the `$lookup` stage.
          # We prepend a `t` to each table name to avoid errors for invalid names.
          let: for(table <- source_tables, into: %{}, do: {"t" <> table, "$" <> table}),
          pipeline: pipeline ++ on_stage,
          as: namespace
        }
      },
      %{
        "$unwind": %{
          path: "$" <> namespace,
          preserveNullAndEmptyArrays: outer_join?
        }
      },
      %{
        "$replaceRoot": %{
          newRoot: %{
            "$mergeObjects": ["$" <> namespace, "$$ROOT"]
          }
        }
      },
      %{"$project": %{namespace => 0}}
    ]
  end

  defp tables_in_branch(name) when is_binary(name), do: [name]
  defp tables_in_branch({:subquery, subquery}), do: [subquery.alias]
  defp tables_in_branch({:join, join}), do: tables_in_branch(join.lhs) ++ tables_in_branch(join.rhs)

  defp move_root_to(path), do: [%{"$replaceRoot": %{newRoot: %{path => "$$ROOT"}}}]

  defp add_join_timing_protection(pipeline, %{ast: query, join_timing_protection?: true}) do
    [
      %{
        "$facet": %{
          rows: pipeline
        }
      },
      %{
        "$project": %{
          rows: %{
            "$cond": [
              %{
                "$eq": ["$rows", []]
              },
              [query.db_columns |> Enum.map(&{Expression.title(&1), Table.invalid_value(&1.type)}) |> Enum.into(%{})],
              "$rows"
            ]
          }
        }
      },
      %{
        "$unwind": "$rows"
      },
      %{
        "$replaceRoot": %{newRoot: "$rows"}
      }
    ]
  end

  defp add_join_timing_protection(pipeline, _subquery), do: pipeline
end
