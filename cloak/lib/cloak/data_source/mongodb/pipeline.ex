defmodule Cloak.DataSource.MongoDB.Pipeline do
  @moduledoc "MongoDB helper functions for mapping a query to an aggregation pipeline."

  alias Cloak.Sql.{Query, Expression, Condition, LikePattern}
  alias Cloak.DataSource
  alias Cloak.DataSource.MongoDB.{Schema, Projector}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Builds a MongoDB aggregation pipeline from a compiled query."
  @spec build(Query.t) :: {String.t, [map]}
  def build(_query, _top_level? \\ true)
  def build(%Query{selected_tables: [table]} = query, top_level?) do
    {collection, pipeline, conditions} = start_pipeline(query.from, table, query.where)
    {collection, pipeline ++ finish_pipeline(%Query{query | where: conditions}, top_level?)}
  end
  def build(%Query{from: {:join, join}} = query, top_level?) do
    join_info = join_info(join, query.selected_tables)
    # The `$lookup` operator projects a foreign document into the specified field from the current document.
    # We create an unique name under which the fields of the projected document will live for the duration of the query.
    namespace = "ac_temp_ns_#{:erlang.unique_integer([:positive])}"
    rhs_table_columns =
      Enum.map(join_info.rhs_table.columns, &%{&1 | name: namespace <> "." <> &1.name})
    join_table = %{name: "join", db_name: "join", columns: join_info.lhs_table.columns ++ rhs_table_columns}
    query =
      Query.Lenses.query_expressions()
      |> Lens.filter(& &1.name != nil and &1.table == join_info.rhs_table)
      |> Lens.map(query, &%Expression{&1 | name: namespace <> "." <> &1.name})
    {collection, pipeline, conditions} = start_pipeline(join_info.lhs, join_info.lhs_table, query.where)
    pipeline =
      pipeline ++
      lookup_table(join_info.rhs_table.collection, join_info.lhs_field, join_info.rhs_field, namespace) ++
      unwind_arrays(join_info.rhs_table.array_path, namespace <> ".") ++
      finish_pipeline(%Query{query | where: conditions, selected_tables: [join_table]}, top_level?)
    {collection, pipeline}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_pipeline(table_name, table, conditions) when is_binary(table_name) do
    {complex_conditions, basic_conditions} = extract_basic_conditions(table, conditions)
    pipeline = filter_data(basic_conditions) ++ unwind_arrays(table.array_path)
    {table.collection, pipeline, complex_conditions}
  end
  defp start_pipeline({:subquery, subquery}, _table, conditions) do
    {collection, pipeline} = build(subquery.ast, false)
    {collection, pipeline, conditions}
  end

  defp finish_pipeline(%Query{selected_tables: [table]} = query, top_level?) do
    case used_array_size_columns(query) do
      [] -> []
      _ -> Projector.project_array_sizes(table)
    end ++
    (table.columns |> Enum.map(& &1.name) |> parse_conditions(query.where)) ++
    parse_query(query, top_level?)
  end

  defp project_columns(columns, _top_level? = true), do:
    # Mongo 3.0 doesn't support projection of arrays, which would more efficient for data transfer.
    columns
    |> Enum.with_index()
    |> Enum.map(fn ({column, index}) -> %Expression{column | alias: "f#{index}"} end)
    |> Projector.project_columns()
  defp project_columns(columns, _top_level? = false), do:
    Projector.project_columns(columns)

  defp parse_query(%Query{subquery?: false} = query, _top_level? = true), do:
    project_columns(query.db_columns, true)
  defp parse_query(%Query{subquery?: true} = query, top_level?), do:
    query
    |> compile_columns()
    |> aggregate_and_project(top_level?)

  defp filter_data(nil), do: []
  defp filter_data(condition), do: [%{'$match': parse_where_condition(condition)}]

  defp parse_operator(:=), do: :'$eq'
  defp parse_operator(:>), do: :'$gt'
  defp parse_operator(:>=), do: :'$gte'
  defp parse_operator(:<), do: :'$lt'
  defp parse_operator(:<=), do: :'$lte'
  defp parse_operator(:<>), do: :'$ne'

  @epoch :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
  defp map_constant(%Expression{constant?: true, value: %NaiveDateTime{} = datetime}), do:
    DateTime.from_naive!(datetime, "Etc/UTC")
  defp map_constant(%Expression{constant?: true, value: %Date{} = date}), do:
    {Date.to_erl(date), {0, 0, 0}}
    |> :calendar.datetime_to_gregorian_seconds()
    |> Kernel.-(@epoch)
    |> DateTime.from_unix!()
  defp map_constant(%Expression{constant?: true, value: %Timex.Duration{} = duration}), do:
    Timex.Duration.to_seconds(duration)
  defp map_constant(%Expression{constant?: true, value: value}), do: value
  defp map_constant(_), do:
    DataSource.raise_error("Conditions on MongoDB data sources have to be between a column and a constant.")

  defp map_field(%Expression{name: field}) when is_binary(field), do: field
  defp map_field(_), do:
    DataSource.raise_error("Conditions on MongoDB data sources have to be between a column and a constant.")

  defp parse_where_condition({:and, lhs, rhs}), do:
    %{'$and': [parse_where_condition(lhs), parse_where_condition(rhs)]}
  defp parse_where_condition({:or, lhs, rhs}), do:
    %{'$or': [parse_where_condition(lhs), parse_where_condition(rhs)]}
  defp parse_where_condition({:comparison, subject, operator, value}), do:
    %{map_field(subject) => %{parse_operator(operator) => map_constant(value)}}
  defp parse_where_condition({:not, {:comparison, subject, :=, value}}), do:
    %{map_field(subject) => %{'$ne': map_constant(value)}}
  defp parse_where_condition({:not, {:comparison, subject, :<>, value}}), do:
    %{map_field(subject) => %{'$eq': map_constant(value)}}
  defp parse_where_condition({:is, subject, :null}), do: %{map_field(subject) => nil}
  defp parse_where_condition({:not, {:is, subject, :null}}), do: %{map_field(subject) => %{'$ne': nil}}
  defp parse_where_condition({:in, subject, values}), do:
    %{map_field(subject) => %{'$in': Enum.map(values, &map_constant/1)}}
  defp parse_where_condition({:not, {:in, subject, values}}), do:
    %{map_field(subject) => %{'$nin': Enum.map(values, &map_constant/1)}}
  defp parse_where_condition({:like, subject, pattern}), do:
    %{map_field(subject) => regex(pattern, "ms")}
  defp parse_where_condition({:ilike, subject, pattern}), do:
    %{map_field(subject) => regex(pattern, "msi")}
  defp parse_where_condition({:not, {:like, subject, pattern}}), do:
    %{map_field(subject) => %{'$not': regex(pattern, "ms")}}
  defp parse_where_condition({:not, {:ilike, subject, pattern}}), do:
    %{map_field(subject) => %{'$not': regex(pattern, "msi")}}

  defp regex(pattern, options), do:
    %BSON.Regex{pattern: LikePattern.to_regex_pattern(map_constant(pattern)), options: options}

  defp extract_basic_conditions(table, conditions) do
    {complex_conditions, basic_conditions} = Condition.partition(conditions, complex_filter(table.array_path))
    {table_conditions, other_tables_conditions} =
      Condition.partition(basic_conditions, &Condition.subject(&1).table.name == table.name)
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
      |> Lens.map(conditions, fn (column) ->
        index = Enum.find_index(extra_columns, & &1 == column)
        %Expression{name: "__condition_#{index}", type: column.type}
      end)
    extra_columns =
      extra_columns
      |> Enum.with_index()
      |> Enum.map(fn ({column, index}) ->
        %Expression{column | alias: "__condition_#{index}"}
      end)
    {conditions, extra_columns}
  end

  defp parse_conditions(existing_fields, conditions) do
    {conditions, extra_columns} = extract_columns_from_conditions(conditions)
    Projector.project_extra_columns(existing_fields, extra_columns) ++ filter_data(conditions)
  end

  defp unwind_arrays(_path, _path \\ "")
  defp unwind_arrays([], _path), do: []
  defp unwind_arrays([array | rest], path) do
    path = path <> array
    [%{'$unwind': "$" <> path} | unwind_arrays(rest, path)]
  end

  defp order_and_range(query), do:
    order_rows(query.order_by) ++
    offset_rows(query.offset) ++
    limit_rows(query.limit)

  defp order_rows([]), do: []
  defp order_rows(order_by) do
    order_by = for {column, dir, :nulls_natural} <- order_by, into: %{} do
      dir = if dir == :desc do -1 else 1 end
      {column.alias || column.name, dir}
    end
    [%{'$sort': order_by}]
  end

  defp offset_rows(0), do: []
  defp offset_rows(amount), do: [%{'$skip': amount}]

  defp limit_rows(nil), do: []
  defp limit_rows(amount), do: [%{'$limit': amount}]

  defp simple_order_by?(query), do:
    query
    |> Query.order_by_expressions()
    |> Enum.all?(& &1.name != nil)

  defp compile_columns(query) do
    # Complex columns referenced by the `ORDER BY` clause, that are not already selected,
    # need to be named and included in the projected columns list so that the `$sort` step can reference them.
    needed_columns =
      (query.db_columns ++ Query.order_by_expressions(query))
      |> Enum.uniq()
      |> Enum.with_index(1)
      |> Enum.map(fn ({column, index}) ->
        alias = column.alias || column.name || "__unknown_#{index}"
        %Expression{column | alias: alias}
      end)
    order_by = for {column, dir, nulls} <- query.order_by do
      column_with_alias = Enum.find(needed_columns, column, &%Expression{&1 | alias: nil} == column)
      {column_with_alias, dir, nulls}
    end
    %Query{query | db_columns: needed_columns, order_by: order_by}
  end

  defp extract_aggregator(%Expression{aggregate?: true} = column), do: [column]
  defp extract_aggregator(%Expression{function: fun} = column) when fun != nil,
    do: Enum.flat_map(column.function_args, &extract_aggregator/1)
  defp extract_aggregator(_column), do: []

  defp project_properties([]), do: nil
  defp project_properties(properties) do
    properties
    |> Enum.with_index()
    |> Enum.map(fn ({column, index}) ->
      Projector.project_column(%Expression{column | alias: "property_#{index}"})
    end)
    |> Enum.into(%{})
  end

  defp project_aggregators(aggregators) do
    aggregators
    |> Enum.with_index()
    |> Enum.map(fn ({column, index}) ->
      Projector.project_column(%Expression{column | alias: "aggregated_#{index}"})
    end)
    |> Enum.into(%{})
  end

  # This extracts the upper part of a column that need to be projected after grouping is done.
  defp extract_column_top(%Expression{constant?: true} = column, _aggregators, _groups), do: column
  defp extract_column_top(%Expression{function: "count", function_args: [{:distinct, _}]} = column, aggregators, _groups) do
    # For distinct count, we gather values into a set and then project the size of the set.
    index = Enum.find_index(aggregators, &Expression.equals(column, &1))
    %Expression{column | function?: true, function: "size",
      function_args: [%Expression{name: "aggregated_#{index}", table: :unknown}]
    }
  end
  defp extract_column_top(%Expression{function_args: [{:distinct, _}]} = column, aggregators, _groups) do
    # For distinct aggregators, we gather values into a set and then project the aggregator over the set.
    index = Enum.find_index(aggregators, &Expression.equals(column, &1))
    %Expression{column | function_args: [%Expression{name: "aggregated_#{index}", table: :unknown}]}
  end
  defp extract_column_top(column, aggregators, groups) do
    case Enum.find_index(aggregators, &Expression.equals(column, &1)) do
      nil ->
        case Enum.find_index(groups, &Expression.equals(column, &1)) do
          nil ->
            # Has to be a function call since the lookups failed.
            args = Enum.map(column.function_args, &extract_column_top(&1, aggregators, groups))
            %Expression{column | function_args: args}
          index ->
            %Expression{name: "_id.property_#{index}", table: :unknown, alias: column.alias || column.name}
        end
      index ->
        %Expression{name: "aggregated_#{index}", table: :unknown, alias: column.alias}
    end
  end

  defp extract_column_top_from_conditions(conditions, aggregators, groups), do:
    Query.Lenses.conditions()
    |> Query.Lenses.operands()
    |> Lens.map(conditions, &extract_column_top(&1, aggregators, groups))

  defp aggregate_and_project(%Query{db_columns: columns, group_by: groups, having: having} = query, top_level?) do
    having_columns =
      Query.Lenses.conditions()
      |> Lens.to_list(having)
      |> Enum.map(&Condition.subject/1)
    aggregators =
      (columns ++ having_columns)
      |> Enum.flat_map(&extract_aggregator/1)
      |> Enum.uniq()
    if aggregators ++ groups == [] do
      if simple_order_by?(query) do
        # if $sort and $limit steps are first, collection indexes might be used to speed up the pipeline
        order_and_range(query) ++ project_columns(columns, top_level?)
      else
        project_columns(columns, top_level?) ++ order_and_range(query)
      end
    else
      column_tops = Enum.map(columns, &extract_column_top(&1, aggregators, groups))
      properties = project_properties(groups)
      group = aggregators |> project_aggregators() |> Enum.into(%{"_id" => properties})
      having = extract_column_top_from_conditions(having, aggregators, groups)
      [%{'$group': group}] ++
      parse_conditions(Map.keys(group), having) ++
      project_columns(column_tops, top_level?) ++
      order_and_range(query)
    end
  end

  defp lookup_table(name, local_field, foreign_field, as) do
    [
      %{'$lookup': %{
          from: name,
          localField: local_field,
          foreignField: foreign_field,
          as: as
        }},
      %{'$unwind': %{path: "$" <> as}}
    ]
  end

  defp get_join_branch_name(name) when is_binary(name), do: name
  defp get_join_branch_name({:subquery, %{alias: name}}) when is_binary(name), do: name

  defp join_info(%{type: :inner_join, lhs: lhs, rhs: {:subquery, subquery}, conditions: condition}, tables), do:
    join_info(%{type: :inner_join, lhs: {:subquery, subquery}, rhs: lhs, conditions: condition}, tables)
  defp join_info(%{type: :inner_join, lhs: lhs, rhs: rhs_name, conditions: condition}, tables)
      when is_binary(rhs_name) do
    lhs_name = get_join_branch_name(lhs)
    {lhs_field, rhs_field} =
      case condition do
        {:comparison, %{table: %{name: ^lhs_name}} = local, :=, %{table: %{name: ^rhs_name}} = foreign} ->
          {local.name, foreign.name}
        {:comparison, %{table: %{name: ^rhs_name}} = foreign, :=, %{table: %{name: ^lhs_name}} = local} ->
          {local.name, foreign.name}
      end
    lhs_table = Enum.find(tables, & &1.name == lhs_name)
    rhs_table = Enum.find(tables, & &1.name == rhs_name)
    true = lhs_table != nil and rhs_table != nil
    %{lhs: lhs, lhs_table: lhs_table, rhs_table: rhs_table, lhs_field: lhs_field, rhs_field: rhs_field}
  end

  def used_array_size_columns(query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(& &1.name != nil and Schema.is_array_size?(&1.name))
    |> Lens.to_list(query)
  end
end
