defmodule Cloak.DataSource.MongoDB.Pipeline do
  @moduledoc "MongoDB helper functions for mapping a query to an aggregation pipeline."

  alias Cloak.Sql.{Query, Expression, Comparison}
  alias Cloak.Query.Runner.RuntimeError
  alias Cloak.DataSource.MongoDB.{Schema, Projector}


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc "Builds a MongoDB aggregation pipeline from a compiled query."
  @spec build(Query.t) :: {String.t, [map]}
  def build(%Query{from: table_name, selected_tables: [table]} = query) when is_binary(table_name) do
    {base_conditions, array_conditions, array_size_conditions} = split_conditions(table.array_path, query.where)
    pipeline =
      parse_where_conditions(base_conditions) ++
      unwind_arrays(table.array_path) ++
      parse_where_conditions(array_conditions) ++
      Projector.map_array_sizes(table) ++
      parse_where_conditions(array_size_conditions) ++
      parse_query(query)
    {table.db_name, pipeline}
  end
  def build(%Query{from: {:subquery, subquery}} = query) do
    {collection, pipeline} = build(subquery.ast)
    pipeline =
      pipeline ++
      parse_where_conditions(query.where) ++
      parse_query(query)
    {collection, pipeline}
  end
  def build(%Query{from: {:join, _}}), do:
    raise RuntimeError, message: "Table joins are not supported on MongoDB data sources."


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp parse_query(%Query{subquery?: false} = query), do:
    Projector.map_columns(query.db_columns)
  defp parse_query(%Query{subquery?: true} = query), do:
    aggregate_and_project(query) ++
    order_rows(query.order_by, query.db_columns) ++
    offset_rows(query.offset) ++
    limit_rows(query.limit)

  defp parse_where_conditions([]), do: []
  defp parse_where_conditions([condition]), do: [%{'$match': parse_where_condition(condition)}]
  defp parse_where_conditions(conditions), do: [%{'$match': %{'$and': Enum.map(conditions, &parse_where_condition/1)}}]

  defp parse_operator(:=), do: :'$eq'
  defp parse_operator(:>), do: :'$gt'
  defp parse_operator(:>=), do: :'$gte'
  defp parse_operator(:<), do: :'$lt'
  defp parse_operator(:<=), do: :'$lte'
  defp parse_operator(:<>), do: :'$ne'

  @dialyzer {:nowarn_function, map_parameter: 1} # https://github.com/elixir-lang/elixir/issues/5634
  defp map_parameter(%NaiveDateTime{} = datetime) do
    {date, {hour, minute, second}} = NaiveDateTime.to_erl(datetime)
    {usec, _precision} = datetime.microsecond
    BSON.DateTime.from_datetime({date, {hour, minute, second, usec}})
  end
  defp map_parameter(%Date{} = date), do:
    BSON.DateTime.from_datetime({Date.to_erl(date), {0, 0, 0, 0}})
  defp map_parameter(%Expression{value: value}), do: value

  defp parse_where_condition({:comparison, %Expression{name: field}, operator, value}), do:
    %{field => %{parse_operator(operator) => map_parameter(value)}}
  defp parse_where_condition({:not, {:comparison, %Expression{name: field}, :=, value}}), do:
    %{field => %{'$ne': map_parameter(value)}}
  defp parse_where_condition({:is, %Expression{name: field}, :null}), do: %{field => nil}
  defp parse_where_condition({:not, {:is, %Expression{name: field}, :null}}), do: %{field => %{'$exists': true}}
  defp parse_where_condition({:in, %Expression{name: field}, values}), do:
    %{field => %{'$in': Enum.map(values, &map_parameter/1)}}
  defp parse_where_condition({:not, {:in, %Expression{name: field}, values}}), do:
    %{field => %{'$nin': Enum.map(values, &map_parameter/1)}}
  defp parse_where_condition({:like, %Expression{name: field}, %Expression{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}
  defp parse_where_condition({:ilike, %Expression{name: field}, %Expression{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "msi"}}
  defp parse_where_condition({:not, {:like, %Expression{name: field}, %Expression{value: pattern}}}), do:
    %{field => %{'$not': %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}}
  defp parse_where_condition({:not, {:ilike, %Expression{name: field}, %Expression{value: pattern}}}), do:
    %{field => %{'$not': %{'$regex': Comparison.to_regex(pattern), '$options': "msi"}}}

  defp split_conditions([], conditions) do
    {array_size_conditions, non_array_size_conditions} =
      Enum.partition(conditions, &Comparison.subject(&1).name |> Schema.is_array_size?())
    {non_array_size_conditions, [], array_size_conditions}
  end
  defp split_conditions([array | _], conditions) do
    {array_size_conditions, non_array_size_conditions} =
      Enum.partition(conditions, &Comparison.subject(&1).name |> Schema.is_array_size?())
    {array_conditions, base_conditions} =
      Enum.partition(non_array_size_conditions, &Comparison.subject(&1).name |> String.starts_with?(array <> "."))
    {base_conditions, array_conditions, array_size_conditions}
  end

  defp unwind_arrays(_path, _path \\ "")
  defp unwind_arrays([], _path), do: []
  defp unwind_arrays([array | rest], path) do
    path = path <> array
    [%{'$unwind': "$" <> path} | unwind_arrays(rest, path)]
  end

  defp order_rows([], _columns), do: []
  defp order_rows(order_by, columns) do
    order_by = for {index, dir} <- order_by, into: %{} do
      dir = if dir == :desc do -1 else 1 end
      name = columns |> Enum.at(index) |> Map.get(:alias)
      {name, dir}
    end
    [%{'$sort': order_by}]
  end

  defp offset_rows(0), do: []
  defp offset_rows(amount), do: [%{'$skip': amount}]

  defp limit_rows(nil), do: []
  defp limit_rows(amount), do: [%{'$limit': amount}]

  defp extract_aggregator(%Expression{aggregate?: true} = column), do: [column]
  defp extract_aggregator(%Expression{function: fun} = column) when fun != nil,
    do: Enum.flat_map(column.function_args, &extract_aggregator/1)
  defp extract_aggregator(_column), do: []

  defp project_properties([]), do: nil
  defp project_properties(properties) do
    properties
    |> Enum.with_index()
    |> Enum.map(fn ({column, index}) ->
      Projector.map_column(%Expression{column | alias: "property_#{index}"})
    end)
    |> Enum.into(%{})
  end

  defp project_aggregators(aggregators) do
    aggregators
    |> Enum.with_index()
    |> Enum.map(fn ({column, index}) ->
      Projector.map_column(%Expression{column | alias: "aggregated_#{index}"})
    end)
    |> Enum.into(%{})
  end

  # This extracts the upper part of a column that need to be projected after grouping is done.
  defp extract_column_top(%Expression{constant?: true} = column, _aggregators, _groups), do: column
  defp extract_column_top(%Expression{function: "count", function_args: [{:distinct, _}]} = column, aggregators, _groups) do
    # For distinct count, we gather values into a set and then project the size of the set.
    index = Enum.find_index(aggregators, &Expression.equals(column, &1))
    %Expression{name: "aggregated_#{index}#", table: :unknown, alias: column.alias}
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

  defp extract_column_top_from_condition({:not, condition}, aggregators), do:
    {:not, extract_column_top_from_condition(condition, aggregators)}
  defp extract_column_top_from_condition({:comparison, lhs, operator, rhs}, aggregators), do:
    {:comparison, extract_column_top(lhs, aggregators, []), operator, rhs}
  defp extract_column_top_from_condition({verb, lhs, rhs}, aggregators) when verb in [:in, :is, :like, :ilike], do:
    {verb, extract_column_top(lhs, aggregators, []), rhs}

  defp aggregate_and_project(%Query{db_columns: columns, distinct?: true}) do
    properties = project_properties(columns)
    column_tops = Enum.map(columns, &extract_column_top(&1, [], columns))
    [%{'$group': %{"_id" => properties}}] ++ Projector.map_columns(column_tops)
  end
  defp aggregate_and_project(%Query{db_columns: columns, group_by: groups, having: having}) do
    aggregators =
      (columns ++ Enum.map(having, &Comparison.subject/1))
      |> Enum.flat_map(&extract_aggregator/1)
      |> Enum.uniq()
    if aggregators ++ groups == [] do
      Projector.map_columns(columns)
    else
      column_tops = Enum.map(columns, &extract_column_top(&1, aggregators, groups))
      properties = project_properties(groups)
      group = aggregators |> project_aggregators() |> Enum.into(%{"_id" => properties})
      having = Enum.map(having, &extract_column_top_from_condition(&1, aggregators))
      [%{'$group': group}] ++ parse_where_conditions(having) ++ Projector.map_columns(column_tops)
    end
  end
end
