defmodule Cloak.DataSource.MongoDB.Pipeline do
  @moduledoc "MongoDB helper functions for mapping a query to an aggregation pipeline."

  alias Cloak.Aql.{Query, Column, Comparison}
  alias Cloak.Query.Runner.RuntimeError
  alias Cloak.DataSource.MongoDB.Schema


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
      project_array_sizes(table) ++
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
    raise RuntimeError, message: "Table joins are not supported on 'mongodb' data sources."


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp parse_query(%Query{subquery?: false} = query), do:
    project_columns(query.db_columns)
  defp parse_query(%Query{group_by: [_|_]}), do:
    raise RuntimeError, message: "Grouping in subqueries is not supported on 'mongodb' data sources."
  defp parse_query(query), do:
    project_columns(query.db_columns) ++
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
  defp parse_operator(:<>), do: :'$neq'

  defp map_parameter(%NaiveDateTime{} = datetime) do
    {date, {hour, minute, second}} = NaiveDateTime.to_erl(datetime)
    BSON.DateTime.from_datetime({date, {hour, minute, second, datetime.microsecond}})
  end
  defp map_parameter(%Date{} = date), do:
    BSON.DateTime.from_datetime({Date.to_erl(date), {0, 0, 0, 0}})
  defp map_parameter(%Column{value: value}), do: value

  defp parse_where_condition({:comparison, %Column{name: field}, operator, value}), do:
    %{field => %{parse_operator(operator) => map_parameter(value)}}
  defp parse_where_condition({:not, {:comparison, %Column{name: field}, :=, value}}), do:
    %{field => %{'$neq': map_parameter(value)}}
  defp parse_where_condition({:is, %Column{name: field}, :null}), do: %{field => nil}
  defp parse_where_condition({:not, {:is, %Column{name: field}, :null}}), do: %{field => %{'$exists': true}}
  defp parse_where_condition({:in, %Column{name: field}, values}), do:
    %{field => %{'$in': Enum.map(values, &map_parameter/1)}}
  defp parse_where_condition({:not, {:in, %Column{name: field}, values}}), do:
    %{field => %{'$nin': Enum.map(values, &map_parameter/1)}}
  defp parse_where_condition({:like, %Column{name: field}, %Column{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}
  defp parse_where_condition({:ilike, %Column{name: field}, %Column{value: pattern}}), do:
    %{field => %{'$regex': Comparison.to_regex(pattern), '$options': "msi"}}
  defp parse_where_condition({:not, {:like, %Column{name: field}, %Column{value: pattern}}}), do:
    %{field => %{'$not': %{'$regex': Comparison.to_regex(pattern), '$options': "ms"}}}
  defp parse_where_condition({:not, {:ilike, %Column{name: field}, %Column{value: pattern}}}), do:
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

  defp array_size_projector(name), do: %{'$size': %{'$ifNull': ["$" <> name, []]}}

  defp project_array_sizes(table) do
    columns = for {name, _type} <- table.columns, do: name
    case Enum.partition(columns, &Schema.is_array_size?/1) do
      {_, []} -> []
      {array_sizes, regular_columns} ->
        projected_columns =
          Enum.map(regular_columns, &{&1, true}) ++
          Enum.map(array_sizes, &{&1, &1 |> Schema.array_size_field() |> array_size_projector()})
        [%{'$project': Enum.into(projected_columns, %{})}]
    end
  end

  defp project_columns(columns), do:
    [%{'$project': columns |> Enum.map(&project_column/1) |> Enum.into(%{"_id" => false})}]

  defp project_column(%Column{name: name, alias: nil}), do: {name, true}
  defp project_column(%Column{name: name, alias: alias}) do
    unless valid_alias?(alias), do:
      raise RuntimeError, message: "MongoDB column alias `#{alias}` contains invalid character(s)."
    {alias, "$" <> name}
  end

  defp valid_alias?(name), do:
    String.match?(name, ~r/^[a-zA-Z_#][a-zA-Z0-9_.#]*$/) and
    ! String.contains?(name, "..") and
    String.last(name) != "."

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
end
