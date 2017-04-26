defmodule Cloak.DataSource.MongoDB.Projector do
  @moduledoc "MongoDB helper functions for projecting columns into the aggregation pipeline."

  alias Cloak.Sql.Expression
  alias Cloak.Query.Runner.RuntimeError
  alias Cloak.DataSource.MongoDB.Schema


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc "Creates a MongoDB projection from a list of selected columns."
  @spec project_columns([Expression.t]) :: [map]
  def project_columns(columns), do:
    [%{'$project': columns |> Enum.map(&project_column/1) |> Enum.into(%{"_id" => false})}]

  @doc "Creates a MongoDB projection for a table containing array size columns."
  @spec project_array_sizes(map) :: [map]
  def project_array_sizes(%{columns: columns, db_name: name}) when is_binary(name) do # table is collection
    columns
    |> Enum.map(fn ({name, _type}) -> name end)
    |> Enum.partition(&Schema.is_array_size?/1)
    |> case do
      {[], _regular_columns} -> []
      {array_sizes, regular_columns} ->
        projected_columns =
          Enum.map(regular_columns, &{&1, true}) ++
          Enum.map(array_sizes, &{&1, &1 |> Schema.array_size_field() |> map_array_size()})
        [%{'$project': Enum.into(projected_columns, %{"_id" => false})}]
    end
  end
  def project_array_sizes(_table), do: [] # table is subquery

  @doc "Creates a MongoDB projection for the table and a set of extra columns needed for later filtering."
  @spec project_extra_columns(map, list) :: [map]
  def project_extra_columns(_table, []), do: []
  def project_extra_columns(%{columns: columns}, extra_columns) do
    projected_columns =
      Enum.map(columns, fn ({name, _type}) -> {name, true} end) ++
      Enum.map(extra_columns, &project_column/1)
    [%{'$project': Enum.into(projected_columns, %{"_id" => false})}]
  end

  @doc "Creates a MongoDB projection from a column."
  @spec project_column(Expression.t) :: {String.t, atom | map}
  def project_column(%Expression{name: name, alias: nil}), do: {name, true}
  def project_column(%Expression{aggregate?: true, function?: true, function: fun, function_args: [arg], alias: alias}), do:
    {get_field_name(alias), parse_function(fun, begin_parse_column(arg))}
  def project_column(column), do: {get_field_name(column.alias), begin_parse_column(column)}


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp map_array_size(name), do: %{'$size': %{'$ifNull': ["$" <> name, []]}}

  defp begin_parse_column(%Expression{function?: true, function: fun} = column) when fun != nil do
    column
    |> extract_fields()
    |> Enum.map(&%{'$gt': ["$" <> &1, nil]})
    |> case do
      [] -> parse_column(column)
      [non_null_arg] -> %{'$cond': [non_null_arg, parse_column(column), nil]}
      non_null_args -> %{'$cond': [%{'$and': non_null_args}, parse_column(column), nil]}
    end
  end
  defp begin_parse_column(column), do: parse_column(column)

  defp valid_alias?(name), do:
    String.match?(name, ~r/^[a-zA-Z_#][a-zA-Z0-9_.#]*$/) and
    not String.contains?(name, "..") and
    String.last(name) != "."

  defp get_field_name(nil), do: "__unknown_field_name_#{:erlang.unique_integer([:positive])}"
  defp get_field_name(""), do: get_field_name(nil)
  defp get_field_name(name) do
    if not valid_alias?(name), do:
      raise RuntimeError, message: "MongoDB column alias `#{name}` contains invalid character(s)."
    name
  end

  defp extract_fields({:distinct, column}), do: extract_fields(column)
  defp extract_fields(:*), do: []
  defp extract_fields(%Expression{constant?: true}), do: []
  defp extract_fields(%Expression{function?: true, function: fun, function_args: args}) when fun != nil, do:
    Enum.flat_map(args, &extract_fields/1)
  defp extract_fields(%Expression{name: name}) when is_binary(name), do: [name]

  defp parse_column(:*), do: :*
  defp parse_column({:distinct, column}), do: {:distinct, parse_column(column)}
  defp parse_column(%Expression{constant?: true, value: value}), do: %{'$literal': value}
  defp parse_column(%Expression{function?: true, function: "length", function_args: [%Expression{name: name}]})
    when is_binary(name), do: "$" <> name <> ".length"
  defp parse_column(%Expression{function?: true, function: {:cast, type}, function_args: [value]}), do:
    parse_function("cast", [parse_column(value), value.type, type])
  defp parse_column(%Expression{function?: true, function: fun, function_args: [arg]}) when fun != nil, do:
    parse_function(fun, parse_column(arg))
  defp parse_column(%Expression{function?: true, function: fun, function_args: args}) when fun != nil, do:
    parse_function(fun, Enum.map(args, &parse_column/1))
  defp parse_column(%Expression{name: name}) when is_binary(name), do: "$" <> name

  defp parse_function("left", [string, count]), do: %{"$substr" => [string, 0, count]}
  defp parse_function("substring", [string, from]), do: %{"$substr" => [string, from, -1]}
  defp parse_function("count", :*), do: %{'$sum': 1}
  defp parse_function(_, {:distinct, value}), do: %{'$addToSet': value}
  defp parse_function("count", value), do: %{'$sum': %{'$cond': [%{'$gt': [value, nil]}, 1, 0]}}
  for {name, translation} <- %{
    "*" => "$multiply", "/" => "$divide", "+" => "$add", "-" => "$subtract",
    "^" => "$pow", "pow" => "$pow", "%" => "$mod", "mod" => "$mod", "sqrt" => "$sqrt",
    "floor" => "$floor", "ceil" => "$ceil", "trunc" => "$trunc", "abs" => "$abs",
    "||" => "$concat", "concat" => "$concat", "substring" => "$substr",
    "lower" => "$toLower", "lcase" => "$toLower", "upper" => "$toUpper", "ucase" => "$toUpper",
    "year" => "$year", "month" => "$month", "day" => "$dayOfMonth", "weekday" => "$dayOfWeek",
    "hour" => "$hour", "minute" => "$minute", "second" => "$second",
    "sum" => "$sum", "avg" => "$avg", "min" => "$min", "max" => "$max", "stddev" => "$stdDevPop", "size" => "$size",
  }, do:
    defp parse_function(unquote(name), args), do: %{unquote(translation) => args}
  defp parse_function("cast", [value, :datetime, :text]), do:
    %{"$dateToString" => %{format: "%Y-%m-%d %H:%M:%S:%L", date: value}}
  defp parse_function("cast", [_value, from, to]), do:
    raise RuntimeError, message:
      "Casting from `#{from}` to `#{to}` is not supported in subqueries on MongoDB data sources."
  defp parse_function(name, _args) when is_binary(name), do:
    raise RuntimeError, message: "Function `#{name}` is not supported in subqueries on MongoDB data sources."
end
