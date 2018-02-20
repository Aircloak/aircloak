defmodule Cloak.DataSource.MongoDB.Projector do
  @moduledoc "MongoDB helper functions for projecting columns into the aggregation pipeline."

  alias Cloak.Sql.Expression
  alias Cloak.DataSource
  alias Cloak.DataSource.MongoDB.Schema


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates a MongoDB projection from a list of selected columns."
  @spec project_columns([Expression.t]) :: [map]
  def project_columns(columns), do:
    [%{'$project': columns |> Enum.map(&project_column/1) |> Enum.into(%{"_id" => false})}]

  @doc "Creates a MongoDB projection for a table containing array size columns."
  @spec project_array_sizes(map) :: [map]
  def project_array_sizes(%{columns: columns, collection: name}) when is_binary(name) do # table is collection
    columns
    |> Enum.map(&(&1.name))
    |> Enum.split_with(&Schema.is_array_size?/1)
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

  @doc "Creates a MongoDB projection that adds a set of extra columns needed for later filtering."
  @spec project_extra_columns(list, list) :: [map]
  def project_extra_columns(_existing_fields, []), do: []
  def project_extra_columns(existing_fields, extra_columns) do
    projected_columns =
      Enum.map(existing_fields, &{&1, true}) ++
      Enum.map(extra_columns, &project_column/1)
    [%{'$project': Enum.into(projected_columns, %{"_id" => false})}]
  end

  @doc "Creates a MongoDB projection from a column."
  @spec project_column(Expression.t) :: {String.t, atom | map}
  def project_column(%Expression{name: name, alias: nil}), do: {name, true}
  def project_column(%Expression{name: name, alias: name}), do: {name, true}
  def project_column(%Expression{aggregate?: true, function?: true, function: fun, function_args: [arg], alias: alias}), do:
    {project_alias(alias), parse_function(fun, begin_parse_column(arg))}
  def project_column(%Expression{constant?: true, alias: empty_alias} = constant) when empty_alias in [nil, ""], do:
    # We need to give an alias to unnamed constants
    project_column(%Expression{constant | alias: "alias_#{System.unique_integer([:positive])}"})
  def project_column(column), do: {project_alias(column.alias), begin_parse_column(column)}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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

  defp project_alias(name) do
    if not Expression.valid_alias?(name), do:
      DataSource.raise_error("MongoDB column alias `#{name}` contains invalid character(s).")
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
  defp parse_column(%Expression{constant?: true, value: %Timex.Duration{} = value}), do:
    %{'$literal': Timex.Duration.to_seconds(value)}
  defp parse_column(%Expression{constant?: true, value: value}), do: %{'$literal': value}
  defp parse_column(%Expression{function?: true, function: {:cast, type}, function_args: [value]}), do:
    parse_function("cast", [parse_column(value), value.type, type])
  defp parse_column(%Expression{function?: true, function: fun, function_args: [arg]}) when fun != nil, do:
    parse_function(fun, parse_column(arg))
  defp parse_column(%Expression{function?: true, function: fun, function_args: args}) when fun != nil, do:
    parse_function(fun, Enum.map(args, &parse_column/1))
  defp parse_column(%Expression{name: name}) when is_binary(name), do: "$" <> name

  defp parse_function("left", [string, count]), do:
    %{'$substrCP': [string, 0, count]}
  defp parse_function("right", [string, count]), do:
    %{'$substrCP': [string, %{'$max': [0, %{'$subtract': [%{'$strLenCP': string}, count]}]}, count]}
  defp parse_function("substring", [string, from]), do:
    %{'$substrCP': [string, %{'$subtract': [from, 1]}, %{'$strLenCP': string}]}
  defp parse_function("substring", [string, from, to]), do:
    %{'$substrCP': [string, %{'$subtract': [from, 1]}, to]}
  defp parse_function("count", :*), do: %{'$sum': 1}
  defp parse_function(_, {:distinct, value}), do: %{'$addToSet': value}
  defp parse_function("count", value), do:
    %{'$sum': %{'$cond': [%{'$gt': [value, nil]}, 1, 0]}}
  # We use the following formula for `quarter`: `div(integer_month - 1, 3) + 1`.
  # Note that integer division does not exist in MongoDB. We instead use a combination of division and floor.
  defp parse_function("quarter", value) do
    month_minus_1 = %{'$subtract': [%{'$month': value}, 1]}
    integer_devision_by_3 = %{'$floor': [%{'$divide': [month_minus_1, 3]}]}
    %{'$add': [integer_devision_by_3, 1]}
  end
  defp parse_function("div", args), do: %{'$trunc': %{'$divide': args}}
  defp parse_function("round", [value, decimals]) do
    scale = %{'$pow': [%{'$literal': 10}, decimals]}
    %{'$divide': [parse_function("round", %{'$multiply': [value, scale]}), scale]}
  end
  defp parse_function("round", value), do:
    %{'$trunc': %{'$add': [value, %{'$cond': [%{'$lt': [value, 0]}, - 0.5, 0.5]}]}}
  defp parse_function("trunc", [value, decimals]) do
    scale = %{'$pow': [%{'$literal': 10}, decimals]}
    %{'$divide': [%{'$trunc': %{'$multiply': [value, scale]}}, scale]}
  end
  for {name, translation} <- %{
    "*" => "$multiply", "/" => "$divide", "+" => "$add", "-" => "$subtract",
    "^" => "$pow", "%" => "$mod", "sqrt" => "$sqrt",
    "floor" => "$floor", "ceil" => "$ceil", "trunc" => "$trunc", "abs" => "$abs",
    "||" => "$concat", "concat" => "$concat", "length" => "$strLenCP",
    "lower" => "$toLower", "upper" => "$toUpper",
    "year" => "$year", "month" => "$month", "day" => "$dayOfMonth", "weekday" => "$dayOfWeek",
    "hour" => "$hour", "minute" => "$minute", "second" => "$second",
    "sum" => "$sum", "avg" => "$avg", "min" => "$min", "max" => "$max", "stddev" => "$stdDevPop", "size" => "$size",
  }, do:
    defp parse_function(unquote(name), args), do: %{unquote(translation) => args}
  defp parse_function("cast", [value, from, :text]) when from in [:real, :integer], do:
    %{'$substr': [value, 0, -1]}
  defp parse_function("cast", [value, :integer, :real]), do: value
  defp parse_function("cast", [value, :real, :integer]), do: parse_function("round", value)
  defp parse_function("cast", [value, :boolean, :integer]), do:
    %{'$cond': [%{'$eq': [value, nil]}, nil, %{'$cond': [value, 1, 0]}]}
  defp parse_function("cast", [value, :boolean, :real]), do:
    %{'$cond': [%{'$eq': [value, nil]}, nil, %{'$cond': [value, 1.0, 0.0]}]}
  defp parse_function("cast", [value, :boolean, :text]), do:
    %{'$cond': [%{'$eq': [value, nil]}, nil, %{'$cond': [value, "true", "false"]}]}
  defp parse_function("cast", [value, :integer, :boolean]), do:
    %{'$cond': [%{'$eq': [value, nil]}, nil, %{'$cond': [%{'$eq': [value, 0]}, false, true]}]}
  defp parse_function("cast", [value, :real, :boolean]), do:
    %{'$cond': [%{'$eq': [value, nil]}, nil, %{'$cond': [%{'$eq': [value, 0.0]}, false, true]}]}
  defp parse_function("cast", [value, :text, :boolean]), do:
    %{'$cond': [%{'$eq': [value, nil]}, nil, %{'$cond': [%{'$in':
      [%{'$toLower': value}, ["true", "yes", "1"]]}, true, false]}]}
  defp parse_function("cast", [value, :datetime, :text]), do:
    %{'$dateToString': %{format: "%Y-%m-%d %H:%M:%S.000%L", date: value}}
  defp parse_function("cast", [_value, from, to]), do:
    DataSource.raise_error("Casting from `#{from}` to `#{to}` is not supported in subqueries on MongoDB data sources.")
  defp parse_function(name, _args) when is_binary(name), do:
    DataSource.raise_error("Function `#{name}` is not supported in subqueries on MongoDB data sources.")
end
