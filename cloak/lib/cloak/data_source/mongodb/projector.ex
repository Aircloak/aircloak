defmodule Cloak.DataSource.MongoDB.Projector do
  @moduledoc "MongoDB helper functions for projecting columns into the aggregation pipeline."

  alias Cloak.Aql.Column
  alias Cloak.Query.Runner.RuntimeError
  alias Cloak.DataSource.MongoDB.Schema


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc "Creates a MongoDB projection from a list of selected columns."
  @spec map_columns([Column.t]) :: [map]
  def map_columns(columns), do:
    [%{'$project': columns |> Enum.map(&map_column/1) |> Enum.into(%{"_id" => false})}]

  @doc "Creates a MongoDB projection for the array size columns in a table."
  @spec map_array_sizes(Cloak.DataSource.table) :: [map]
  def map_array_sizes(table) do
    columns = for {name, _type} <- table.columns, do: name
    case Enum.partition(columns, &Schema.is_array_size?/1) do
      {_, []} -> []
      {array_sizes, regular_columns} ->
        projected_columns =
          Enum.map(regular_columns, &{&1, true}) ++
          Enum.map(array_sizes, &{&1, &1 |> Schema.array_size_field() |> map_array_size()})
        [%{'$project': Enum.into(projected_columns, %{})}]
    end
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp map_array_size(name), do: %{'$size': %{'$ifNull': ["$" <> name, []]}}

  defp map_column(%Column{name: name, alias: nil}), do: {name, true}
  defp map_column(%Column{db_function: fun} = column) when fun != nil do
    non_null_args =
      column
      |> extract_fields()
      |> Enum.map(&%{'$gt': ["$" <> &1, nil]})
    {
      get_field_name(column.alias),
      %{'$cond': [%{'$and': non_null_args}, parse_column(column), nil]}
    }
  end
  defp map_column(column), do: {get_field_name(column.alias), parse_column(column)}

  defp valid_alias?(name), do:
    String.match?(name, ~r/^[a-zA-Z_#][a-zA-Z0-9_.#]*$/) and
    ! String.contains?(name, "..") and
    String.last(name) != "."

  defp get_field_name(nil), do: "__unknown_field_name_#{:erlang.unique_integer([:positive])}"
  defp get_field_name(""), do: get_field_name(nil)
  defp get_field_name(name) do
    if not valid_alias?(name), do:
      raise RuntimeError, message: "MongoDB column alias `#{name}` contains invalid character(s)."
    name
  end

  defp extract_fields(%Column{constant?: true}), do: []
  defp extract_fields(%Column{db_function: fun, db_function_args: args}) when fun != nil, do:
    Enum.flat_map(args, &extract_fields/1)
  defp extract_fields(%Column{name: name}) when is_binary(name), do: [name]

  defp parse_column(%Column{constant?: true, value: value}), do: %{'$literal': value}
  defp parse_column(%Column{db_function: "length", db_function_args: [%Column{name: name}]})
    when is_binary(name), do: "$" <> name <> ".length"
  defp parse_column(%Column{db_function: {:cast, type}, db_function_args: [value]}), do:
    parse_function("cast", [parse_column(value), value.type, type])
  defp parse_column(%Column{db_function: fun, db_function_args: args}) when fun != nil, do:
    parse_function(fun, Enum.map(args, &parse_column/1))
  defp parse_column(%Column{name: name}) when is_binary(name), do: "$" <> name

  defp parse_function("left", [string, count]), do: %{"$substr" => [string, 0, count]}
  defp parse_function("substring", [string, from]), do: %{"$substr" => [string, from, -1]}
  for {name, translation} <- %{
    "*" => "$multiply", "/" => "$divide", "+" => "$add", "-" => "$subtract",
    "^" => "$pow", "%" => "$mod", "mod" => "$mod", "sqrt" => "$sqrt",
    "floor" => "$floor", "ceil" => "$ceil", "trunc" => "$trunc", "abs" => "$abs",
    "||" => "$concat", "concat" => "$concat", "substring" => "$substr",
    "lower" => "$toLower", "lcase" => "$toLower", "upper" => "$toUpper", "ucase" => "$toUpper",
    "year" => "$year", "month" => "$month", "day" => "$dayOfMonth", "weekday" => "$dayOfWeek",
    "hour" => "$hour", "minute" => "$minute", "second" => "$second",
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
