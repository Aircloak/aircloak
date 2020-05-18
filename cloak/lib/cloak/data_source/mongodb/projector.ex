defmodule Cloak.DataSource.MongoDB.Projector do
  @moduledoc "MongoDB helper functions for projecting columns into the aggregation pipeline."

  alias Cloak.Sql.{Expression, LikePattern}
  alias Cloak.Query.ExecutionError
  alias Cloak.DataSource.MongoDB.Schema

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Creates a MongoDB projection from a list of selected columns."
  @spec project_columns([Expression.t()]) :: [map]
  def project_columns(columns),
    do: [%{"$project": columns |> Enum.map(&project_column/1) |> Enum.into(%{"_id" => false})}]

  @doc "Creates a MongoDB projection for a table containing array size columns."
  @spec project_array_sizes(map) :: [map]
  # table is collection
  def project_array_sizes(%{columns: columns, collection: name}) when is_binary(name) do
    columns
    |> Enum.map(& &1.name)
    |> Enum.filter(&Schema.is_array_size?/1)
    |> case do
      [] ->
        []

      array_sizes ->
        new_columns =
          array_sizes |> Enum.map(&{&1, &1 |> Schema.array_size_field() |> map_array_size()}) |> Enum.into(%{})

        [%{"$addFields": new_columns}]
    end
  end

  # table is subquery
  def project_array_sizes(_table), do: []

  @doc "Creates a MongoDB projection from a column."
  @spec project_column(Expression.t()) :: {String.t(), atom | map}
  def project_column(%Expression{kind: :column, table: %{name: table}, name: name, alias: alias})
      when alias in [nil, name],
      do: {name, "$#{table}.#{name}"}

  def project_column(%Expression{kind: :constant, alias: empty_alias} = constant)
      when empty_alias in [nil, ""],
      # We need to give an alias to unnamed constants
      do:
        project_column(%Expression{
          constant
          | alias: "alias_#{System.unique_integer([:positive])}"
        })

  def project_column(column), do: {project_alias(column.alias), parse_expression(column)}

  @doc "Parses an expression for inclusion in the MongoDB aggregation pipeline."
  @spec project_expression(Expression.t()) :: atom | map
  def project_expression(expression), do: parse_expression(expression)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp map_array_size(name), do: null_check("$" <> name, %{"$size": "$" <> name})

  defp project_alias(name) do
    if not Expression.valid_alias?(name),
      do: raise(ExecutionError, message: "MongoDB column alias `#{name}` contains invalid character(s).")

    name
  end

  defp parse_expression(:*), do: :*
  defp parse_expression({:distinct, expression}), do: {:distinct, parse_expression(expression)}

  @epoch :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
  defp parse_expression(%Expression{kind: :constant, value: %NaiveDateTime{} = datetime}),
    do: DateTime.from_naive!(datetime, "Etc/UTC")

  defp parse_expression(%Expression{kind: :constant, value: %Date{} = date}),
    do:
      {Date.to_erl(date), {0, 0, 0}}
      |> :calendar.datetime_to_gregorian_seconds()
      |> Kernel.-(@epoch)
      |> DateTime.from_unix!()

  defp parse_expression(%Expression{kind: :constant, value: %Timex.Duration{} = value}),
    do: %{"$literal": Timex.Duration.to_seconds(value)}

  defp parse_expression(%Expression{kind: :constant, type: :like_pattern, value: {pattern, _regex, _regex_ci}}),
    do: LikePattern.to_regex_pattern(pattern)

  defp parse_expression(%Expression{kind: :constant, value: value}), do: %{"$literal": value}

  defp parse_expression(%Expression{kind: :function, name: {:cast, type}, args: [value]}),
    do: parse_function("cast", [parse_expression(value), value.type, type])

  defp parse_expression(%Expression{kind: :function, name: name, args: [arg]}),
    do: parse_function(name, parse_expression(arg))

  defp parse_expression(%Expression{kind: :function, name: name, args: args}),
    do: parse_function(name, Enum.map(args, &parse_expression/1))

  defp parse_expression(%Expression{kind: :column, table: :unknown, name: name}), do: "$" <> name
  defp parse_expression(%Expression{kind: :column, table: %{name: table}, name: name}), do: "$#{table}.#{name}"

  # left of a negative value should return all but the last n characters.
  # left("aircloak", -2) --> "airclo"
  # left("aircloak", -100) --> ""
  defp parse_function("left", [value, %{"$literal": negative_length}]) when negative_length < 0 do
    substring_length =
      parse_function("+", [
        parse_function("length", [value]),
        negative_length
      ])

    null_check(
      value,
      parse_function("case", [
        %{"$gt": [substring_length, 0]},
        parse_function("substring", [value, 1, substring_length]),
        ""
      ])
    )
  end

  defp parse_function("left", [string, count]), do: null_check(string, count, %{"$substrCP": [string, 0, count]})

  # right of a negative value should return all but the first n characters
  # right("aircloak", -2) --> "rcloak"
  # right("aircloak", -100) --> ""
  defp parse_function("right", [value, %{"$literal": negative_length}]) when negative_length < 0 do
    substring_length =
      parse_function("+", [
        parse_function("length", [value]),
        negative_length
      ])

    null_check(
      value,
      parse_function("case", [
        %{"$gt": [substring_length, 0]},
        parse_function("substring", [
          value,
          parse_function("+", [
            parse_function("abs", [negative_length]),
            1
          ]),
          substring_length
        ]),
        ""
      ])
    )
  end

  defp parse_function("right", [string, count]),
    do:
      null_check(string, count, %{
        "$substrCP": [
          string,
          %{"$max": [0, %{"$subtract": [%{"$strLenCP": string}, count]}]},
          count
        ]
      })

  defp parse_function("substring", [string, from]),
    do: null_check(string, from, %{"$substrCP": [string, %{"$subtract": [from, 1]}, %{"$strLenCP": string}]})

  defp parse_function("substring", [string, from, to]),
    do: null_check(string, from, to, %{"$substrCP": [string, %{"$subtract": [from, 1]}, to]})

  defp parse_function("count", :*), do: %{"$sum": 1}
  defp parse_function(_, {:distinct, value}), do: %{"$addToSet": value}
  defp parse_function("count", value), do: %{"$sum": %{"$cond": [%{"$gt": [value, nil]}, 1, 0]}}
  # We use the following formula for `quarter`: `div(integer_month - 1, 3) + 1`.
  # Note that integer division does not exist in MongoDB. We instead use a combination of division and floor.
  defp parse_function("quarter", value) do
    month_minus_1 = %{"$subtract": [%{"$month": value}, 1]}
    integer_devision_by_3 = %{"$floor": [%{"$divide": [month_minus_1, 3]}]}
    %{"$add": [integer_devision_by_3, 1]}
  end

  defp parse_function("div", args), do: %{"$trunc": %{"$divide": args}}

  defp parse_function("round", [value, decimals]) do
    scale = %{"$pow": [%{"$literal": 10}, decimals]}
    %{"$divide": [parse_function("round", %{"$multiply": [value, scale]}), scale]}
  end

  defp parse_function("round", value),
    do: %{"$trunc": %{"$add": [value, %{"$cond": [%{"$lt": [value, 0]}, -0.5, 0.5]}]}}

  defp parse_function("trunc", [value, decimals]) do
    scale = %{"$pow": [%{"$literal": 10}, decimals]}
    %{"$divide": [%{"$trunc": %{"$multiply": [value, scale]}}, scale]}
  end

  for {name, translation} <- %{
        "length" => "$strLenCP",
        "lower" => "$toLower",
        "upper" => "$toUpper",
        "size" => "$size"
      },
      do: defp(parse_function(unquote(name), args), do: null_check(args, %{unquote(translation) => args}))

  for {name, translation} <- %{
        "*" => "$multiply",
        "unsafe_mul" => "$multiply",
        "+" => "$add",
        "unsafe_add" => "$add",
        "-" => "$subtract",
        "unsafe_sub" => "$subtract",
        "unsafe_div" => "$divide",
        "unsafe_mod" => "$mod",
        "floor" => "$floor",
        "ceil" => "$ceil",
        "trunc" => "$trunc",
        "abs" => "$abs",
        "||" => "$concat",
        "concat" => "$concat",
        "year" => "$year",
        "month" => "$month",
        "day" => "$dayOfMonth",
        "weekday" => "$dayOfWeek",
        "hour" => "$hour",
        "minute" => "$minute",
        "second" => "$second",
        "sum" => "$sum",
        "avg" => "$avg",
        "min" => "$min",
        "max" => "$max",
        "stddev" => "$stdDevSamp",
        "and" => "$and",
        "or" => "$or",
        "not" => "$not",
        "=" => "$eq",
        "!<>" => "$eq",
        "<>" => "$ne",
        ">" => "$gt",
        ">=" => "$gte",
        "<" => "$lt",
        "<=" => "$lte"
      },
      do: defp(parse_function(unquote(name), args), do: %{unquote(translation) => args})

  defp parse_function("checked_div", [dividend, divisor, _epsilon]), do: parse_function("/", [dividend, divisor])

  defp parse_function("/", [dividend, divisor]),
    do: %{
      "$cond": [
        %{"$eq": [divisor, 0]},
        nil,
        %{"$divide": [dividend, divisor]}
      ]
    }

  defp parse_function("checked_mod", [dividend, divisor]), do: parse_function("%", [dividend, divisor])

  defp parse_function("%", [dividend, divisor]),
    do: %{
      "$cond": [
        %{"$eq": [divisor, 0]},
        nil,
        %{"$mod": [dividend, divisor]}
      ]
    }

  defp parse_function("sqrt", value),
    do: %{
      "$cond": [
        %{"$lt": [value, 0]},
        nil,
        %{"$sqrt": value}
      ]
    }

  defp parse_function("^", [base, exponent]),
    do: %{
      "$cond": [
        %{"$lt": [base, 0]},
        nil,
        %{"$pow": [base, exponent]}
      ]
    }

  defp parse_function("variance", [value]), do: %{"$pow": [%{"$stdDevSamp" => [value]}, 2]}

  defp parse_function("cast", [value, from, :text]) when from in [:real, :integer],
    do: null_check(value, %{"$substr": [value, 0, -1]})

  defp parse_function("cast", [value, :integer, :real]), do: value

  defp parse_function("cast", [value, :boolean, :integer]),
    do: null_check(value, %{"$cond": [value, 1, 0]})

  defp parse_function("cast", [value, :boolean, :real]),
    do: null_check(value, %{"$cond": [value, 1.0, 0.0]})

  defp parse_function("cast", [value, :boolean, :text]),
    do: null_check(value, %{"$cond": [value, "true", "false"]})

  defp parse_function("cast", [value, :integer, :boolean]),
    do: null_check(value, %{"$cond": [%{"$eq": [value, 0]}, false, true]})

  defp parse_function("cast", [value, :real, :boolean]),
    do: null_check(value, %{"$cond": [%{"$eq": [value, 0.0]}, false, true]})

  defp parse_function("cast", [value, :text, :boolean]),
    do: %{
      "$cond": [
        %{"$in": [%{"$toLower": value}, ["true", "yes", "1", "t", "y"]]},
        true,
        %{"$cond": [%{"$in": [%{"$toLower": value}, ["false", "no", "0", "f", "n"]]}, false, nil]}
      ]
    }

  defp parse_function("cast", [value, :datetime, :text]),
    do: %{"$dateToString": %{format: "%Y-%m-%d %H:%M:%S.000%L", date: value}}

  defp parse_function("cast", [_value, from, to]) do
    raise ExecutionError,
      message: "Casting from `#{from}` to `#{to}` is not supported in subqueries on MongoDB data sources."
  end

  defp parse_function("is_null", arg), do: %{"$lte": [arg, nil]}
  defp parse_function("in", [subject | targets]), do: %{"$in": [subject, targets]}

  defp parse_function("like", [subject, regex]),
    do: %{"$regexMatch": %{input: subject, regex: regex, options: "ms"}}

  defp parse_function("ilike", [subject, regex]),
    do: %{"$regexMatch": %{input: subject, regex: regex, options: "msi"}}

  defp parse_function("coalesce", [arg]), do: arg
  defp parse_function("coalesce", [first | rest]), do: %{"$ifNull": [first, parse_function("coalesce", rest)]}

  defp parse_function("case", args) do
    {branches, default} = parse_case(args, [])

    %{
      "$switch": %{
        branches: branches,
        default: default
      }
    }
  end

  defp parse_function(name, _args) when is_binary(name),
    do: raise(ExecutionError, message: "Function `#{name}` is not supported in subqueries on MongoDB data sources.")

  defp parse_case([if_arg, then_arg | rest], branches),
    do: parse_case(rest, [%{case: if_arg, then: then_arg} | branches])

  defp parse_case([else_arg], branches), do: {Enum.reverse(branches), else_arg}

  defp null_check(value1, value2, %{"$literal": _}, expression), do: null_check(value1, value2, expression)

  defp null_check(value1, value2, value3, expression),
    do: %{
      "$cond": [
        %{"$or": [%{"$lte": [value1, nil]}, %{"$lte": [value2, nil]}, %{"$lte": [value3, nil]}]},
        nil,
        expression
      ]
    }

  defp null_check(value1, %{"$literal": _}, expression), do: null_check(value1, expression)

  defp null_check(value1, value2, expression),
    do: %{"$cond": [%{"$or": [%{"$lte": [value1, nil]}, %{"$lte": [value2, nil]}]}, nil, expression]}

  defp null_check(value, expression), do: %{"$cond": [%{"$lte": [value, nil]}, nil, expression]}
end
