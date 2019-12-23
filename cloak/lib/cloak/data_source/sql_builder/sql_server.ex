defmodule Cloak.DataSource.SqlBuilder.SQLServer do
  @moduledoc "Helper module for converting a query to SQL Server specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct variance
      < > <= >= = <> and or not in is_null like ilike
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc mod ^ * / + - %
      unsafe_pow unsafe_mul unsafe_div unsafe_add unsafe_sub unsafe_sub unsafe_mod
      checked_mod checked_div checked_pow
      length lower upper ltrim rtrim left right substring concat
      hex cast coalesce hash bool_op grouping_id case
    )

  @aliases %{
    "unsafe_pow" => "^",
    "checked_pow" => "^",
    "unsafe_mul" => "*",
    "unsafe_div" => "/",
    "unsafe_add" => "+",
    "unsafe_sub" => "-",
    "unsafe_mod" => "%",
    "checked_mod" => "%"
  }

  @impl Dialect
  for {function, alias} <- @aliases do
    def function_sql(unquote(function), args), do: function_sql(unquote(alias), args)
  end

  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["DATEPART(", unquote(datepart), ", ", args, ")"]
  end

  def function_sql("ceil", [arg]), do: ["CEILING(", arg, ")"]
  def function_sql("concat", args), do: Enum.intersperse(args, " + ")
  def function_sql("length", [arg]), do: ["(LEN(", arg, " + N'.') - 1)"]
  def function_sql("trunc", [arg]), do: ["ROUND(", arg, ", 0, 1)"]
  def function_sql("trunc", [arg1, arg2]), do: ["ROUND(", arg1, ",", arg2, ", 1)"]
  def function_sql("round", [arg]), do: ["ROUND(", arg, ", 0)"]

  def function_sql("hex", [arg]), do: ["LOWER(CONVERT(nvarchar, CAST(", arg, " AS varbinary), 2))"]

  def function_sql("hash", [arg]),
    do: function_sql("hex", [["SUBSTRING(HASHBYTES('md5', CAST(", arg, " AS varchar)), 3, 4)"]])

  def function_sql("bool_op", [["N'", op, ?'], arg1, arg2]) do
    condition = Dialect.bool_op_default(op, arg1, arg2)
    ["(CASE WHEN ", condition, " THEN 1 WHEN NOT (", condition, ") THEN 0 ELSE NULL END)"]
  end

  def function_sql("avg", [arg]), do: ["AVG(", cast_sql(arg, :numeric, :real), ")"]
  def function_sql("stddev", [arg]), do: ["STDEV(", cast_sql(arg, :numeric, :real), ")"]
  def function_sql("variance", [arg]), do: ["VAR(", cast_sql(arg, :numeric, :real), ")"]

  def function_sql("substring", [arg1, arg2]), do: ["SUBSTRING(", arg1, ", ", arg2, ", LEN(", arg1, "))"]

  def function_sql("^", [arg1, arg2]), do: ["POWER(", cast_sql(arg1, :numeric, :real), ", ", arg2, ")"]
  def function_sql("checked_div", [arg1, arg2, _epsilon]), do: function_sql("/", [arg1, arg2])
  def function_sql("/", [arg1, arg2]), do: ["(", cast_sql(arg1, :numeric, :real), " / ", arg2, ")"]

  for binary_operator <- ~w(+ - * %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql("case", args), do: ["CASE", case_branches(args), " END"]

  def function_sql("like", [subject, pattern]), do: [?(, subject, " COLLATE Latin1_General_CS_AS LIKE ", pattern, ?)]
  def function_sql("ilike", [subject, pattern]), do: [?(, subject, " COLLATE Latin1_General_CI_AS LIKE ", pattern, ?)]

  def function_sql(name, args), do: super(name, args)

  @impl Dialect
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset), " ROWS"]

  def limit_sql(limit, offset), do: [" OFFSET ", to_string(offset), " ROWS FETCH NEXT ", to_string(limit), " ROWS ONLY"]

  @impl Dialect
  def literal(%NaiveDateTime{} = value), do: [?', value |> NaiveDateTime.truncate(:millisecond) |> to_string(), ?']
  def literal(%Date{} = value), do: [?', to_string(value), ?']
  def literal(%Time{} = value), do: [?', to_string(value), ?']
  def literal(false), do: "0"
  def literal(true), do: "1"
  def literal(value) when is_binary(value), do: ["N'", value, ?']
  def literal(value), do: super(value)

  @impl Dialect
  def cast_sql(["DISTINCT " | value], from, to), do: ["DISTINCT " | cast_sql(value, from, to)]

  def cast_sql(value, :real, :integer),
    do: [
      "CASE WHEN ABS(",
      value,
      ") > #{@integer_range} THEN NULL ELSE CAST(",
      function_sql("round", [value]),
      " AS BIGINT) END"
    ]

  def cast_sql(value, :unknown, :text), do: ["TRY_CAST(", value, " AS varbinary)"]
  def cast_sql(value, _, type), do: ["TRY_CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def time_arithmetic_expression("+", [date, interval]), do: ["DATEADD(s, ", interval, ", ", date, ")"]

  def time_arithmetic_expression("-", [date, interval]), do: ["DATEADD(s, -(", interval, "), ", date, ")"]

  @impl Dialect
  def date_subtraction_expression([arg1, arg2]), do: ["DATEDIFF(s, ", arg2, ", ", arg1, ")"]

  @impl Dialect
  def order_by(column, :asc, :nulls_last), do: ["CASE WHEN ", column, " IS NULL THEN 1 ELSE 0 END, ", column, " ASC"]

  def order_by(column, :desc, :nulls_first), do: ["CASE WHEN ", column, " IS NULL THEN 0 ELSE 1 END, ", column, " DESC"]

  def order_by(column, :asc, _), do: [column, " ASC"]
  def order_by(column, :desc, _), do: [column, " DESC"]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:real), do: "float"
  defp sql_type(:boolean), do: "bit"
  defp sql_type(:integer), do: "bigint"
  # Due to limitations in the ODBC driver, we can't use nvarchar(max).
  # https://github.com/Aircloak/aircloak/pull/3111/commits/b6c59287bd6602de7d3cbf32592119a70e8f3e53#r219216993
  defp sql_type(:text), do: "nvarchar(4000)"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)

  defp case_branches([if_arg, then_arg | rest]), do: [" WHEN ", if_arg, " <> 0 THEN ", then_arg, case_branches(rest)]
  defp case_branches([else_branch]), do: [" ELSE ", else_branch]
end
