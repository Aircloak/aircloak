defmodule Cloak.DataSource.SqlBuilder.PostgreSQL do
  @moduledoc "Helper module for converting a query to PostgreSQL specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct sum_distinct min_distinct max_distinct avg_distinct stddev_distinct
      year quarter month day hour minute second weekday date_trunc
      sqrt floor ceil abs round trunc div mod ^ * / + -
      length lower upper btrim ltrim rtrim left right substring concat
      hex cast coalesce hash bool_op
    )

  @impl Dialect
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("weekday", args), do: ["EXTRACT(DOW FROM ", args, ")"]
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNC(CAST(", arg1, " AS decimal), ", arg2, ")"]
  def function_sql("round", [arg1, arg2]), do: ["ROUND(CAST(", arg1, " AS decimal), ", arg2, ")"]
  def function_sql("hex", [arg]), do: ["ENCODE(CONVERT_TO(", arg, ", 'utf8'), 'hex')"]

  def function_sql("hash", [arg]), do: ["SUBSTR(MD5(", arg, "::text), 5, 8)"]

  def function_sql("bool_op", [[?', op, ?'], arg1, arg2]), do: ["(", arg1, " ", op, " ", arg2, ")"]

  def function_sql("/", [arg1, arg2]), do: ["(", arg1, " :: double precision / ", arg2, ")"]

  for binary_operator <- ~w(+ - * ^ %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def ilike_sql(what, match), do: [what, " ILIKE ", match]

  @impl Dialect
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def cast_sql(value, :boolean, :integer), do: cast_sql(value, :boolean, :int32)

  def cast_sql(value, :boolean, :real), do: value |> cast_sql(:boolean, :integer) |> cast_sql(:integer, :real)

  def cast_sql(value, :integer, :boolean),
    do: ["CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " = 0 THEN FALSE ELSE TRUE END"]

  def cast_sql(value, :real, :boolean),
    do: [
      "CASE WHEN ",
      value,
      " IS NULL THEN NULL WHEN ",
      value,
      " = 0.0 THEN FALSE ELSE TRUE END"
    ]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def unicode_literal(value), do: [?', value, ?']

  @impl Dialect
  def interval_literal(value), do: ["interval '", interval_to_string(value), ?']

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:real), do: "float"
  defp sql_type(:boolean), do: "bool"
  defp sql_type(:datetime), do: "timestamp"
  defp sql_type(:integer), do: "bigint"
  defp sql_type(:int32), do: "integer"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)

  def interval_to_string(interval) do
    case Timex.Duration.to_string(interval) do
      "P" -> "P0"
      interval -> interval
    end
  end
end
