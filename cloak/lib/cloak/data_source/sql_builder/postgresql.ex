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
      variance variance_distinct
      year quarter month day hour minute second weekday date_trunc
      sqrt floor ceil abs round trunc mod ^ * / + - %
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

  def function_sql("/", [arg1, arg2]), do: ["(", arg1, " :: double precision / NULLIF(", arg2, ", 0))"]
  def function_sql("%", [arg1, arg2]), do: ["(", arg1, " % NULLIF(", arg2, ", 0))"]

  for binary_operator <- ~w(+ - *) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql("^", [arg1, arg2]), do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE POWER(", arg1, ", ", arg2, ") END"]

  def function_sql("sqrt", [arg]), do: ["CASE WHEN ", arg, " < 0 THEN NULL ELSE SQRT(", arg, ") END"]

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def ilike_sql(what, {pattern, escape = "\\"}), do: [what, " ILIKE ", ?', pattern, ?', " ESCAPE ", ?', escape, ?']

  @impl Dialect
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def cast_sql(value, :boolean, :integer), do: cast_sql(value, :boolean, :int32)

  def cast_sql(value, :real, :integer),
    do: ["CASE WHEN ABS(", value, ") > #{@integer_range} THEN NULL ELSE CAST(", value, " AS BIGINT) END"]

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

  def cast_sql(value, :text, :boolean),
    do: [
      "CASE WHEN TRIM(LOWER(",
      value,
      ")) IN ('1', 't', 'true', 'yes', 'y') THEN TRUE WHEN TRIM(LOWER(",
      value,
      ")) IN ('0', 'f', 'false', 'no', 'n') THEN FALSE ELSE NULL END"
    ]

  def cast_sql(value, :text, :integer),
    do: ["CASE WHEN ", value, " ~ '#{@is_integer_regex}' THEN CAST(", value, " AS BIGINT) ELSE NULL END"]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def literal(%Timex.Duration{} = value), do: ["interval '", interval_to_string(value), ?']
  def literal(value), do: Dialect.literal_default(value)

  @impl Dialect
  def select_table_names(prefix),
    do: "SELECT table_name FROM information_schema.tables WHERE table_name LIKE '#{prefix}%'"

  @impl Dialect
  def interval_division([arg1, arg2]), do: ["(", arg1, " / ", arg2, ")"]

  @impl Dialect
  def analyst_meta_table_create_statement(quoted_table_name) do
    """
    CREATE TABLE #{quoted_table_name} (
      "air" TEXT NOT NULL,
      "data_source" TEXT NOT NULL,
      "analyst" INTEGER NOT NULL,
      "name" TEXT NOT NULL,
      "db_name" TEXT NOT NULL,
      "statement" TEXT NOT NULL,
      "fingerprint" TEXT NOT NULL,
      PRIMARY KEY ("air", "data_source", "analyst", "name"),
      UNIQUE("db_name")
    )
    """
  end

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
