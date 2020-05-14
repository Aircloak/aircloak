defmodule Cloak.DataSource.SqlBuilder.PostgreSQL do
  @moduledoc "Helper module for converting a query to PostgreSQL specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @aliases %{
    "+" => "ac_add",
    "*" => "ac_mul",
    "-" => "ac_sub"
  }

  @unsafe_operators %{
    "unsafe_add" => "+",
    "unsafe_sub" => "-",
    "unsafe_mul" => "*"
  }

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct variance
      < > <= >= = <> and or not in is_null like ilike !<>
      year quarter month day hour minute second weekday date_trunc
      sqrt floor ceil abs round trunc mod ^ * / + - %
      unsafe_pow unsafe_mul unsafe_div unsafe_add unsafe_sub unsafe_sub unsafe_mod
      checked_mod checked_div checked_pow
      length lower upper btrim ltrim rtrim left right substring concat
      hex cast coalesce grouping_id case
    )

  @impl Dialect
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("weekday", args), do: ["(EXTRACT(DOW FROM ", args, ") + 1)"]
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNC(CAST(", arg1, " AS decimal), ", arg2, ")"]
  def function_sql("round", [arg1, arg2]), do: ["ROUND(CAST(", arg1, " AS decimal), ", arg2, ")"]
  def function_sql("hex", [arg]), do: ["ENCODE(CONVERT_TO(", arg, ", 'utf8'), 'hex')"]

  def function_sql("boolean_expression", [arg]), do: arg

  def function_sql("unsafe_div", [arg1, arg2]), do: ["(", arg1, " :: double precision / ", arg2, ")"]

  def function_sql("checked_div", [arg1, arg2, epsilon]),
    do: ["CASE WHEN ABS(", arg2, ") < ", epsilon, " THEN NULL ELSE (", arg1, " :: double precision / ", arg2, ") END"]

  def function_sql("/", [arg1, arg2]),
    do: function_sql("pg_temp.ac_div", [[arg1, " :: double precision"], arg2])

  def function_sql("%", args), do: function_sql("checked_mod", args)
  def function_sql("checked_mod", [arg1, arg2]), do: ["MOD(", arg1, ", NULLIF(", arg2, ", 0))"]
  def function_sql("unsafe_mod", [arg1, arg2]), do: ["MOD(", arg1, ", ", arg2, ")"]

  for {function, operator} <- @unsafe_operators do
    def function_sql(unquote(function), [arg1, arg2]), do: ["(", arg1, unquote(operator), arg2, ")"]
  end

  for {function, alias} <- @aliases do
    def function_sql(unquote(function), args), do: function_sql("pg_temp.#{unquote(alias)}", args)
  end

  def function_sql("unsafe_pow", [arg1, arg2]), do: ["power(", arg1, ", ", arg2, ")"]

  def function_sql("checked_pow", [arg1, arg2]),
    do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE power(", arg1, ", ", arg2, ") END"]

  def function_sql("^", [arg1, arg2]),
    do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE pg_temp.ac_pow(", arg1, ", ", arg2, ") END"]

  def function_sql("sqrt", [arg]), do: ["CASE WHEN ", arg, " < 0 THEN NULL ELSE SQRT(", arg, ") END"]

  def function_sql("grouping_id", args), do: function_sql("grouping", args)

  def function_sql("case", args), do: Dialect.case_default(args)

  def function_sql("!<>", [arg1, arg2]), do: [arg1, " IS NOT DISTINCT FROM ", arg2]

  def function_sql(name, args), do: super(name, args)

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

  def cast_sql(value, :text, :boolean),
    do: [
      "CASE WHEN TRIM(LOWER(",
      value,
      ")) IN ('1', 't', 'true', 'yes', 'y') THEN TRUE WHEN TRIM(LOWER(",
      value,
      ")) IN ('0', 'f', 'false', 'no', 'n') THEN FALSE ELSE NULL END"
    ]

  def cast_sql(value, :text, to) when to in [:integer, :real, :date, :time, :datetime],
    do: ["pg_temp.ac_text_to_#{to}(", value, ")"]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def literal(%Timex.Duration{} = value), do: ["interval '", interval_to_string(value), ?']
  def literal(value), do: super(value)

  @impl Dialect
  def select_table_names(prefix),
    do: "SELECT table_name FROM information_schema.tables WHERE table_name LIKE '#{prefix}%'"

  @impl Dialect
  def date_subtraction_expression(:date, [arg1, arg2]),
    do: super(:datetime, [cast_sql(arg1, :date, :datetime), cast_sql(arg2, :date, :datetime)])

  def date_subtraction_expression(type, args), do: super(type, args)

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
