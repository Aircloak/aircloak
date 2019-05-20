defmodule Cloak.DataSource.SqlBuilder.SAPHana do
  @moduledoc "Helper module for converting a query to SAP HANA specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @max_unsigned_bigint 9_223_372_036_854_775_807

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev variance count_distinct sum_distinct min_distinct max_distinct avg_distinct
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc mod
      length lower upper btrim/1 ltrim rtrim left right substring concat
      cast coalesce bool_op hash
    )

  @impl Dialect
  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("quarter", [arg]), do: ["CAST(SUBSTRING(QUARTER(", arg, "), 7, 1) AS integer)"]

  def function_sql("sqrt", [arg]), do: ["CASE WHEN ", arg, " < 0 THEN NULL ELSE SQRT(", arg, ") END"]

  def function_sql("round", [arg]), do: ["ROUND(", arg, ", 0, ROUND_HALF_UP)"]
  def function_sql("round", [arg1, arg2]), do: ["ROUND(", arg1, ", ", arg2, ", ROUND_HALF_UP)"]
  def function_sql("trunc", [arg]), do: ["ROUND(", arg, ", 0, ROUND_DOWN)"]
  def function_sql("trunc", [arg1, arg2]), do: ["ROUND(", arg1, ", ", arg2, ", ROUND_DOWN)"]
  def function_sql("btrim", args), do: function_sql("trim", args)

  def function_sql("avg", [["DISTINCT " <> _ | _] = arg]),
    do: [
      "AVG(DISTINCT TO_DECIMAL(",
      arg |> to_string() |> String.replace(~r/DISTINCT /, ""),
      "))"
    ]

  def function_sql("hash", [arg]), do: ["LOWER(SUBSTR(HASH_MD5(CAST(", arg, " AS binary)), 3, 4))"]

  def function_sql("bool_op", [["N'", op, ?'], arg1, arg2]) do
    condition = [arg1, " ", op, " ", arg2]
    ["(CASE WHEN ", condition, " THEN 1 WHEN NOT (", condition, ") THEN 0 ELSE NULL END)"]
  end

  def function_sql("avg", [arg]), do: ["AVG(TO_DECIMAL(", arg, "))"]
  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]
  def function_sql("variance", [arg]), do: ["VAR_SAMP(", arg, ")"]

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def native_support_for_ilike?(), do: false

  @impl Dialect
  def limit_sql(nil, offset), do: limit_sql(@max_unsigned_bigint, offset)
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def literal(value) when is_binary(value), do: ["N'", value, ?']
  def literal(value), do: Dialect.literal_default(value)

  @impl Dialect
  def cast_sql(value, :real, :integer),
    do: [
      "CASE WHEN ABS(",
      value,
      ") > #{@integer_range} THEN NULL ELSE CAST(",
      function_sql("round", [value]),
      " AS BIGINT) END"
    ]

  def cast_sql(value, from, :boolean) when from in [:real, :integer],
    do: ["CASE WHEN ", value, " != 0 THEN TRUE WHEN ", value, " = 0 THEN FALSE ELSE NULL END"]

  def cast_sql(value, :text, :boolean),
    do: [
      "CASE WHEN TRIM(LOWER(",
      value,
      ")) IN ('1', 't', 'true', 'yes', 'y') THEN TRUE WHEN TRIM(LOWER(",
      value,
      ")) IN ('0', 'f', 'false', 'no', 'n') THEN FALSE ELSE NULL END"
    ]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def time_arithmetic_expression("+", [date, interval]), do: ["ADD_SECONDS(", date, ", ", interval, ")"]

  def time_arithmetic_expression("-", [date, interval]), do: ["ADD_SECONDS(", date, ", -(", interval, "))"]

  @impl Dialect
  def date_subtraction_expression([arg1, arg2]), do: ["SECONDS_BETWEEN(", arg2, ", ", arg1, ")"]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:text), do: "nvarchar"
  defp sql_type(:datetime), do: "timestamp"
  defp sql_type(:integer), do: "bigint"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
