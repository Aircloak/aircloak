defmodule Cloak.DataSource.SqlBuilder.SAPIQ do
  @moduledoc "Helper module for converting a query to SAP IQ specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @max_limit 2_147_483_647

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct sum_distinct min_distinct max_distinct avg_distinct
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc mod div ^ % * / + -
      length lower upper btrim/1 ltrim/1 rtrim/1 left right substring concat
      cast coalesce hash bool_op
    )

  @impl Dialect
  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]
  def function_sql("weekday", [arg]), do: ["DOW(", arg, ")"]
  def function_sql("%", args), do: function_sql("mod", args)
  def function_sql("^", args), do: function_sql("power", args)
  def function_sql("btrim", [arg1]), do: function_sql("ltrim", [function_sql("rtrim", [arg1])])
  def function_sql("concat", [arg1, arg2]), do: function_sql("+", [arg1, arg2])
  def function_sql("trunc", [arg]), do: ["TRUNCNUM(", arg, ", 0)"]
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNCNUM(", arg1, ", ", arg2, ")"]
  def function_sql("round", [arg]), do: ["ROUND(", arg, ", 0)"]

  def function_sql("/", [arg1, arg2]),
    do: ["(", cast_sql(arg1, nil, :decimal), " / ", cast_sql(arg2, nil, :decimal), ")"]

  for binary_operator <- ~w(+ - *) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql("hash", [arg]), do: ["SUBSTRING(HASH(CAST(", arg, " AS text), 'md5'), 5, 8)"]

  def function_sql("bool_op", [["N'", op, ?'], arg1, arg2]) do
    condition = [arg1, " ", op, " ", arg2]
    ["(CASE WHEN ", condition, " THEN 1 WHEN NOT (", condition, ") THEN 0 ELSE NULL END)"]
  end

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def native_support_for_ilike?(), do: false

  @impl Dialect
  def limit_sql(nil, offset), do: limit_sql(@max_limit, offset)
  def limit_sql(limit, offset), do: [" TOP ", to_string(limit), " START AT ", to_string(offset + 1)]

  @impl Dialect
  def unicode_literal(value), do: ["N'", value, ?']

  @impl Dialect
  def boolean_literal(false), do: "0"
  def boolean_literal(true), do: "1"

  @impl Dialect
  def cast_sql(value, :real, :integer), do: ["CAST(ROUND(", value, ", 0) AS ", sql_type(:integer), ")"]

  def cast_sql(value, :boolean, :integer), do: value

  def cast_sql(value, :boolean, :text),
    do: ["(CASE WHEN ", value, " = 1 THEN 'true' WHEN ", value, " = 0 THEN 'false' ELSE NULL END)"]

  def cast_sql(value, type, :boolean) when type in [:integer, :real],
    do: ["(CASE WHEN ", value, " <> 0 THEN 1 WHEN ", value, " = 0 THEN 0 ELSE NULL END)"]

  def cast_sql(value, :text, :boolean),
    do: [
      "(CASE WHEN LOWER(",
      value,
      ") = 'true' THEN 1 WHEN LOWER(",
      value,
      ") = 'false' THEN 0 WHEN ",
      value,
      " = '1' THEN 1 WHEN ",
      value,
      " = '0' THEN 0 ELSE NULL END)"
    ]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def range_at_statement_start?(), do: true

  @impl Dialect
  def order_by(column, :asc, :nulls_last), do: ["CASE WHEN ", column, " IS NULL THEN 1 ELSE 0 END, ", column, " ASC"]
  def order_by(column, :desc, :nulls_first), do: ["CASE WHEN ", column, " IS NULL THEN 0 ELSE 1 END, ", column, " DESC"]
  def order_by(column, :asc, _), do: [column, " ASC"]
  def order_by(column, :desc, _), do: [column, " DESC"]

  @impl Dialect
  def time_arithmetic_expression("+", [date, interval]), do: ["DATEADD(ss, ", interval, ", ", date, ")"]

  def time_arithmetic_expression("-", [date, interval]), do: ["DATEADD(ss, -(", interval, "), ", date, ")"]

  @impl Dialect
  def date_subtraction_expression([arg1, arg2]), do: ["DATEDIFF(ss, ", arg2, ", ", arg1, ")"]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:text), do: "nvarchar"
  defp sql_type(:datetime), do: "timestamp"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
