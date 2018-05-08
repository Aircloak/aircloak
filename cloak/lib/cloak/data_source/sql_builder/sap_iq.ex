defmodule Cloak.DataSource.SqlBuilder.SAPIQ do
  @moduledoc "Helper module for converting a query to SAP IQ specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct sum_distinct min_distinct max_distinct avg_distinct
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc mod div ^ % * / + -
      length lower upper btrim/1 ltrim/1 rtrim/1 left right substring concat
      cast coalesce
    )

  @impl Dialect
  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]
  def function_sql("weekday", [arg]), do: ["DOW(", arg, ")"]
  def function_sql("%", args), do: function_sql("mod", args)
  def function_sql("^", args), do: function_sql("power", args)
  def function_sql("btrim", [arg1]), do: function_sql("ltrim", [function_sql("rtrim", [arg1])])
  def function_sql("concat", [arg1, arg2]), do: function_sql("+", [arg1, arg2])

  def function_sql("/", [arg1, arg2]),
    do: ["(", cast_sql(arg1, nil, :decimal), " / ", cast_sql(arg2, nil, :decimal), ")"]

  for binary_operator <- ~w(+ - *) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def native_support_for_ilike?(), do: false

  @impl Dialect
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def unicode_literal(value), do: ["N'", value, ?']

  @impl Dialect
  def boolean_literal(false), do: "0"
  def boolean_literal(true), do: "1"

  @impl Dialect
  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:text), do: "nvarchar"
  defp sql_type(:datetime), do: "timestamp"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
