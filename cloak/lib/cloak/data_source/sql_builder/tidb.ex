defmodule Cloak.DataSource.SqlBuilder.TiDB do
  @moduledoc "Helper module for converting a query to TiDB specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  alias Cloak.DataSource.SqlBuilder.MySQL
  use Cloak.DataSource.SqlBuilder.Dialect

  defdelegate supported_functions(), to: MySQL

  def function_sql("stddev", [arg]), do: function_sql("sqrt", [function_sql("variance", [arg])])
  def function_sql("variance", [arg]), do: ["VAR_POP(", arg, ") * (1.0 + CAST(1.0 AS double) / (COUNT(", arg, ") - 1))"]

  def function_sql("checked_div", [arg1, arg2, epsilon]),
    do: [
      "CASE WHEN ",
      function_sql("abs", [arg2]),
      " < ",
      epsilon,
      " THEN NULL ELSE ",
      function_sql("unsafe_div", [arg1, arg2]),
      " END"
    ]

  def function_sql("unsafe_div", [arg1, arg2]), do: ["(CAST(", arg1, " AS double) / ", arg2, ")"]
  def function_sql("ilike", [subject, pattern]), do: ["(LOWER(", subject, ") LIKE LOWER(", pattern, "))"]
  def function_sql(name, args), do: MySQL.function_sql(name, args)

  defdelegate limit_sql(limit, offset), to: MySQL

  defdelegate cast_sql(value, from, to), to: MySQL

  defdelegate literal(value), to: MySQL

  defdelegate order_by(column, direction, option), to: MySQL

  defdelegate quote_char(), to: MySQL

  def supports_overriding_pattern_escape?(), do: false
end
