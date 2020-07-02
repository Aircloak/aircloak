defmodule Cloak.DataSource.SqlBuilder.TiDB do
  @moduledoc "Helper module for converting a query to TiDB specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  alias Cloak.DataSource.SqlBuilder.MySQL
  use Cloak.DataSource.SqlBuilder.Dialect

  defdelegate supported_functions(), to: MySQL

  def function_sql("stddev", [arg]), do: function_sql("sqrt", [function_sql("variance", [arg])])
  def function_sql("variance", [arg]), do: ["VAR_POP(", arg, ") * (1.0 + 1.0 / (COUNT(", arg, ") - 1))"]
  def function_sql(name, args), do: MySQL.function_sql(name, args)

  defdelegate limit_sql(limit, offset), to: MySQL

  defdelegate cast_sql(value, from, to), to: MySQL

  defdelegate literal(value), to: MySQL

  defdelegate order_by(column, direction, option), to: MySQL

  defdelegate quote_char(), to: MySQL

  def supports_overriding_pattern_escape?(), do: false
end
