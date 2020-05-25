defmodule Cloak.DataSource.SqlBuilder.TiDB do
  @moduledoc "Helper module for converting a query to TiDB specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  alias Cloak.DataSource.SqlBuilder.MySQL
  use Cloak.DataSource.SqlBuilder.Dialect

  def supported_functions(), do: MySQL.supported_functions()

  def function_sql("stddev", [arg]), do: function_sql("sqrt", [function_sql("variance", [arg])])
  def function_sql("variance", [arg]), do: ["VAR_POP(", arg, ")"]
  def function_sql(name, args), do: MySQL.function_sql(name, args)

  def limit_sql(limit, offset), do: MySQL.limit_sql(limit, offset)

  def cast_sql(value, from, to), do: MySQL.cast_sql(value, from, to)

  def literal(value), do: MySQL.literal(value)

  def order_by(column, direction, option), do: MySQL.order_by(column, direction, option)

  def quote_char(), do: ?`
end
