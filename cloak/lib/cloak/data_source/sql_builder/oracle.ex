defmodule Cloak.DataSource.SqlBuilder.Oracle do
  @moduledoc "Helper module for converting a query to Oracle- specific SQL."

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
  def function_sql("bool_op", [[?', op, ?'], arg1, arg2]) do
    condition = [arg1, " ", op, " ", arg2]
    ["(CASE WHEN ", condition, " THEN 1 WHEN NOT (", condition, ") THEN 0 ELSE NULL END)"]
  end

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def unicode_literal(value), do: [?', value, ?']

  @impl Dialect
  def alias_sql(object, alias), do: [object, " ", alias]

  @impl Dialect
  def limit_sql(_, _), do: []

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:real), do: "BINARY_FLOAT"
  defp sql_type(:boolean), do: "NUMBER(1,0)"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:integer), do: "NUMBER"
  defp sql_type(:date), do: "DATE"
  defp sql_type(:text), do: "VARCHAR2"
  defp sql_type(:time), do: "INTERVAL DAY(0) TO SECOND(6)"
end
