defmodule Cloak.DataSource.SqlBuilder.ClouderaImpala do
  @moduledoc "Helper module for converting a query to Cloudera Data Platform (CDP) Impala specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @unsafe_operators %{
    "unsafe_add" => "+",
    "unsafe_sub" => "-",
    "unsafe_mul" => "*",
    "unsafe_div" => "/",
    "unsafe_mod" => "%"
  }

  @impl Dialect
  def supported_functions(), do: ~w(
    count sum min max avg stddev count_distinct variance
    < > <= >= = <> and or not in is_null
    unsafe_pow unsafe_add unsafe_sub unsafe_mul unsafe_div unsafe_mod
  )

  @impl Dialect
  def function_sql("unsafe_pow", [arg1, arg2]), do: ["pow(", arg1, ", ", arg2, ")"]

  for {function, operator} <- @unsafe_operators do
    def function_sql(unquote(function), [arg1, arg2]), do: ["(", arg1, unquote(operator), arg2, ")"]
  end

  def function_sql(name, args), do: super(name, args)

  @impl Dialect
  def literal(value), do: super(value)

  @impl Dialect
  def quote_char(), do: ?`

  @impl Dialect
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # 8 byte binary float
  defp sql_type(:real), do: "DOUBLE"
  # 8 byte binary integer
  defp sql_type(:integer), do: "BIGINT"
  defp sql_type(:boolean), do: "BOOLEAN"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:date), do: "TIMESTAMP"
  defp sql_type(:text), do: "STRING"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
