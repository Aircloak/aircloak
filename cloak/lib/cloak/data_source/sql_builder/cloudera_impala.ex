defmodule Cloak.DataSource.SqlBuilder.ClouderaImpala do
  @moduledoc "Helper module for converting a query to Cloudera Data Platform (CDP) Impala specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.Query.ExecutionError

  @impl Dialect
  def supported_functions(), do: ~w(
    count sum min max avg stddev count_distinct variance
    < > <= >= = <> and or not in is_null
  )

  @impl Dialect
  def function_sql(name, args), do: super(name, args)

  @impl Dialect
  def literal(value), do: super(value)

  @impl Dialect
  def quote_char(), do: ?`

  @impl Dialect
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def cast_sql(_value, _from, _to), do: raise(ExecutionError, message: "No support for cast")
end
