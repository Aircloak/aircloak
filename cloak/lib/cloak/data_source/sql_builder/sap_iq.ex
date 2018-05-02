defmodule Cloak.DataSource.SqlBuilder.SAPIQ do
  @moduledoc "Helper module for converting a query to SAP IQ specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @impl Dialect
  def supported_functions(), do: ~w()

  @impl Dialect
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def native_support_for_ilike?(), do: false

  @impl Dialect
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def unicode_literal(value), do: ["N'", value, ?']

  @impl Dialect
  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:text), do: "varchar"
  defp sql_type(:datetime), do: "timestamp"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
