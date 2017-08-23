defmodule Cloak.DataSource.SqlBuilder.SAPHana do
  @moduledoc "Helper module for converting a query to SAP HANA specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  @behaviour Cloak.DataSource.SqlBuilder.Dialect

  @doc false
  def supported_functions(), do:
    ~w(
      count sum min max avg stddev
      year quarter month day hour minute second weekday
    )

  @doc false
  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
  def function_sql("quarter", [arg]), do: ["cast(substring(quarter(", arg, "), 7, 1) as integer)"]
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc false
  def sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
