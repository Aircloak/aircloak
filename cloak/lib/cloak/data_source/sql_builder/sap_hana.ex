defmodule Cloak.DataSource.SqlBuilder.SAPHana do
  @moduledoc "Helper module for converting a query to SAP HANA specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  @behaviour Cloak.DataSource.SqlBuilder.Dialect

  @doc false
  def supported_functions(), do:
    ~w()

  @doc false
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc false
  def sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
