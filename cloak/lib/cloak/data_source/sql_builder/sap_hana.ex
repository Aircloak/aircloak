defmodule Cloak.DataSource.SqlBuilder.SAPHana do
  @moduledoc "Helper module for converting a query to SAP HANA specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect

  @doc false
  def supported_functions(), do:
    ~w(
      count sum min max avg stddev
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round mod ^ % * / + -
      length lower upper btrim/1 ltrim rtrim left right substring substring_for concat
      cast coalesce bucket
    )

  @doc false
  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
  def function_sql("quarter", [arg]), do: ["cast(substring(quarter(", arg, "), 7, 1) as integer)"]
  def function_sql("%", args), do: function_sql("mod", args)
  def function_sql("^", args), do: function_sql("power", args)
  for binary_operator <- ~w(+ - * /) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  def function_sql("btrim", args), do: function_sql("trim", args)
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc false
  def sql_type(:text), do: "NCLOB"
  def sql_type(:datetime), do: "TIMESTAMP"
  def sql_type(type) when is_atom(type), do: String.upcase(Atom.to_string(type))

  @doc false
  def unicode_literal(value), do: ["N'", value, ?']
end
