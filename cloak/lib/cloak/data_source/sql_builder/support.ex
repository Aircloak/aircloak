defmodule Cloak.DataSource.SqlBuilder.Support do
  @moduledoc "Module for detecting when a query is supported by the SQL builder module."

  alias Cloak.Sql.{Query, Expression}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def supported_query?(query) do
    sql_dialect = query.data_source.driver_dialect
    used_functions = Query.Lenses.query_functions() |> Lens.to_list(query) |> Enum.map(&function_signature/1)
    not Enum.any?(used_functions, & &1 in unsupported_functions(sql_dialect))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @mysql_unsupported_functions ["date_trunc/2", "btrim/2", "ltrim/2", "rtrim/2"]
  @sqlserver_unsupported_functions ["date_trunc/2", "btrim/1", "btrim/2"]

  defp unsupported_functions(:mysql), do: @mysql_unsupported_functions
  defp unsupported_functions(:sqlserver), do: @sqlserver_unsupported_functions
  defp unsupported_functions(_sql_dialect), do: []

  defp function_signature(%Expression{function: name, function_args: args}) when is_binary(name), do:
    "#{name}/#{length(args)}"
  defp function_signature(_expression), do: nil # cast, bucket, etc.
end
