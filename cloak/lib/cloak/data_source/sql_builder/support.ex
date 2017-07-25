defmodule Cloak.DataSource.SqlBuilder.Support do
  @moduledoc "Module for detecting when a query is supported by the SQL builder module."

  alias Cloak.Sql.Query

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def supported_query?(query) do
    sql_dialect = query.data_source.driver_dialect
    used_functions = Query.Lenses.query_functions() |> Lens.to_list(query) |> Enum.map(& &1.function)
    not Enum.any?(used_functions, & &1 in unsupported_functions(sql_dialect))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @mysql_unsupported_functions ["date_trunc", "btrim", "ltrim", "rtrim"]
  @sqlserver_unsupported_functions ["date_trunc", "btrim"]

  defp unsupported_functions(:mysql), do: @mysql_unsupported_functions
  defp unsupported_functions(:sqlserver), do: @sqlserver_unsupported_functions
  defp unsupported_functions(_sql_dialect), do: []
end
