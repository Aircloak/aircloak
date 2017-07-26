defmodule Cloak.DataSource.SqlBuilder.Support do
  @moduledoc "Module for detecting when a query is supported by the SQL builder module."

  alias Cloak.Sql.{Query, Expression}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def supported_query?(query) do
    sql_dialect = query.data_source.driver_dialect
    Query.Lenses.query_functions()
    |> Lens.to_list(query)
    |> Enum.map(&function_signature/1)
    |> Enum.all?(&supported_function?(&1, sql_dialect))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @postgresql_supported_functions ~w(
    count sum min max avg stddev
    year quarter month day hour minute second weekday date_trunc
    sqrt floor ceiling ceil abs round trunc div mod pow ^ * / + - %
    length lower lcase upper ucase btrim ltrim rtrim left right substring substring_for || concat
    hex cast coalesce
  )

  @mysql_supported_functions ~w(
    count sum min max avg stddev
    year quarter month day hour minute second weekday
    sqrt floor ceiling ceil abs round trunc div mod pow ^ * / + - %
    length lower lcase upper ucase btrim/1 ltrim/1 rtrim/1 left right substring substring_for || concat
    hex cast coalesce
  )

  @sqlserver_supported_functions ~w(
    count sum min max avg stddev
    year quarter month day hour minute second weekday
    sqrt floor ceiling ceil abs round trunc div mod pow ^ * / + - %
    length lower lcase upper ucase ltrim rtrim left right substring substring_for || concat
    hex cast coalesce
  )

  defp supported_functions(:postgresql), do: @postgresql_supported_functions
  defp supported_functions(:mysql), do: @mysql_supported_functions
  defp supported_functions(:sqlserver), do: @sqlserver_supported_functions
  defp supported_functions(_sql_dialect), do: []

  defp function_signature(%Expression{function: name, function_args: args}) when is_binary(name), do:
    {name, length(args)}
  defp function_signature(%Expression{function: {:cast, _target}, function_args: [_]}), do: {"cast", 1}
  defp function_signature(_expression), do: {nil, 0}

  defp supported_function?({name, args}, sql_dialect) do
    supported_functions = supported_functions(sql_dialect)
    name in supported_functions or "#{name}/#{args}" in supported_functions
  end
end
