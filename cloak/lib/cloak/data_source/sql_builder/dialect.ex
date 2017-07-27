defmodule Cloak.DataSource.SqlBuilder.Dialect do
  @moduledoc "Specifies the interface for implementing dialect-specific SQL operations."

  alias Cloak.Sql.Expression

  @doc "Returns the list of supported functions for this SQL dialect."
  @callback supported_functions() :: [String.t]

  @doc "Generates dialect specific SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @callback function_sql(Expression.function_name, [iodata]) :: iodata

  @doc "Returns the dialect-specific SQL type for casting."
  @callback sql_type(atom) :: String.t
end
