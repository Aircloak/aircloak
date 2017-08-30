defmodule Cloak.DataSource.SqlBuilder.Dialect do
  @moduledoc "Specifies the interface for implementing dialect-specific SQL operations."

  alias Cloak.Sql.Expression

  @doc "Returns the list of supported functions for this SQL dialect."
  @callback supported_functions() :: [String.t]

  @doc "Generates dialect specific SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @callback function_sql(Expression.function_name, [iodata]) :: iodata

  @doc "Generates dialect specific SQL for the LIKE operator."
  @callback like_sql(iodata, iodata) :: iodata

  @doc "Generates dialect specific SQL for the ILIKE operator."
  @callback ilike_sql(iodata, iodata) :: iodata

  @doc "Generates dialect specific SQL for the ILIKE operator."
  @callback limit_sql(pos_integer | nil, non_neg_integer) :: iodata

  @doc "Generates dialect specific SQL for casting a column of an unknown type."
  @callback cast_unknown_sql(iodata) :: iodata

  @doc "Returns the dialect-specific SQL type for casting."
  @callback sql_type(atom) :: String.t

  alias Cloak.Query.ExecutionError

  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      @doc false
      def like_sql(what, match), do:
        [what, " LIKE ", match]

      @doc false
      def ilike_sql(what, match), do:
        # ilike requires the support for collation, so each data source must explicitly handle this
        raise ExecutionError, message: "ILIKE operator is not supported on this data source"

      @doc false
      def limit_sql(nil, _offset), do:
        # ilike requires the support for collation, so each data source must explicitly handle this
        raise ExecutionError, message: "OFFSET operator is not supported on this data source"
      def limit_sql(_limit, _offset), do:
        # ilike requires the support for collation, so each data source must explicitly handle this
        raise ExecutionError, message: "LIMIT operator is not supported on this data source"

      @doc false
      def cast_unknown_sql(column_sql), do:
        raise ExecutionError, message: "column `#{column_sql}` is of an unknown type"

      defoverridable like_sql: 2, ilike_sql: 2, limit_sql: 2, cast_unknown_sql: 1
    end
  end
end
