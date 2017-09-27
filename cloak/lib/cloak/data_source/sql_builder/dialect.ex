defmodule Cloak.DataSource.SqlBuilder.Dialect do
  @moduledoc "Specifies the interface for implementing dialect-specific SQL operations."

  alias Cloak.Sql.Expression

  @doc "Returns the list of supported functions for this SQL dialect."
  @callback supported_functions() :: [String.t]

  @doc "Generates dialect-specific SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @callback function_sql(Expression.function_name, [iodata]) :: iodata

  @doc "Generates dialect-specific SQL for the LIKE operator."
  @callback like_sql(iodata, iodata) :: iodata

  @doc "Generates dialect-specific SQL for the ILIKE operator."
  @callback ilike_sql(iodata, iodata) :: iodata

  @doc "Generates dialect-specific SQL for the ILIKE operator."
  @callback limit_sql(pos_integer | nil, non_neg_integer) :: iodata

  @doc "Generates dialect-specific SQL for casting a column of an unknown type."
  @callback cast_unknown_sql(iodata) :: iodata

  @doc "Returns the dialect-specific SQL type for casting."
  @callback sql_type(atom) :: String.t

  @doc "Returns the dialect-specific SQL for a unicode string literal."
  @callback unicode_literal(iodata) :: iodata

  @doc "Returns the dialect-specific SQL for an interval literal."
  @callback interval_literal(iodata) :: iodata

  @doc "Returns the dialect-specific SQL for adding/subtracting to a date/time/datetime."
  @callback time_arithmetic_expression(String.t, iodata) :: iodata

  @doc "Returns the dialect-specific SQL for subtracting two date/time/datetimes."
  @callback date_subtraction_expression(iodata) :: iodata

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

      @doc false
      def cast_sql(value, type), do:
        ["CAST(", value, " AS ", sql_type(type), ")"]

      @doc false
      def time_arithmetic_expression(operator, [arg1, arg2]), do: ["(", arg1, " ", operator, " ", arg2, ")"]

      @doc false
      def date_subtraction_expression([arg1, arg2]), do: ["(", arg1, " - ", arg2, ")"]

      @doc false
      def interval_literal(duration), do: duration |> Timex.Duration.to_seconds() |> to_string()

      defoverridable like_sql: 2, ilike_sql: 2, limit_sql: 2, cast_unknown_sql: 1, cast_sql: 2, interval_literal: 1,
        time_arithmetic_expression: 2, date_subtraction_expression: 1
    end
  end
end
