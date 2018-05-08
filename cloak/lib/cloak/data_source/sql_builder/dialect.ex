defmodule Cloak.DataSource.SqlBuilder.Dialect do
  @moduledoc "Specifies the interface for implementing dialect-specific SQL operations."

  alias Cloak.Sql.Expression

  @doc "Returns the list of supported functions for this SQL dialect."
  @callback supported_functions() :: [String.t()]

  @doc "Generates dialect-specific SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @callback function_sql(Expression.function_name(), [iodata]) :: iodata

  @doc "Generates dialect-specific SQL for the LIKE operator."
  @callback like_sql(iodata, iodata) :: iodata

  @doc "Defaults to true. If false, the sql builder will rewrite ILIKE to the LIKE equivalent"
  @callback native_support_for_ilike?() :: boolean

  @doc "Generates dialect-specific SQL for the ILIKE operator."
  @callback ilike_sql(iodata, iodata) :: iodata

  @doc "Generates dialect-specific SQL for the ILIKE operator."
  @callback limit_sql(pos_integer | nil, non_neg_integer) :: iodata

  @doc "Generates dialect-specific SQL for casting a column."
  @callback cast_sql(iodata, atom, atom) :: iodata

  @doc "Returns the dialect-specific SQL for a unicode string literal."
  @callback unicode_literal(iodata) :: iodata

  @doc "Returns the dialect-specific SQL for a boolean literal."
  @callback boolean_literal(boolean) :: iodata

  @doc "Returns the dialect-specific SQL for an interval literal."
  @callback interval_literal(Timex.Duration.t()) :: iodata

  @doc "Returns the dialect-specific SQL for adding/subtracting to a date/time/datetime."
  @callback time_arithmetic_expression(String.t(), iodata) :: iodata

  @doc "Returns the dialect-specific SQL for subtracting two date/time/datetimes."
  @callback date_subtraction_expression(iodata) :: iodata

  @doc "Returns the dialect-specific ORDER BY clause SQL for the given column, order and nulls directive."
  @callback order_by(iodata, :asc | :desc, :nulls_first | :nulls_last | :nulls_natural) :: iodata

  alias Cloak.Query.ExecutionError

  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      @impl unquote(__MODULE__)
      def like_sql(what, match), do: [what, " LIKE ", match]

      @impl unquote(__MODULE__)
      def native_support_for_ilike?(), do: true

      @impl unquote(__MODULE__)
      def ilike_sql(what, match),
        # ILIKE requires the support for collation. Each data source that returns true for
        # `native_support_for_ilike?/1` must explicitly handle this
        do:
          raise(
            ExecutionError,
            message: "This data source is missing an Aircloak ILIKE implementation"
          )

      @impl unquote(__MODULE__)
      def limit_sql(nil, _offset),
        # ilike requires the support for collation, so each data source must explicitly handle this
        do: raise(ExecutionError, message: "OFFSET operator is not supported on this data source")

      def limit_sql(_limit, _offset),
        # ilike requires the support for collation, so each data source must explicitly handle this
        do: raise(ExecutionError, message: "LIMIT operator is not supported on this data source")

      @impl unquote(__MODULE__)
      def time_arithmetic_expression(operator, [arg1, arg2]), do: ["(", arg1, " ", operator, " ", arg2, ")"]

      @impl unquote(__MODULE__)
      def date_subtraction_expression([arg1, arg2]), do: ["(", arg1, " - ", arg2, ")"]

      @impl unquote(__MODULE__)
      def interval_literal(duration), do: duration |> Timex.Duration.to_seconds() |> to_string()

      @impl unquote(__MODULE__)
      def boolean_literal(value), do: to_string(value)

      @impl unquote(__MODULE__)
      def order_by(column, :asc, :nulls_natural), do: [column, " ASC"]
      def order_by(column, :desc, :nulls_natural), do: [column, " DESC"]
      def order_by(column, :asc, :nulls_first), do: [column, " ASC NULLS FIRST"]
      def order_by(column, :desc, :nulls_first), do: [column, " DESC NULLS FIRST"]
      def order_by(column, :asc, :nulls_last), do: [column, " ASC NULLS LAST"]
      def order_by(column, :desc, :nulls_last), do: [column, " DESC NULLS LAST"]

      defoverridable like_sql: 2,
                     ilike_sql: 2,
                     limit_sql: 2,
                     interval_literal: 1,
                     boolean_literal: 1,
                     time_arithmetic_expression: 2,
                     date_subtraction_expression: 1,
                     native_support_for_ilike?: 0,
                     order_by: 3
    end
  end
end
