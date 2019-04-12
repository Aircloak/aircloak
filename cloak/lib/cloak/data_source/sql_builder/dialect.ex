defmodule Cloak.DataSource.SqlBuilder.Dialect do
  @moduledoc "Specifies the interface for implementing dialect-specific SQL operations."

  alias Cloak.Sql.Expression

  @doc "Returns the list of supported functions for this SQL dialect."
  @callback supported_functions() :: [String.t()]

  @doc "Generates dialect specific SELECT statement hints"
  @callback select_hints() :: iodata

  @doc "Generates dialect-specific SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @callback function_sql(Expression.function_name(), [iodata]) :: iodata

  @doc "Generates dialect-specific SQL for the LIKE operator."
  @callback like_sql(iodata, iodata) :: iodata

  @doc "Defaults to true. If false, the sql builder will rewrite ILIKE to the LIKE equivalent"
  @callback native_support_for_ilike?() :: boolean

  @doc "Generates dialect-specific SQL for the ILIKE operator."
  @callback ilike_sql(iodata, Cloak.Sql.LikePattern.t()) :: iodata

  @doc "Generates dialect-specific SQL for the LIMIT clause."
  @callback limit_sql(pos_integer | nil, non_neg_integer) :: iodata

  @doc "Generates dialect-specific SQL for casting a column."
  @callback cast_sql(iodata, atom, atom) :: iodata

  @doc "Generates dialect-specific SQL for aliasing an object."
  @callback alias_sql(iodata, iodata) :: iodata

  @doc "Returns the dialect-specific SQL for a literal."
  @callback literal(Cloak.DataSource.field()) :: iodata

  @doc "Returns the dialect-specific SQL for adding/subtracting to a date/time/datetime."
  @callback time_arithmetic_expression(String.t(), iodata) :: iodata

  @doc "Returns the dialect-specific SQL for subtracting two date/time/datetimes."
  @callback date_subtraction_expression(iodata) :: iodata

  @doc "Returns the dialect-specific SQL for dividing an interval."
  @callback interval_division(iodata) :: iodata

  @doc "Returns the dialect-specific ORDER BY clause SQL for the given column, order and nulls directive."
  @callback order_by(iodata, :asc | :desc, :nulls_first | :nulls_last | :nulls_natural) :: iodata

  @doc "Returns if range clauses (limit/offset) should be applied at the start of the statement."
  @callback range_at_statement_start?() :: boolean

  @doc "Returns the charcter used for quoting names."
  @callback quote_char() :: integer

  @doc "Returns the select statement for retrieving the names of all tables which start with the given prefix."
  @callback select_table_names(prefix :: String.t()) :: String.t()

  @doc "Returns the statement for creating the analyst meta table."
  @callback analyst_meta_table_create_statement(String.t()) :: String.t()

  @doc "Returns the expression for representing a large string constant."
  @callback long_string(String.t()) :: String.t()

  alias Cloak.Query.ExecutionError

  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      @integer_range 9_223_372_036_854_775_807
      @is_integer_regex "^ *-?[0-9]{1,18} *$"
      @is_real_regex "^ *-?[0-9]{1,99}(\\.[0-9]*)?([eE]-?[0-9]{1,2})? *$"

      @impl unquote(__MODULE__)
      def select_hints(), do: ""

      @impl unquote(__MODULE__)
      def like_sql(what, match), do: [what, " LIKE ", match]

      @impl unquote(__MODULE__)
      def native_support_for_ilike?(), do: true

      @impl unquote(__MODULE__)
      def ilike_sql(what, pattern),
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
      def alias_sql(object, alias), do: [object, " AS ", alias]

      @impl unquote(__MODULE__)
      def time_arithmetic_expression(operator, [arg1, arg2]), do: ["(", arg1, " ", operator, " ", arg2, ")"]

      @impl unquote(__MODULE__)
      def date_subtraction_expression([arg1, arg2]), do: ["(", arg1, " - ", arg2, ")"]

      @impl unquote(__MODULE__)
      def interval_division(args), do: function_sql("/", args)

      @impl unquote(__MODULE__)
      def literal(value), do: literal_default(value)

      @impl unquote(__MODULE__)
      def order_by(column, :asc, :nulls_natural), do: [column, " ASC"]
      def order_by(column, :desc, :nulls_natural), do: [column, " DESC"]
      def order_by(column, :asc, :nulls_first), do: [column, " ASC NULLS FIRST"]
      def order_by(column, :desc, :nulls_first), do: [column, " DESC NULLS FIRST"]
      def order_by(column, :asc, :nulls_last), do: [column, " ASC NULLS LAST"]
      def order_by(column, :desc, :nulls_last), do: [column, " DESC NULLS LAST"]

      @impl unquote(__MODULE__)
      def range_at_statement_start?(), do: false

      @impl unquote(__MODULE__)
      def quote_char(), do: ?"

      @impl unquote(__MODULE__)
      def select_table_names(_prefix),
        do: raise(RuntimeError, "Analyst tables are not supported on this data source.")

      @impl unquote(__MODULE__)
      def analyst_meta_table_create_statement(_quoted_table_name),
        do: raise(RuntimeError, "Analyst tables are not supported on this data source.")

      @impl unquote(__MODULE__)
      def long_string(string), do: "'#{string}'"

      defoverridable unquote(__MODULE__)
    end
  end

  @spec literal_default(Cloak.DataSource.field()) :: iodata
  def literal_default(%NaiveDateTime{} = value), do: ["timestamp '", to_string(value), ?']

  def literal_default(%Time{} = value), do: ["time '", to_string(value), ?']
  def literal_default(%Date{} = value), do: ["date '", to_string(value), ?']

  def literal_default(%Timex.Duration{} = duration),
    do: duration |> Timex.Duration.to_seconds() |> to_string()

  def literal_default(value) when is_number(value), do: to_string(value)

  def literal_default(value) when is_boolean(value), do: to_string(value)

  def literal_default(value) when is_binary(value), do: [?', value, ?']

  def literal_default(nil), do: "NULL"
end
