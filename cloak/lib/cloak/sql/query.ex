defmodule Cloak.Sql.Query do
  @moduledoc """
  Represents a compiled SQL query.

  The struct defined by this module fully describes the goal of the query. It
  is used in various query execution phases, for example to fetch data from the
  database, perform anonymized aggregation, and produce the final output.
  """

  alias Cloak.DataSource
  alias Cloak.Sql.{Expression, Compiler, Parser, Query.Lenses, NoiseLayer, Condition}
  require Logger

  @type filter_clause :: nil | Expression.t()

  @type user_views :: %{
          String.t() => %{
            :sql => String.t(),
            optional(:comment) => String.t() | nil
          }
        }

  @type row_index :: non_neg_integer

  @type parameter :: %{value: DataSource.field(), type: DataSource.Table.data_type()}

  @type type :: :standard | :restricted | :anonymized

  @type anonymization_type :: :statistics | :user_id

  @type from_clause :: table | subquery | join

  @type table :: String.t()

  @type join ::
          {:join,
           %{
             type: :cross_join | :inner_join | :full_outer_join | :left_outer_join | :right_outer_join,
             lhs: from_clause,
             rhs: from_clause,
             condition: filter_clause
           }}

  @type subquery :: {:subquery, %{ast: t, alias: String.t()}}

  @type union :: {:union, subquery, subquery}

  @type t :: %__MODULE__{
          analyst_id: analyst_id,
          data_source: DataSource.t(),
          command: :select | :show | :explain | :union,
          columns: [Expression.t()],
          column_titles: [String.t()],
          aggregators: [Expression.t()],
          implicit_count?: boolean,
          group_by: [Expression.t()],
          grouping_sets: [[non_neg_integer()]],
          where: filter_clause,
          order_by: [{Expression.t(), :asc | :desc, :nulls_first | :nulls_last | :nulls_natural}],
          show: :tables | :columns | nil,
          selected_tables: [DataSource.Table.t()],
          db_columns: [Expression.t()],
          from: from_clause | union | nil,
          subquery?: boolean,
          limit: pos_integer | nil,
          offset: non_neg_integer,
          having: filter_clause,
          distinct?: boolean,
          emulated?: boolean,
          parameters: [parameter] | nil,
          views: user_views,
          next_row_index: row_index,
          noise_layers: [NoiseLayer.t()],
          view?: boolean,
          table_aliases: %{String.t() => DataSource.Table.t()},
          type: type,
          anonymization_type: anonymization_type,
          available_tables: [DataSource.Table.t()],
          analyst_tables: %{String.t() => Cloak.AnalystTable.t()},
          analyst_table: nil | Cloak.AnalystTable.t(),
          required_analyst_tables: MapSet.t(),
          source_range: nil | {Parser.location(), Parser.location()}
        }

  @type analyst_id :: pos_integer | nil

  @type metadata :: %{
          selected_types: [String.t()],
          parameter_types: [String.t()]
        }

  @type described_columns :: [%{name: String.t(), type: String.t(), key_type: String.t(), comment: String.t() | nil}]

  defstruct columns: [],
            where: nil,
            group_by: [],
            grouping_sets: [],
            order_by: [],
            column_titles: [],
            aggregators: [],
            info: [],
            selected_tables: [],
            implicit_count?: false,
            analyst_id: nil,
            data_source: nil,
            command: nil,
            show: nil,
            db_columns: [],
            from: nil,
            subquery?: false,
            limit: nil,
            offset: 0,
            having: nil,
            distinct?: false,
            parameters: [],
            views: %{},
            emulated?: false,
            next_row_index: 0,
            parameter_types: %{},
            noise_layers: [],
            view?: false,
            table_aliases: %{},
            type: :restricted,
            anonymization_type: :user_id,
            available_tables: [],
            analyst_tables: %{},
            analyst_table: nil,
            required_analyst_tables: MapSet.new(),
            source_range: nil

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Describes the result of a parameterized query.

  This function will return the description of the result, such as column names
  and types, without executing the query.
  """
  @spec describe_query(analyst_id, DataSource.t(), String.t(), [parameter] | nil, user_views) ::
          {:ok, [String.t()], metadata} | {:error, String.t()}
  def describe_query(analyst_id, data_source, statement, parameters, views),
    do:
      with(
        {:ok, query} <- make_query(analyst_id, data_source, statement, parameters, views),
        do: {:ok, query.column_titles, metadata(query)}
      )

  @doc "Validates a user-defined view."
  @spec validate_view(analyst_id(), DataSource.t(), String.t(), String.t(), user_views) ::
          {:ok, described_columns}
          | {:error, field :: atom, reason :: String.t()}
  def validate_view(analyst_id, data_source, name, sql, views) do
    with :ok <- view_name_ok?(data_source, name),
         {:ok, parsed_query} <- Parser.parse(sql),
         {:ok, compiled_query} <- Compiler.validate_view(analyst_id, data_source, parsed_query, views) do
      {:ok, describe_selected(compiled_query)}
    else
      {:error, _field, _error} = error -> error
      {:error, sql_error} -> {:error, :sql, sql_error}
    end
  end

  @doc "Describes the selected coumns of the given query."
  @spec describe_selected(t) :: described_columns
  def describe_selected(query) do
    table = to_table(query, "some_table")

    Enum.map(
      table.columns,
      fn column ->
        %{
          name: column.name,
          type: to_string(column.type),
          access: to_string(column.access),
          key_type: with(key_type when not is_nil(key_type) <- table.keys[column.name], do: to_string(key_type)),
          comment: Cloak.DataSource.Table.column_comment(table, column.name)
        }
      end
    )
  end

  @doc "Creates a table definition which corresponds to the given select query."
  @spec to_table(t, String.t(), [DataSource.Table.option()]) :: DataSource.Table.t()
  def to_table(query, name, opts \\ []) do
    query
    |> selected_columns()
    |> Compiler.Helpers.create_table_from_columns(name, opts)
  end

  @doc "Returns the list of properly-aliased columns from the `SELECT` clause."
  @spec selected_columns(t) :: [Expression.t()]
  def selected_columns(%__MODULE__{command: command} = query) when command in [:select, :union] do
    Enum.zip(query.column_titles, query.columns)
    |> Enum.map(fn {title, column} -> %Expression{column | alias: title} end)
  end

  @doc "Adds one or more info messages to the query."
  @spec add_info(t, String.t() | [String.t()]) :: t
  def add_info(query, info_messages) when is_list(info_messages),
    do: Enum.reduce(info_messages, query, &add_info(&2, &1))

  def add_info(query, info_message), do: %__MODULE__{query | info: [info_message | query.info]}

  @doc "Adds one or more debug info messages to the query."
  @spec add_debug_info(t, String.t() | [String.t()]) :: t
  def add_debug_info(query, info_messages) when is_list(info_messages),
    do: Enum.reduce(info_messages, query, &add_debug_info(&2, &1))

  def add_debug_info(query, info_message) do
    if Logger.level() == :debug,
      do: add_info(query, "[Debug] " <> info_message),
      else: query
  end

  @doc "Returns all info messages in the given query."
  @spec info_messages(t) :: [String.t()]
  def info_messages(query), do: Enum.reverse(query.info)

  @doc "Adds a database column to the query and updates all references to that column."
  @spec add_db_column(t, Expression.t()) :: t
  def add_db_column(query, column) do
    # A db column we're adding has to have a well-defined id
    false = is_nil(Expression.id(column))
    column_matcher = &(Expression.equals?(&1, column) and &1.alias == column.alias)

    case Enum.find(query.db_columns, column_matcher) do
      nil ->
        {next_row_index, query} = next_row_index(query)

        %__MODULE__{query | db_columns: query.db_columns ++ [column]}
        |> put_in(
          [Lenses.query_expressions() |> Lens.filter(column_matcher) |> Lens.key(:row_index)],
          next_row_index
        )

      _ ->
        query
    end
  end

  @doc "Sets the parameter type."
  @spec set_parameter_type(t, pos_integer, DataSource.Table.data_type()) :: t
  def set_parameter_type(query, parameter_index, type),
    do: %__MODULE__{
      query
      | parameter_types: Map.put(query.parameter_types, parameter_index, type)
    }

  @doc "Merges parameter types of other query into this query."
  @spec merge_parameter_types(t, t) :: t
  def merge_parameter_types(query, other_query),
    do: %__MODULE__{
      query
      | parameter_types: Map.merge(query.parameter_types, other_query.parameter_types)
    }

  @doc "Retrieves the parameter type."
  @spec parameter_type(t, pos_integer) :: DataSource.Table.data_type()
  def parameter_type(query, parameter_index), do: Map.get(query.parameter_types, parameter_index, :unknown)

  @doc "Returns the ordered list of parameter types."
  @spec parameter_types(t) :: [DataSource.t()]
  def parameter_types(query),
    # Using `:array` here ensures that we capture unresolved params. E.g. in a query
    #   `select cast($1 as integer), $3, cast($4 as boolean)`
    # this function will correctly return `[:integer, :unknown, :unknown, :boolean]`.
    do:
      query.parameter_types
      |> Enum.reduce(:array.new(default: :unknown), fn {index, type}, acc ->
        :array.set(index - 1, type, acc)
      end)
      |> :array.to_list()

  @doc "When debug logging is enabled, logs the query and the specified message."
  @spec debug_log(t, String.t()) :: t
  def debug_log(query, message) do
    Logger.debug(fn ->
      try do
        statement =
          Lenses.all_queries()
          |> Lens.map(
            query,
            &%__MODULE__{&1 | subquery?: true, data_source: %{&1.data_source | driver: Cloak.DataSource.PostgreSQL}}
          )
          |> DataSource.SqlBuilder.build()

        "#{message}: `#{statement}` ..."
      rescue
        error -> "#{message}\n#{inspect(query)}\nEncountered #{inspect(error)} in SqlBuilder"
      end
    end)

    query
  end

  @doc "Returns the list of order by expressions."
  def order_by_expressions(query), do: Enum.map(query.order_by, fn {column, _, _} -> column end)

  @doc "Returns the ordered list of bucket columns."
  @spec bucket_columns(t) :: [Expression.t()]
  def bucket_columns(query),
    do: query.columns ++ (query |> order_by_expressions() |> Enum.reject(&(&1 in query.columns)))

  @doc "Returns the table that the given name refers to in the given query. Useful for resolving aliases."
  @spec resolve_table(t, String.t()) :: DataSource.Table.t()
  def resolve_table(query, table_name) do
    case query.table_aliases[table_name] do
      nil -> Enum.find(query.selected_tables, &(&1.name == table_name))
      table -> table
    end
  end

  @doc "Updates the emulation flag to reflect whether the query needs to be emulated."
  @spec set_emulation_flag(t) :: t
  def set_emulation_flag(query),
    do: Compiler.Helpers.apply_bottom_up(query, &%__MODULE__{&1 | emulated?: needs_emulation?(&1)})

  @doc "Retrieves the query metadata."
  @spec metadata(Query.t()) :: metadata
  defdelegate metadata(query), to: __MODULE__.Metadata

  @doc "Replaces all occurrences of one expression with another expression."
  @spec replace_expression(t, Expression.t(), Expression.t()) :: t
  def replace_expression(query, expression, new_expression),
    do:
      Lenses.query_expressions()
      |> Lens.filter(&Expression.equals?(&1, expression))
      |> Lens.map(query, fn _ -> new_expression end)

  @doc """
  Finds the subquery a given column comes from.

  Returns `:database_column` if the column does not come from any subquery. Otherwise returns `{column, subquery}`.
  """
  @spec resolve_subquery_column(Expression.t(), t) :: :database_column | {Expression.t(), t}
  def resolve_subquery_column(column, query) do
    case source_subquery(column, query) do
      nil ->
        :database_column

      %{ast: subquery} ->
        column_index = Enum.find_index(subquery.column_titles, &(&1 == column.name))
        false = is_nil(column_index)
        column = Enum.at(subquery.columns, column_index)
        {column, subquery}
    end
  end

  @doc "Resolves the columns which must be fetched from the database."
  @spec resolve_db_columns(t) :: t
  def resolve_db_columns(%__MODULE__{command: command} = query) when command in [:select, :union],
    do:
      query
      |> reset_db_columns()
      |> Compiler.Helpers.apply_bottom_up(&include_required_expressions/1)

  def resolve_db_columns(%__MODULE__{} = query), do: query

  @doc "Returns the where clauses that can be applied by the data source."
  @spec offloaded_where(t) :: filter_clause
  def offloaded_where(query), do: Condition.reject(query.where, &emulated_condition?(&1, query))

  @doc "Returns the where clauses that must be applied by inside the cloak."
  @spec emulated_where(t) :: filter_clause
  def emulated_where(query), do: Condition.reject(query.where, &(not emulated_condition?(&1, query)))

  @doc "Returns the maximum allowed number of rare conditions for the query"
  @spec max_rare_negative_conditions(t) :: non_neg_integer
  def max_rare_negative_conditions(%__MODULE__{data_source: data_source}) do
    default_limit = Application.get_env(:cloak, :shadow_tables) |> Keyword.fetch!(:max_rare_negative_conditions)
    data_source[:max_rare_negative_conditions] || default_limit
  end

  @doc "Returns the partial-aggregation limit for buckets."
  @spec lcf_buckets_aggregation_limit(t()) :: non_neg_integer()
  def lcf_buckets_aggregation_limit(%__MODULE__{data_source: data_source}),
    do: data_source[:lcf_buckets_aggregation_limit] || Application.get_env(:cloak, :lcf_buckets_aggregation_limit, 3)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp source_subquery(column, query) do
    Lens.to_list(Lenses.direct_subqueries(), query)
    |> Enum.find(&(&1.alias == column.table.name))
  end

  defp make_query(analyst_id, data_source, query_string, parameters, views) do
    with {:ok, parsed_query} <- Parser.parse(query_string) do
      Compiler.compile(parsed_query, analyst_id, data_source, parameters, views)
    end
  end

  defp view_name_ok?(data_source, view_name) do
    if Enum.any?(DataSource.tables(data_source), &(&1.name == view_name)) do
      {:error, :name, "has already been taken"}
    else
      :ok
    end
  end

  defp next_row_index(query), do: {query.next_row_index, %__MODULE__{query | next_row_index: query.next_row_index + 1}}

  # -------------------------------------------------------------------
  # Calculation of db_columns
  # -------------------------------------------------------------------

  defp include_required_expressions(query), do: Enum.reduce(required_expressions(query), query, &add_db_column(&2, &1))

  defp required_expressions(%__MODULE__{emulated?: false, type: type} = query)
       when type != :anonymized,
       # non-emulated, non-anonymized subquery -> the selected columns are all selected expressions
       do: selected_columns(query)

  defp required_expressions(%__MODULE__{} = query) do
    # anonymized query or emulated subquery -> we're only fetching columns,
    # while other expressions (e.g. function calls) will be resolved in the post-processing phase
    used_columns =
      query
      |> needed_columns()
      |> extract_columns()
      |> Enum.reject(&Expression.constant?/1)

    List.wrap(Compiler.Helpers.id_column(query)) ++ used_columns
  end

  defp needed_columns(query),
    do: [
      query.columns,
      query.group_by,
      emulated_where(query),
      query.having,
      order_by_expressions(query),
      query.aggregators,
      Compiler.NoiseLayers.noise_layer_columns(query)
    ]

  defp extract_columns(columns), do: Lenses.leaf_expressions() |> Lens.to_list(columns)

  defp reset_db_columns(query), do: %__MODULE__{query | next_row_index: 0, db_columns: []}

  # -------------------------------------------------------------------
  # Emulation
  # -------------------------------------------------------------------

  defp needs_emulation?(query) do
    cond do
      not query.data_source.driver.supports_query?(query) -> true
      has_emulated_or_anonymized_subqueries?(query) -> true
      is_emulated_query?(query) -> true
      has_emulated_join_conditions?(query) -> true
      true -> false
    end
  end

  defp has_emulated_or_anonymized_subqueries?(query),
    do: query |> get_in([Lenses.direct_subqueries()]) |> Enum.any?(&(&1.ast.emulated? or &1.ast.type == :anonymized))

  defp is_emulated_query?(query), do: query.type != :anonymized and has_emulated_expressions?(query)

  defp emulated_condition?(condition, query) do
    emulated_expression?(condition, query.data_source) or
      (query.emulated? and (multiple_tables_condition?(condition) or not is_binary(query.from)))
  end

  defp emulated_expression?(expression, data_source) do
    Lenses.all_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.reject(&data_source.driver.supports_function?(&1, data_source))
    |> Lens.to_list(expression)
    |> Enum.count() > 0
  end

  defp has_emulated_expressions?(query),
    do:
      [
        query.columns,
        query.group_by,
        query.having,
        query.where,
        order_by_expressions(query)
      ]
      |> List.flatten()
      |> Enum.any?(&emulated_expression?(&1, query.data_source))

  defp has_emulated_join_conditions?(query),
    do:
      query
      |> Compiler.Helpers.all_join_conditions()
      |> Enum.any?(&emulated_expression?(&1, query.data_source))

  defp multiple_tables_condition?(condition) do
    Lenses.conditions_terminals()
    |> Lens.to_list([condition])
    |> Enum.map(& &1.table)
    |> Enum.uniq()
    |> Enum.reject(&(&1 == :unknown))
    |> Enum.count() > 1
  end
end
