defmodule Cloak.Sql.Query do
  @moduledoc """
  Represents a compiled SQL query.

  The struct defined by this module fully describes the goal of the query. It
  is used in various query execution phases, for example to fetch data from the
  database, perform anonymized aggregation, and produce the final output.
  """

  alias Cloak.{DataSource, Query.DataDecoder}
  alias Cloak.Sql.{Expression, Compiler, Function, Parser, Query.Lenses, NoiseLayer, Condition}
  require Logger

  @type comparison :: {:comparison, Expression.t, Parser.comparator, Expression.t}

  @type condition ::
      comparison
    | {:like | :ilike, Expression.t, Expression.t}
    | {:is, Expression.t, :null}
    | {:in, Expression.t, [Expression.t]}

  @type filter_clause ::
      nil
    | condition
    | {:not, condition}
    | {:and | :or, condition, condition}

  @type view_map :: %{view_name :: String.t => view_sql :: String.t}

  @type row_index :: non_neg_integer

  @type parameter :: %{value: DataSource.field, type: DataSource.Table.data_type}

  @type t :: %__MODULE__{
    data_source: DataSource.t,
    command: :select | :show,
    columns: [Expression.t],
    column_titles: [String.t],
    aggregators: [Function.t],
    # When row-splitters are used (like `extract_words`), the row splitting has to happen
    # prior to other functions being executed. All function call chains that contain one or
    # more row-splitters in them are partitioned such that the row-splitters and their child
    # function applications are contained in the row-splitters. The original function call
    # chains are then amended to take a virtual column as their input representing the output
    # of the row-splitters:
    #
    #   avg(length(extract_words(cast(number as text))))
    #
    # becomes:
    #
    #   avg(length(<dummy_column>))
    #   extract_words(cast(number as text))
    #
    # where the latter of these two is contained in the row-splitters.
    implicit_count?: boolean,
    group_by: [Function.t],
    where: filter_clause,
    order_by: [{Expression.t, :asc | :desc}],
    show: :tables | :columns | nil,
    selected_tables: [DataSource.Table.t],
    db_columns: [Expression.t],
    from: Parser.from_clause | String.t | nil,
    subquery?: boolean,
    limit: pos_integer | nil,
    offset: non_neg_integer,
    having: filter_clause,
    distinct?: boolean,
    sample_rate: nil | non_neg_integer,
    emulated?: boolean,
    parameters: [parameter] | nil,
    views: view_map,
    projected?: boolean,
    next_row_index: row_index,
    noise_layers: [NoiseLayer.t],
    view?: boolean,
    table_aliases: %{String.t => DataSource.Table.t},
    virtual_table?: boolean,
  }

  @type features :: %{
    num_selected_columns: pos_integer,
    num_db_columns: pos_integer,
    num_tables: pos_integer,
    num_group_by: non_neg_integer,
    functions: [String.t],
    expressions: [String.t],
    where_conditions: [String.t],
    column_types: [String.t],
    selected_types: [String.t],
    parameter_types: [String.t],
    decoders: [String.t],
    driver: String.t,
    driver_dialect: String.t,
    emulated: boolean,
  }

  defstruct [
    columns: [], where: nil, group_by: [], order_by: [], column_titles: [], aggregators: [],
    info: [], selected_tables: [], implicit_count?: false, data_source: nil, command: nil,
    show: nil, db_columns: [], from: nil, subquery?: false, limit: nil, offset: 0, having: nil, distinct?: false,
    parameters: [], views: %{}, emulated?: false, sample_rate: nil, projected?: false,
    next_row_index: 0, parameter_types: %{}, noise_layers: [], view?: false, table_aliases: %{}, virtual_table?: false,
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Describes the result of a parameterized query.

  This function will return the description of the result, such as column names
  and types, without executing the query.
  """
  @spec describe_query(DataSource.t, String.t, [parameter] | nil, view_map) ::
    {:ok, [String.t], features} | {:error, String.t}
  def describe_query(data_source, statement, parameters, views), do:
    with {:ok, query, features} <- make_query(data_source, statement, parameters, views), do:
      {:ok, query.column_titles, features}


  @doc "Validates a user-defined view."
  @spec validate_view(DataSource.t, String.t, String.t, view_map) ::
    {:ok, [%{name: String.t, type: String.t, user_id: boolean}]} |
    {:error, field :: atom, reason :: String.t}
  def validate_view(data_source, name, sql, views) do
    with :ok <- view_name_ok?(data_source, name),
         {:ok, parsed_query} <- Parser.parse(sql),
         {:ok, compiled_query, _features} <- Compiler.validate_view(data_source, parsed_query, views)
    do
      {:ok,
        Enum.zip(compiled_query.column_titles, compiled_query.columns)
        |> Enum.map(fn({name, column}) ->
              %{name: name, type: Atom.to_string(Function.type(column)),
                user_id: match?(%Expression{user_id?: true}, column)}
            end)
      }
    else
      {:error, _field, _error} = error -> error
      {:error, sql_error} -> {:error, :sql, sql_error}
    end
  end

  @doc "Adds one or more info messages to the query."
  @spec add_info(t, String.t | [String.t]) :: t
  def add_info(query, info_messages) when is_list(info_messages), do:
    Enum.reduce(info_messages, query, &add_info(&2, &1))
  def add_info(query, info_message), do:
    %__MODULE__{query | info: [info_message | query.info]}

  @doc "Returns all info messages in the given query."
  @spec info_messages(t) :: [String.t]
  def info_messages(query), do: Enum.reverse(query.info)

  @doc "Adds a database column to the query and updates all references to that column."
  @spec add_db_column(t, Expression.t) :: t
  def add_db_column(query, column) do
    # A db column we're adding has to have a well-defined id
    false = is_nil(Expression.id(column))
    column_matcher = &(Expression.id(&1) == Expression.id(column) and &1.alias == column.alias)

    case Enum.find(query.db_columns, column_matcher) do
      nil ->
        {next_row_index, query} = next_row_index(query)

        %__MODULE__{query | db_columns: query.db_columns ++ [column]}
        |> put_in([Lenses.query_expressions() |> Lens.filter(column_matcher) |> Lens.key(:row_index)], next_row_index)
      _ ->
        query
    end
  end

  @doc "Sets the parameter type."
  @spec set_parameter_type(t, pos_integer, DataSource.Table.data_type) :: t
  def set_parameter_type(query, parameter_index, type), do:
    %__MODULE__{query | parameter_types: Map.put(query.parameter_types, parameter_index, type)}

  @doc "Merges parameter types of other query into this query."
  @spec merge_parameter_types(t, t) :: t
  def merge_parameter_types(query, other_query), do:
    %__MODULE__{query | parameter_types: Map.merge(query.parameter_types, other_query.parameter_types)}

  @doc "Retrieves the parameter type."
  @spec parameter_type(t, pos_integer) :: DataSource.Table.data_type
  def parameter_type(query, parameter_index), do:
    Map.get(query.parameter_types, parameter_index, :unknown)

  @doc "Returns the ordered list of parameter types."
  @spec parameter_types(t) :: [DataSource.t]
  def parameter_types(query), do:
    # Using `:array` here ensures that we capture unresolved params. E.g. in a query
    #   `select cast($1 as integer), $3, cast($4 as boolean)`
    # this function will correctly return `[:integer, :unknown, :unknown, :boolean]`.
    query.parameter_types
    |> Enum.reduce(:array.new(default: :unknown), fn({index, type}, acc) -> :array.set(index - 1, type, acc) end)
    |> :array.to_list()

  @doc "When debug logging is enabled, logs the query and the specified message."
  @spec debug_log(t, String.t) :: t
  def debug_log(query, message) do
    Logger.debug(fn () ->
      try do
        statement = DataSource.SqlBuilder.build(%__MODULE__{query | subquery?: true},
          Cloak.DataSource.SqlBuilder.PostgreSQL)
        "#{message}: `#{statement}` ..."
      rescue
        error -> "#{message}\n#{inspect(query)}\nEncountered #{inspect(error)} in SqlBuilder"
      end
    end)
    query
  end

  @doc "Returns the list of order by expressions."
  def order_by_expressions(query), do:
    Enum.map(query.order_by, fn({column, _}) -> column end)

  @doc "Returns the ordered list of bucket columns."
  @spec bucket_columns(t) :: [Expression.t]
  def bucket_columns(query), do:
    query.columns ++ (query |> order_by_expressions() |> Enum.reject(& &1 in query.columns))

  @doc "Returns the table that the given name refers to in the given query. Useful for resolving aliases."
  @spec resolve_table(t, String.t) :: {:ok, DataSource.Table.t}
  def resolve_table(query, table_name) do
    case query.selected_tables |> Enum.find(& &1.name == table_name) do
      nil -> :error
      table -> {:ok, table}
    end
  end

  @doc "Updates the emulation flag to reflect whether the query needs to be emulated."
  @spec set_emulation_flag(t) :: t
  def set_emulation_flag(query), do:
    Compiler.Helpers.apply_bottom_up(query, &%__MODULE__{&1 | emulated?: needs_emulation?(&1)})

  @doc "Returns the list of outermost selected splitters."
  @spec outermost_selected_splitters(t) :: [Expression.t]
  def outermost_selected_splitters(query), do:
    Lens.to_list(Lenses.outermost_selected_splitters(), query)

  @doc "Returns the list of outermost where splitters."
  @spec outermost_where_splitters(t) :: [Expression.t]
  def outermost_where_splitters(query), do:
    Lens.to_list(Lenses.outermost_where_splitters(), query)

  @doc "Retrieves the query features."
  @spec features(Query.t) :: features
  defdelegate features(query), to: __MODULE__.Features

  @doc "Replaces all occurrences of one expression with another expression."
  @spec replace_expression(t, Expression.t, Expression.t) :: t
  def replace_expression(query, expression, new_expression), do:
    Lenses.query_expressions()
    |> Lens.satisfy(& Expression.semantic(&1) == Expression.semantic(expression))
    |> Lens.map(query, fn(_) -> new_expression end)

  @doc """
  Finds the subquery a given column comes from.

  Returns `:database_column` if the column does not come from any subquery. Otherwise returns `{column, subquery}`.
  """
  @spec resolve_subquery_column(Expression.t, t) :: :database_column | {Expression.t, t}
  def resolve_subquery_column(column, query) do
    Lens.to_list(Lenses.direct_subqueries(), query)
    |> Enum.find(&(&1.alias == column.table.name))
    |> case do
      nil -> :database_column
      %{ast: subquery} ->
        column_index = Enum.find_index(subquery.column_titles, &(&1 == column.name))
        column = Enum.at(subquery.columns, column_index)
        {column, subquery}
    end
  end

  @doc "Resolves the columns which must be fetched from the database."
  @spec resolve_db_columns(t) :: t
  def resolve_db_columns(%__MODULE__{command: :select} = query), do:
    query |> reset_db_columns() |> Compiler.Helpers.apply_bottom_up(&include_required_expressions/1)
  def resolve_db_columns(%__MODULE__{} = query), do: query

  @doc "Returns the where clauses that can be applied by the data source."
  @spec offloaded_where(t) :: filter_clause
  def offloaded_where(query), do:
    Condition.reject(query.where, &emulated_condition?(&1, query))

  @doc "Returns the where clauses that must be applied by inside the cloak."
  @spec emulated_where(t) :: filter_clause
  def emulated_where(query), do:
    Condition.reject(query.where, &not emulated_condition?(&1, query))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp make_query(data_source, query_string, parameters, views) do
    with {:ok, parsed_query} <- Parser.parse(query_string) do
      Compiler.compile(data_source, parsed_query, parameters, views)
    end
  end

  defp view_name_ok?(data_source, view_name) do
    if Enum.any?(DataSource.tables(data_source), &(&1.name == view_name)) do
      {:error, :name, "has already been taken"}
    else
      :ok
    end
  end

  defp next_row_index(query), do:
    {query.next_row_index, %__MODULE__{query | next_row_index: query.next_row_index + 1}}


  # -------------------------------------------------------------------
  # Calculation of db_columns
  # -------------------------------------------------------------------

  defp include_required_expressions(query), do:
    Enum.reduce(required_expressions(query), query, &add_db_column(&2, &1))

  defp required_expressions(%__MODULE__{command: :select, subquery?: true, emulated?: false} = query) do
    Enum.zip(query.column_titles, query.columns)
    |> Enum.map(fn({column_alias, column}) -> %Expression{column | alias: column_alias} end)
  end
  defp required_expressions(%__MODULE__{command: :select} = query) do
    # top-level query -> we're only fetching columns, while other expressions (e.g. function calls)
    # will be resolved in the post-processing phase
    used_columns =
      query
      |> needed_columns()
      |> extract_columns()
      |> Enum.reject(& &1.constant?)

    [Compiler.Helpers.id_column(query) | used_columns]
  end

  defp needed_columns(query), do:
    [
      query.columns,
      query.group_by,
      emulated_where(query),
      query.having,
      order_by_expressions(query),
      Compiler.NoiseLayers.noise_layer_columns(query)
    ]

  defp extract_columns(columns), do: Lenses.leaf_expressions() |> Lens.to_list(columns)

  defp reset_db_columns(query), do: %__MODULE__{query | next_row_index: 0, db_columns: []}

 # -------------------------------------------------------------------
 # Emulation
 # -------------------------------------------------------------------

 defp needs_emulation?(query), do:
   not query.data_source.driver.supports_query?(query) or
   query |> get_in([Lenses.direct_subqueries()]) |> Enum.any?(&(&1.ast.emulated?)) or
   (query.subquery? and has_emulated_expressions?(query)) or
   has_emulated_join_conditions?(query)

 defp emulated_condition?(condition, query) do
   emulated_expression_condition?(condition, query.data_source) or
   (
     query.emulated? and
     (
       multiple_tables_condition?(condition) or
       not is_binary(query.from)
     )
   )
 end

 defp emulated_expression?(expression, data_source), do:
   DataDecoder.needs_decoding?(expression) or
   (expression.function? and not data_source.driver.supports_function?(expression, data_source))

 defp emulated_expression_condition?(condition, data_source) do
   Lenses.conditions_terminals()
   |> Lens.to_list([condition])
   |> Enum.any?(&emulated_expression?(&1, data_source))
 end

 defp has_emulated_expressions?(query), do:
   Lenses.all_expressions()
   |> Lens.to_list([query.columns, query.group_by, query.having, query.where])
   |> Enum.any?(&emulated_expression?(&1, query.data_source))

 defp has_emulated_join_conditions?(query), do:
   query
   |> Compiler.Helpers.all_join_conditions()
   |> get_in([Lenses.all_expressions()])
   |> Enum.any?(&emulated_expression?(&1, query.data_source))

 defp multiple_tables_condition?(condition) do
   Lenses.conditions_terminals()
   |> Lens.to_list([condition])
   |> Enum.map(& &1.table)
   |> Enum.uniq()
   |> Enum.count() > 1
 end
end
