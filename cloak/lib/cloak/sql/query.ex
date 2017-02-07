defmodule Cloak.Sql.Query do
  @moduledoc """
  Represents a compiled SQL query.

  The struct defined by this module fully describes the goal of the query. It
  is used in various query execution phases, for example to fetch data from the
  database, perform anonymized aggregation, and produce the final output.
  """

  alias Cloak.DataSource
  alias Cloak.Sql.{Expression, Compiler, Function, Parser, Query.Lenses, Range}

  @type negatable_condition ::
      {:comparison, Expression.t, :=, Expression.t}
    | {:like | :ilike, Expression.t, Expression.t}
    | {:is, Expression.t, :null}
    | {:in, Expression.t, [Expression.t]}

  @type where_clause ::
      negatable_condition
    | {:not, negatable_condition}
    | {:comparison, Expression.t, Parser.comparator, Expression.t}

  @type having_clause :: {:comparison, Expression.t, Parser.comparator, Expression.t}

  @type view_map :: %{view_name :: String.t => view_sql :: String.t}

  @type row_index :: non_neg_integer

  @type parameter :: %{value: DataSource.field, type: DataSource.data_type}

  @type t :: %__MODULE__{
    data_source: DataSource.t,
    command: :select | :show,
    columns: [Expression.t] | :*,
    column_titles: [String.t],
    property: [Function.t],
    aggregators: [Function.t],
    # When row-splitters are used (like `extract_matches`), the row splitting has to happen
    # prior to other functions being executed. All function call chains that contain one or
    # more row-splitters in them are partitioned such that the row-splitters and their child
    # function applications are contained in the row-splitters. The original function call
    # chains are then amended to take a virtual column as their input representing the output
    # of the row-splitters:
    #
    #   avg(length(extract_matches(cast(number as text), '\d+')))
    #
    # becomes:
    #
    #   avg(length(<dummy_column>)
    #   extract_matches(cast(number as text), '\d+')
    #
    # where the latter of these two is contained in the row-splitters.
    row_splitters: [%{function_spec: Parser.function_spec, row_index: row_index}],
    implicit_count?: boolean,
    group_by: [Function.t],
    where: [where_clause],
    emulated_where: [where_clause],
    order_by: [{pos_integer, :asc | :desc}],
    show: :tables | :columns,
    selected_tables: [DataSource.table],
    db_columns: [Expression.t],
    from: Parser.from_clause | nil,
    subquery?: boolean,
    limit: pos_integer | nil,
    offset: non_neg_integer,
    having: [having_clause],
    distinct?: boolean,
    emulated?: boolean,
    ranges: [Range.t],
    parameters: [parameter] | nil,
    views: view_map,
    projected?: boolean,
    next_row_index: row_index
  }

  defstruct [
    columns: [], where: [], group_by: [], order_by: [], column_titles: [], property: [], aggregators: [],
    info: [], selected_tables: [], row_splitters: [], implicit_count?: false, data_source: nil, command: nil,
    show: nil, db_columns: [], from: nil, subquery?: false, limit: nil, offset: 0, having: [], distinct?: false,
    features: nil, emulated_where: [], ranges: %{}, parameters: [], views: %{}, emulated?: false,
    projected?: false, next_row_index: 0, parameter_types: %{}
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a compiled query from a string representation.

  Raises on error.
  """
  @spec make!(DataSource.t, String.t, [parameter], view_map) :: t
  def make!(data_source, string, parameters, views) do
    {:ok, query} = make(data_source, string, parameters, views)
    query
  end

  @doc "Creates a compiled query from a string representation."
  @spec make(DataSource.t, String.t, [parameter], view_map) ::
    {:ok, t} | {:error, String.t}
  def make(data_source, string, parameters, views) when is_list(parameters), do:
    make_query(data_source, string, parameters, views)

  @doc "Returns the list of unique columns used in the aggregation process."
  @spec aggregated_columns(t) :: [Expression.t]
  def aggregated_columns(query) do
    query.aggregators
    |> Enum.flat_map(&(&1.function_args))
    |> Enum.uniq()
  end

  @doc """
  Returns a list of features used by a query, that can be used for
  analytics purposes by Aircloak.
  Examples include how many columns were selected, which, if any
  functions where used, etc.
  """
  @spec extract_features(t) :: map
  def extract_features(query) do
    %{
      num_selected_columns: num_selected_columns(query.column_titles),
      num_db_columns: num_db_columns(query.columns),
      num_tables: num_tables(query.selected_tables),
      num_group_by: num_group_by(query),
      functions: extract_functions(query.columns),
      where_conditions: extract_where_conditions(query.where ++ query.emulated_where),
      column_types: extract_column_types(query.columns),
      selected_types: selected_types(query.columns),
      parameter_types: Enum.map(parameter_types(query), &stringify/1),
      decoders: extract_decoders(query),
    }
  end

  @doc """
  Describes the result of a parameterized query.

  This function will return the description of the result, such as column names
  and types, without executing the query.
  """
  @spec describe_query(DataSource.t, String.t, [parameter] | nil, view_map) ::
    {:ok, [String.t], map} | {:error, String.t}
  def describe_query(data_source, statement, parameters, views), do:
    with {:ok, query} <- make_query(data_source, statement, parameters, views), do:
      {:ok, query.column_titles, extract_features(query)}


  @doc "Validates a user-defined view."
  @spec validate_view(DataSource.t, String.t, String.t, view_map) ::
    {:ok, [%{name: String.t, type: String.t, user_id: boolean}]} |
    {:error, field :: atom, reason :: String.t}
  def validate_view(data_source, name, sql, views) do
    with :ok <- view_name_ok?(data_source, name),
         {:ok, parsed_query} <- Parser.parse(sql),
         {:ok, compiled_query} <- Compiler.validate_view(data_source, parsed_query, views)
    do
      {:ok,
        Enum.zip(compiled_query.column_titles, compiled_query.columns)
        |> Enum.map(fn({name, column}) ->
              %{name: name, type: stringify(Function.type(column)),
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
    case Enum.find(query.db_columns, &Expression.id(&1) == Expression.id(column) and &1.alias == column.alias) do
      nil ->
        {next_row_index, query} = next_row_index(query)
        Lens.map(
          Lenses.query_expressions() |> Lenses.expressions_like(column) |> Lens.key(:row_index),
          %__MODULE__{query | db_columns: query.db_columns ++ [column]},
          fn(_) -> next_row_index end
        )
      _ ->
        query
    end
  end

  @doc "Returns the next row index and the transformed query with incremented row index."
  @spec next_row_index(t) :: {row_index, t}
  def next_row_index(query), do:
    {query.next_row_index, %__MODULE__{query | next_row_index: query.next_row_index + 1}}

  @doc "Sets the parameter type."
  @spec set_parameter_type(t, pos_integer, DataSource.data_type) :: t
  def set_parameter_type(query, parameter_index, type), do:
    %__MODULE__{query | parameter_types: Map.put(query.parameter_types, parameter_index, type)}

  @doc "Merges parameter types of other query into this query."
  @spec merge_parameter_types(t, t) :: t
  def merge_parameter_types(query, other_query), do:
    %__MODULE__{query | parameter_types: Map.merge(query.parameter_types, other_query.parameter_types)}

  @doc "Retrieves the parameter type."
  @spec parameter_type(t, pos_integer) :: DataSource.data_type
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

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp make_query(data_source, query_string, parameters, views) do
    with {:ok, parsed_query} <- Parser.parse(query_string) do
      Compiler.compile(data_source, parsed_query, parameters, views)
    end
  end

  defp selected_types(columns), do:
    columns
    |> Enum.map(&Function.type/1)
    |> Enum.map(&stringify/1)

  defp num_selected_columns(columns), do: length(columns)

  defp num_db_columns(columns), do:
    columns
    |> extract_columns()
    |> Enum.uniq()
    |> Enum.reject(&(&1.constant?))
    |> Enum.count()

  defp num_tables(tables), do: length(tables)

  defp num_group_by(%{group_by: clauses}), do: length(clauses)

  defp extract_functions([:*]), do: []
  defp extract_functions(columns), do:
    columns
    |> Enum.flat_map(&extract_function/1)
    |> Enum.uniq()

  defp extract_function(%Expression{function?: true, function: function, function_args: args}), do:
    [Function.readable_name(function) | extract_functions(args)]
  defp extract_function(%Expression{}), do: []
  defp extract_function({:distinct, param}), do: extract_function(param)

  defp extract_where_conditions(clauses), do:
    clauses
    |> Enum.map(&extract_where_condition/1)
    |> Enum.uniq()

  defp extract_where_condition({:not, {:comparison, _column, :=, _comparator}}), do: "<>"
  defp extract_where_condition({:not, something}), do:
    "not #{extract_where_condition(something)}"
  defp extract_where_condition({:comparison, _column, comparison, _comparator}), do:
    Atom.to_string(comparison)
  defp extract_where_condition({:is, _column, :null}), do: "null"
  defp extract_where_condition({condition, _column, _value_or_pattern}), do:
    Atom.to_string(condition)

  defp extract_column_types(columns), do:
    columns
    |> extract_columns()
    |> Enum.flat_map(&extract_column_type/1)
    |> Enum.uniq()
    |> Enum.map(&stringify/1)

  defp extract_column_type(%Expression{constant?: true, type: type}), do: [type]
  defp extract_column_type(%Expression{table: :unknown}), do: []
  defp extract_column_type(%Expression{table: %{columns: columns}, name: name}), do:
    columns
    |> Enum.filter(& elem(&1, 0) == name)
    |> Enum.map(& elem(&1, 1))

  defp extract_columns(columns), do: Enum.flat_map(columns, &extract_column/1)

  defp extract_column({:distinct, value}), do: extract_column(value)
  defp extract_column(%Expression{function?: true, function_args: [:*]}), do: []
  defp extract_column(%Expression{function?: true, function_args: args}), do: extract_columns(args)
  defp extract_column(%Expression{} = column), do: [column]

  defp extract_decoders(query) do
    Lenses.query_expressions()
    |> Lens.satisfy(&match?(%Expression{function?: false, constant?: false, table: %{decoders: [_|_]}}, &1))
    |> Lens.satisfy(&decoded?/1)
    |> Lens.get(query)
    |> Enum.map(&decoder/1)
    |> Enum.uniq()
  end

  defp decoder(%{name: name, table: %{decoders: decoders}}) do
    case Enum.find(decoders, &(name in &1.columns)) do
      nil -> nil
      decoder -> stringify(decoder.spec)
    end
  end

  defp decoded?(column), do: decoder(column) != nil

  defp stringify(string) when is_binary(string), do: string
  defp stringify(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp stringify(function) when is_function(function), do: inspect(function)

  defp view_name_ok?(data_source, view_name) do
    if Enum.any?(DataSource.tables(data_source), &(&1.name == view_name)) do
      {:error, :name, "has already been taken"}
    else
      :ok
    end
  end
end
