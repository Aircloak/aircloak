defmodule Cloak.Aql.Query do
  @moduledoc """
  Represents a compiled AQL query.

  The struct defined by this module fully describes the goal of the query. It
  is used in various query execution phases, for example to fetch data from the
  database, perform anonymized aggregation, and produce the final output.
  """

  alias Cloak.DataSource
  alias Cloak.Aql.{Column, Compiler, Function, Parser}

  @type negatable_condition ::
      {:comparison, Column.t, :=, Column.t}
    | {:like | :ilike, Column.t, Column.t}
    | {:is, Column.t, :null}
    | {:in, Column.t, [Column.t]}

  @type where_clause ::
      negatable_condition
    | {:not, negatable_condition}
    | {:comparison, Column.t, Parser.comparator, Column.t}

  @type having_clause :: {:comparison, Column.t, Parser.comparator, Column.t}

  @type view_map :: %{view_name :: String.t => view_sql :: String.t}

  @type t :: %__MODULE__{
    data_source: DataSource.t,
    features: Cloak.Features.t,
    command: :select | :show,
    columns: [Column.t],
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
    row_splitters: [Function.t],
    implicit_count?: boolean,
    unsafe_filter_columns: [Column.t],
    group_by: [Function.t],
    where: [where_clause],
    lcf_check_conditions: [where_clause],
    encoded_where: [where_clause],
    order_by: [{pos_integer, :asc | :desc}],
    show: :tables | :columns,
    selected_tables: [DataSource.table],
    mode: :parsed | :unparsed,
    db_columns: [Column.t],
    from: Parser.from_clause | nil,
    subquery?: boolean,
    limit: pos_integer | nil,
    offset: non_neg_integer,
    having: [having_clause],
    distinct?: boolean,
    emulated?: boolean,
    parameters: [DataSource.field],
    views: view_map
  }

  defstruct [
    columns: [], where: [], lcf_check_conditions: [], unsafe_filter_columns: [], group_by: [],
    order_by: [], column_titles: [], info: [], selected_tables: [], property: [], aggregators: [],
    row_splitters: [], implicit_count?: false, data_source: nil, command: nil, show: nil, mode: nil,
    db_columns: [], from: nil, subquery?: false, limit: nil, offset: 0, having: [], distinct?: false,
    features: nil, encoded_where: [], parameters: [], views: %{}, emulated?: false
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a compiled query from a string representation.

  Raises on error.
  """
  @spec make!(DataSource.t, String.t, [DataSource.field], view_map) :: t
  def make!(data_source, string, parameters, views) do
    {:ok, query} = make(data_source, string, parameters, views)
    query
  end

  @doc "Creates a compiled query from a string representation."
  @spec make(DataSource.t, String.t, [DataSource.field], view_map) ::
    {:ok, t} | {:error, String.t}
  def make(data_source, string, parameters, views) do
    with {:ok, parsed_query} <- Parser.parse(data_source, string) do
      Compiler.compile(data_source, parsed_query, parameters, views)
    end
  end

  @doc "Returns the list of unique columns used in the aggregation process."
  @spec aggregated_columns(t) :: [Column.t]
  def aggregated_columns(query) do
    query.aggregators
    |> Enum.flat_map(&Function.arguments/1)
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
      where_conditions: extract_where_conditions(query.where ++ query.lcf_check_conditions ++ query.encoded_where),
      column_types: extract_column_types(query.columns),
      selected_types: selected_types(query.columns),
    }
  end

  @doc """
  Describes the result of a parameterized query.

  This function will return the description of the result, such as column names
  and types, without executing the query.
  """
  @spec describe_query(DataSource.t, String.t, [DataSource.field], view_map) ::
    {:ok, [String.t], map} | {:error, String.t}
  def describe_query(data_source, statement, parameters, views), do:
    with {:ok, query} <- make(data_source, statement, parameters, views), do:
      {:ok, query.column_titles, extract_features(query)}


  @doc "Validates a user-defined view."
  @spec validate_view(DataSource.t, String.t, view_map) :: :ok | {:error, String.t}
  def validate_view(data_source, sql, views), do:
    with {:ok, parsed_query} <- Parser.parse(data_source, sql), do:
      Compiler.validate_view(data_source, parsed_query, views)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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

  defp extract_function(%Column{}), do: []
  defp extract_function({:distinct, param}), do: extract_function(param)
  defp extract_function(function = {:function, _, params}), do: [Function.name(function) | extract_functions(params)]

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

  defp extract_column_type(%Column{constant?: true, type: type}), do: [type]
  defp extract_column_type(%Column{table: :unknown}), do: []
  defp extract_column_type(%Column{table: %{columns: columns}, name: name}), do:
    columns
    |> Enum.filter(& elem(&1, 0) == name)
    |> Enum.map(& elem(&1, 1))

  defp extract_columns(columns), do: Enum.flat_map(columns, &extract_column/1)

  defp extract_column({:function, _, [:*]}), do: []
  defp extract_column({:function, _, params}), do: extract_columns(params)
  defp extract_column({:distinct, value}), do: extract_column(value)
  defp extract_column(%Column{} = column), do: [column]

  defp stringify(string) when is_binary(string), do: string
  defp stringify(atom) when is_atom(atom), do: Atom.to_string(atom)
end
