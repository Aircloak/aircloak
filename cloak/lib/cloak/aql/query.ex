defmodule Cloak.Aql.Query do
  @moduledoc "Handles representing and creating SQL query abstract syntax trees."

  alias Cloak.Aql.{Column, Function, Parser}

  @type t :: %__MODULE__{
    data_source: DataSource.t,
    command: :select | :show,
    columns: [Column.t],
    column_titles: [String.t],
    property: [Function.t],
    aggregators: [Function.t],
    implicit_count: true,
    unsafe_filter_columns: [Column.t],
    group_by: [Function.t],
    where: [Parser.where_clause],
    where_not: [Parser.where_clause],
    order_by: [{pos_integer, :asc | :desc}],
    show: :tables | :columns,
    selected_tables: [DataSource.table],
    mode: :parsed | :unparsed,
    unsafe_subquery: String.t | nil,
    db_id_columns: [Column.t],
    db_data_columns: [Column.t],
    from: Parser.from_clause | nil
  }

  defstruct [
    columns: [], where: [], where_not: [], unsafe_filter_columns: [], group_by: [], order_by: [],
    column_titles: [], info: [], selected_tables: [], property: [], aggregators: [],
    implicit_count: false, data_source: nil, command: nil, show: nil, mode: nil,
    unsafe_subquery: nil, db_id_columns: [], db_data_columns: [], from: nil
  ]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Transforms the analyst provided SQL query from a string format into an abstract syntax tree format.
  This AST can later be used to execute the query against the data store.
  Raises on error.
  """
  @spec make!(DataSource.t, String.t) :: t
  def make!(data_source, string) do
    {:ok, query} = make(data_source, string)
    query
  end

  @doc """
  Transforms the analyst provided SQL query from a string format into an abstract syntax tree format.
  This AST can later be used to execute the query against the data store.
  """
  @spec make(DataSource.t, String.t) :: {:ok, t} | {:error, String.t}
  def make(data_source, string) do
    with {:ok, parsed_query} <- Cloak.Aql.Parser.parse(string) do
      Cloak.Aql.Compiler.compile(data_source, parsed_query)
    end
  end

  @doc "Returns the list of unique columns used in the aggregation process."
  @spec aggregated_columns(t) :: [Column.t]
  def aggregated_columns(query) do
    query.aggregators
    |> Enum.flat_map(&Function.arguments/1)
    |> Enum.uniq()
  end
end
