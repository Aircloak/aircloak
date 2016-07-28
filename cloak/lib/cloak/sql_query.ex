defmodule Cloak.SqlQuery do
  @moduledoc "Handles representing and creating SQL query abstract syntax trees."

  @type t :: Compiler.compiled_query


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Transforms the analyst provided SQL query from a string format into an abstract syntax tree format.
  This AST can later be used to execute the query against the data store.
  Raises on error.
  """
  @spec make!(atom, String.t) :: t
  def make!(data_source, string) do
    {:ok, query} = make(data_source, string)
    query
  end

  @doc """
  Transforms the analyst provided SQL query from a string format into an abstract syntax tree format.
  This AST can later be used to execute the query against the data store.
  """
  @spec make(atom, String.t) :: {:ok, t} | {:error, String.t}
  def make(data_source, string) do
    with {:ok, parsed_query} <- Cloak.SqlQuery.Parser.parse(string) do
      Cloak.SqlQuery.Compiler.compile(data_source, parsed_query)
    end
  end

  @doc "Returns the list of unique columns used in the aggregation process."
  @spec aggregated_columns(t) :: [String.t]
  def aggregated_columns(query),
    do: (for {:function, _, column} <- query.aggregators, do: column) |> Enum.uniq()

  @doc """
  Returns a list of unique columns needed from the database.

  This function can be used by drivers to get the list of columns which need to
  be retrieved from the database.

  __Note__: a column might have a special name equal to `:*`.
  This is a pseudocolumn which doesn't exist in the database. It is the
  responsibility of the driver to check for this value and in its place return a
  `nil` value for each returned row.
  """
  @spec db_columns(t) :: [String.t]
  def db_columns(query) do
    (query.columns ++ query.group_by ++ query.unsafe_filter_columns)
    |> Enum.map(&full_column_name/1)
    |> Enum.uniq()
    |> Enum.reject(&:* == &1)
  end

  @doc "Converts a column identifier into a printable name"
  @spec full_column_name(Cloak.SqlQuery.Parser.column) :: String.t | :*
  def full_column_name({:function, "count", :*}), do: :*
  def full_column_name({:function, _function, identifier}), do: full_column_name(identifier)
  def full_column_name({:distinct, identifier}), do: full_column_name(identifier)
  # This case is needed for DS Proxy. We can't qualify the identifiers when
  # we don't know what tables are part of the select, therefore we have to
  # pass through unknown tables, and drop them from the names here.
  def full_column_name({:identifier, :unknown, column}), do: column
  def full_column_name({:identifier, table, column}), do: "#{table}.#{column}"
end
