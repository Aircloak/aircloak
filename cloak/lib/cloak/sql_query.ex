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

  @doc "Returns a list of column titles for the query."
  @spec column_titles(t) :: [String.t]
  def column_titles(%{columns: columns}) do
    Enum.map(columns, &shallow_column_name/1)
  end

  @doc "Returns the list of unique columns used in the aggregation process."
  @spec aggregated_columns(t) :: [String.t]
  def aggregated_columns(query),
    do: (for {:function, _, column} <- query.aggregators, do: column) |> Enum.uniq()

  @doc """
  Returns a list of unique columns needed from the database.

  This function can be used by drivers to get the list of columns which need to
  be retrieved from the database.

  __Note__: a column might have a special name equal to `count_all_column/0`.
  This is a pseudocolumn which doesn't exist in the database. It is the
  responsibility of the driver to check for this value and in its place return a
  `nil` value for each returned row.
  """
  @spec db_columns(t) :: [String.t]
  def db_columns(query) do
    (query.columns ++ Map.get(query, :group_by, []) ++ query.unsafe_filter_columns)
    |> Enum.map(&full_column_name/1)
    |> Enum.uniq()
  end

  @doc "Returns the name of the `user_id` column."
  @spec user_id_column(t) :: String.t
  def user_id_column(query) do
    hd(query.columns)
  end

  @doc "Returns the name of the pseudo count all column. See `db_columns/1` for details."
  @spec count_all_column() :: String.t
  def count_all_column(), do: "ac_count_all_placeholder"

  @doc "Converts a column identifier into a printable name"
  @spec full_column_name(Cloak.SqlQuery.Parser.column) :: String.t | :*
  def full_column_name({:function, "count", :*}), do: count_all_column()
  def full_column_name({:function, _function, identifier}), do: full_column_name(identifier)
  def full_column_name({:distinct, identifier}), do: full_column_name(identifier)
  def full_column_name({:identifier, table, column}), do: "#{table}.#{column}"
  def full_column_name(:*), do: "*"
  def full_column_name(identifier) when is_binary(identifier), do: identifier


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  def shallow_column_name({:function, function, _}), do: function
  def shallow_column_name({:distinct, identifier}), do: shallow_column_name(identifier)
  def shallow_column_name({:identifier, _table, column}), do: column
  def shallow_column_name(identifier) when is_binary(identifier), do: identifier
end
