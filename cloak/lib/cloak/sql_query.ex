defmodule Cloak.SqlQuery do
  @moduledoc "Handles representing and creating SQL query abstract syntax trees."

  @type t :: Compiler.compiled_query

  alias Cloak.SqlQuery.Column


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
  @spec aggregated_columns(t) :: [Column.t]
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
  @spec db_columns(t) :: [Column.t]
  def db_columns(query) do
    (query.db_columns ++ query.group_by ++ query.unsafe_filter_columns)
    |> Enum.map(&extract_column/1)
    |> Enum.uniq()
    |> Enum.reject(&:* == &1)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp extract_column({:function, "count", :*}), do: :*
  defp extract_column({:function, _function, identifier}), do: extract_column(identifier)
  defp extract_column({:distinct, identifier}), do: extract_column(identifier)
  defp extract_column(%Column{} = column), do: column
end
