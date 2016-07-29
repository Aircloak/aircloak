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
end
