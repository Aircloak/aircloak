defmodule Cloak.SqlQuery do
  @moduledoc "Handles representing and creating SQL queries in structured form."

  @type t :: Compiler.compiled_query


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Transforms a string into a structured SQL query ready for execution. Raises on error."
  @spec make!(atom, String.t) :: t
  def make!(data_source, string) do
    {:ok, query} = make(data_source, string)
    query
  end

  @doc "Transforms a string into a structured SQL query ready for execution."
  @spec make(atom, String.t) :: {:ok, t} | {:error, String.t}
  def make(data_source, string) do
    with {:ok, parsed_query} <- Cloak.SqlQuery.Parser.parse(string) do
      Cloak.SqlQuery.Compiler.compile(data_source, parsed_query)
    end
  end

  @doc "Returns a list of column titles for the query."
  @spec column_titles(t) :: [String.t]
  def column_titles(%{columns: columns}) do
    Enum.map(columns, &column_title/1)
  end

  @doc "Returns a string title for the given column specification."
  @spec column_title(Parser.column) :: String.t
  def column_title({:count, :star}), do: "count(*)"
  def column_title(column), do: column
end
