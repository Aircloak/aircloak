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
    Enum.map(columns, &Cloak.SqlQuery.Compiler.column_title/1)
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
    |> Enum.map(&column_name/1)
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


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp column_name({:function, "count", :*}), do: count_all_column()
  defp column_name({:function, _function, identifier}), do: identifier
  defp column_name(column) when is_binary(column), do: column
end
