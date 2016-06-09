defmodule Cloak.Query.Columns do
  @moduledoc "Functions from extracting column information from a query."

  alias Cloak.SqlQuery.Compiler
  alias Cloak.SqlQuery.Parser

  @type options :: [] | [user_id: true]

  @doc """
  Returns all columns the query selects. If a `user_id: true` option is provided, then the user_id
  column will be included in the list. Includes grouping columns and ones used to implement negated
  where conditions.
  """
  @spec all(Compiler.compiled_query, options) :: [Parser.column]
  def all(query, options \\ [])
  def all(query, user_id: true), do: [:user_id | all(query)]
  def all(%{columns: columns, unsafe_filter_columns: unsafe_filter_columns} = query, _) do
    unselected_group_by_columns = Map.get(query, :group_by, [])
    columns ++ unselected_group_by_columns ++ unsafe_filter_columns
  end

  @doc "Convenience function. Returns the index of the given column in `Columns.all`."
  @spec index(Parser.column, Compiler.compiled_query, options) :: pos_integer | nil
  def index(column, query, options \\ []) do
    Enum.find_index(all(query, options), &(&1 == column))
  end
end
