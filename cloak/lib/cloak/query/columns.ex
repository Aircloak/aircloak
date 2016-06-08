defmodule Cloak.Query.Columns do
  def all(query, options \\ [])
  def all(query, user_id: true), do: [:user_id | all(query)]
  def all(%{columns: columns, filter_columns: filter_columns} = query, []) do
    unselected_group_by_columns = Map.get(query, :group_by, []) -- columns

    columns ++ unselected_group_by_columns ++ filter_columns
  end

  def index(column, query, options \\ []) do
    Enum.find_index(all(query, options), &(&1 == column))
  end
end
