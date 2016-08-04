defmodule Cloak.SqlQuery.Column do
  @moduledoc "Represents a column in a compiled query."

  @type t :: %__MODULE__{
    table: :unknown | Cloak.DataSource.table,
    name: String.t,
    type: Cloak.DataSource.data_type | nil,
    user_id?: boolean,
    in_scope?: boolean,
    db_row_position: nil | non_neg_integer
  }
  defstruct [:table, :name, :type, :user_id?, :db_row_position, :in_scope?]

  @doc """
  Returns the column alias.

  The alias is unique in the query scope, and can be used by data source drivers
  to uniquely distinguish between different names.
  """
  @spec alias(t) :: String.t
  def alias(%__MODULE__{table: :unknown, name: name}), do: name
  def alias(column), do: "#{column.table.name}.#{column.name}"

  @doc """
  Returns a display name of the column.

  This function should mostly be used when producing error messages.
  """
  @spec display_name(t) :: String.t
  def display_name(column) do
    "column `#{column.name}` from table `#{column.table.name}`"
  end

  @doc "Returns the column value of a database row."
  @spec value(t, DataSource.row) :: DataSource.field
  def value(column, row), do: Enum.at(row, column.db_row_position)
end
