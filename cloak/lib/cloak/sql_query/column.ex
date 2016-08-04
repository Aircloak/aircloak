defmodule Cloak.SqlQuery.Column do
  @moduledoc "Represents a column in a compiled query."

  @type column_type :: Cloak.DataSource.data_type | nil
  @type t :: %__MODULE__{
    table: :unknown | Cloak.DataSource.table,
    name: String.t,
    type: column_type,
    user_id?: boolean,
    db_row_position: nil | non_neg_integer,
    constant?: boolean,
    value: any,
  }
  defstruct [table: :unknown, name: nil, type: nil, user_id?: false, db_row_position: nil, constant?: false, value: nil]

  @doc "Returns a column struct representing the constant `value`."
  @spec constant(column_type, any) :: t
  def constant(type, value) do
    %__MODULE__{table: :unknown, name: :constant, type: type, user_id?: false, constant?: true, value: value}
  end

  @doc "Returns true if the given term is a constant column, false otherwise."
  @spec constant?(Cloak.SqlQuery.Parser.column | t) :: boolean
  def constant?(%__MODULE__{constant?: true}), do: true
  def constant?(_), do: false

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
  def value(%__MODULE__{constant?: true, value: value}, _row), do: value
  def value(column, row), do: Enum.at(row, column.db_row_position)
end
