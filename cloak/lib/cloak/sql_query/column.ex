defmodule Cloak.SqlQuery.Column do
  @moduledoc "Represents a column in a compiled query."

  @type column_type :: Cloak.DataSource.data_type | nil
  @type t :: %__MODULE__{
    table: :unknown | Cloak.DataSource.table,
    name: String.t,
    type: column_type,
    user_id?: boolean,
    constant?: boolean,
    value: any,
  }
  defstruct [table: :unknown, name: nil, type: nil, user_id?: false, constant?: false, value: nil]

  @doc "Returns a column struct representing the constant `value`."
  @spec constant(column_type, any) :: t
  def constant(type, value) do
    %__MODULE__{table: :unknown, name: :constant, type: type, user_id?: false, constant?: true, value: value}
  end

  @doc """
  Returns the column alias.

  The alias is unique in the query scope, and can be used by data source drivers
  to uniquely distinguish between different names.
  """
  @spec alias(t) :: String.t
  def alias(%__MODULE__{table: :unknown, name: name}), do: name
  def alias(column), do: "#{column.table.user_name}.#{column.name}"

  @doc """
  Returns a display name of the column.

  This function should mostly be used when producing error messages.
  """
  @spec display_name(t) :: String.t
  def display_name(column) do
    "column `#{column.name}` from table `#{column.table.user_name}`"
  end
end
