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
    %__MODULE__{constant?: true, value: value, type: normalize_type(type), name: :constant}
  end

  @doc "Returns true if the given term is a constant column, false otherwise."
  @spec constant?(Cloak.SqlQuery.Parser.column | t) :: boolean
  def constant?(%__MODULE__{constant?: true}), do: true
  def constant?(_), do: false

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
  for position <- 0..99 do
    # Generates pattern matching clauses to improve sequential access to a value:
    #
    #   defp value(%__MODULE__{db_row_position: 0}, [el | _]), do: el
    #   defp value(%__MODULE__{db_row_position: 1}, [_, el | _]), do: el
    #   defp value(%__MODULE__{db_row_position: 2}, [_, _, el | _]), do: el
    #   ...
    #
    # This works faster than `Enum.at`, especially if positions are larger.
    matched_value = quote do: value
    matched_row_head = List.duplicate(quote(do: _), position) ++ [matched_value]
    matched_row = quote do: [unquote_splicing(matched_row_head) | _]

    def value(%__MODULE__{db_row_position: unquote(position)}, unquote(matched_row)),
      do: unquote(matched_value)
  end
  # Fallback to `Enum.at` for larger positions
  def value(column, row), do: Enum.at(row, column.db_row_position)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_type(:string), do: :text
  defp normalize_type(:float), do: :real
  defp normalize_type(type), do: type
end
