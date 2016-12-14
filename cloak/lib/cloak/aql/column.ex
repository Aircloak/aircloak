defmodule Cloak.Aql.Column do
  @moduledoc "Represents a column in a compiled query."

  alias Cloak.DataSource

  @type column_type :: DataSource.data_type | nil
  @type db_function :: String.t | {:cast, DataSource.data_type | :varbinary}
  @type t :: %__MODULE__{
    table: :unknown | DataSource.table,
    name: String.t | :constant | nil,
    alias: String.t | nil,
    type: column_type,
    user_id?: boolean,
    db_row_position: nil | non_neg_integer,
    constant?: boolean,
    value: any,
    db_function: db_function | nil,
    db_function_args: [t],
    aggregate?: boolean
  }
  defstruct [
    table: :unknown, name: nil, alias: nil, type: nil, user_id?: false, db_row_position: nil, constant?: false,
    value: nil, db_function: nil, db_function_args: [], aggregate?: false
  ]

  @doc "Returns a column struct representing the constant `value`."
  @spec constant(column_type, any) :: t
  def constant(type, value) do
    %__MODULE__{constant?: true, value: value, type: normalize_type(type), name: :constant}
  end

  @doc "Creates a column representing a database function call."
  @spec db_function(db_function, [t], column_type, boolean) :: t
  def db_function(db_function, db_function_args, type \\ nil, aggregate? \\ false) do
    %__MODULE__{db_function: db_function, db_function_args: db_function_args, type: type, aggregate?: aggregate?}
  end

  @doc "Returns true if the given term is a constant column, false otherwise."
  @spec constant?(Cloak.Aql.Parser.column | t) :: boolean
  def constant?(%__MODULE__{constant?: true}), do: true
  def constant?(_), do: false

  @doc "Returns true if the given term represents a database function call."
  @spec db_function?(Cloak.Aql.Parser.column | t) :: boolean
  def db_function?(%__MODULE__{db_function: fun}) when fun != nil, do: true
  def db_function?(_), do: false

  @doc "Returns true if the given term represents a database function call."
  @spec aggregate_db_function?(Cloak.Aql.Parser.column | t) :: boolean
  def aggregate_db_function?(column), do: db_function?(column) && column.aggregate?

  @doc """
  Returns a display name of the column.

  This function should mostly be used when producing error messages.
  """
  @spec display_name(t) :: String.t
  def display_name({:function, _name, args}) do
    args
    |> Enum.map(&display_name/1)
    |> Enum.filter(&(&1))
    |> hd()
  end
  def display_name(%__MODULE__{constant?: true}), do: nil
  def display_name(%__MODULE__{name: name, table: table}) when is_binary(name), do:
    "`#{name}` from table `#{table.name}`"
  def display_name(%__MODULE__{alias: alias}) when is_binary(alias), do: "`#{alias}`"
  def display_name(%__MODULE__{db_function: function}) when is_binary(function), do: "`#{function}`"

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

  @doc "Checks two columns for equality."
  @spec equals(any, any) :: boolean
  def equals({:distinct, c1}, {:distinct, c2}), do: equals(c1, c2)
  def equals(:*, :*), do: true
  def equals(%__MODULE__{} = c1, %__MODULE__{} = c2), do:
    c1.table == c2.table and
    c1.name == c2.name and
    c1.value == c2.value and
    c1.db_function == c2.db_function and
    Enum.zip(c1.db_function_args, c2.db_function_args) |> Enum.all?(fn ({arg1, arg2}) -> equals(arg1, arg2) end)
  def equals(_c1, _c2), do: false


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_type(:string), do: :text
  defp normalize_type(:float), do: :real
  defp normalize_type(type), do: type
end
