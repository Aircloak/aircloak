defmodule Cloak.Query.ShrinkAndDrop.HalfBuffer do
  @moduledoc """
  Implements one side of the buffer for the shrink and drop algorithm. For an overview see "Shrink and drop" in
  docs/anonymization.md.
  """

  alias Cloak.Query.ShrinkAndDrop.Buffer

  defstruct [:size, :comparator, :min, :max, :users]

  @type t :: %__MODULE__{
    size: pos_integer,
    comparator: comparator,
    min: nil | Buffer.row_value,
    max: nil | Buffer.row_value,
    users: %{Buffer.user_id => Buffer.row_data},
  }
  @type comparator :: (Buffer.row_value, Buffer.row_value -> boolean)


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns an empty HalfBuffer."
  @spec new(pos_integer, comparator) :: t
  def new(size, comparator), do: %__MODULE__{size: size, comparator: comparator, min: nil, max: nil, users: %{}}

  @doc "Returns true if the value is less than or equal to the smallest value in this buffer, false otherwise."
  @spec under?(t, Buffer.row_value) :: boolean
  def under?(%{min: nil}, _), do: false
  def under?(%{min: min}, value), do: Cloak.Data.lt(value, min)

  @doc "Returns true if the value is greater than or equal to the largest value in this buffer, false otherwise."
  @spec over?(t, Buffer.row_value) :: boolean
  def over?(%{max: nil}, _), do: false
  def over?(%{max: max}, value), do: Cloak.Data.lt_eq(max, value)

  @doc """
  Adds the row to the buffer. Returns the new state of the buffer along with any rows that needed to be removed due to
  the buffer overflowing.
  """
  @spec add(t, Buffer.row_value, Buffer.user_id, Buffer.row_data) :: {t, [Buffer.row_data]}
  def add(buffer = %{min: nil}, value, user_id, row), do:
    {%{buffer | min: value, max: value, users: %{user_id => %{value: value, rows: [row]}}}, []}
  def add(buffer, value, user_id, row) do
    %{
      buffer |
      min: Cloak.Data.min(buffer.min, value),
      max: Cloak.Data.max(buffer.max, value),
      users: Map.update(buffer.users, user_id, %{value: value, rows: [row]}, fn(%{value: old_value, rows: rows}) ->
        %{value: (if buffer.comparator.(old_value, value), do: old_value, else: value), rows: [row | rows]}
      end),
    }
    |> pop_as_needed()
  end

  @doc "Returns true if the buffer has any data with the given user_id, false otherwise."
  @spec includes?(t, Buffer.user_id) :: boolean
  def includes?(%{users: users}, user_id), do: Map.has_key?(users, user_id)

  @doc "Returns true if the buffer has no data, false otherwise."
  @spec empty?(t) :: boolean
  def empty?(%{users: users}), do: Enum.empty?(users)

  @doc "Returns true if data can be added to the buffer without overflow, false otherwise."
  @spec has_space?(t) :: boolean
  def has_space?(%{size: size, users: users}), do: Enum.count(users) < size

  @doc """
  Returns a number such that all data in the buffer except for the outermost n users is "inside" that number with
  respect to the buffer's comparator.
  """
  @spec values_except_extreme(t, non_neg_integer) :: number
  def values_except_extreme(%{comparator: comparator, users: users}, n) do
    n = min(Enum.count(users) - 1, n)
    users |> Map.values() |> Enum.map(&(&1[:value])) |> Enum.sort_by(&(&1), comparator) |> Enum.drop(n) |> hd()
  end

  @doc "Returns all data in the buffer for which the value is included in the given interval."
  @spec inside(t, Cloak.Sql.FixAlign.interval(number)) :: [Buffer.row_data]
  def inside(%{users: users}, {x, y}) do
    users
    |> Map.values()
    |> Enum.flat_map(&(&1[:rows]))
    |> Enum.filter(fn({_, _, value, _}) -> Cloak.Data.lt_eq(x, value) && Cloak.Data.gt(y, value) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pop_as_needed(buffer = %{size: size, users: users}), do:
    if Enum.count(users) > size, do: pop(buffer), else: {buffer, []}

  defp pop(buffer) do
    {user_to_pop, _} = Enum.reduce(buffer.users, Enum.at(buffer.users, 0), &to_pop(buffer.comparator, &1, &2))
    {%{rows: rows}, new_users} = Map.pop(buffer.users, user_to_pop)

    {
      %{
        buffer |
        users: new_users,
        min: new_users |> Map.values() |> Enum.map(&(&1[:value])) |> Enum.reduce(&Cloak.Data.min/2),
        max: new_users |> Map.values() |> Enum.map(&(&1[:value])) |> Enum.reduce(&Cloak.Data.max/2),
      },
      rows
    }
  end

  defp to_pop(comparator, x = {user_id_x, %{value: x_value}}, y = {user_id_y, %{value: y_value}}) do
    cond do
      x_value == y_value && user_id_x < user_id_y -> x
      x_value == y_value -> y
      comparator.(y_value, x_value) -> x
      true -> y
    end
  end
end
