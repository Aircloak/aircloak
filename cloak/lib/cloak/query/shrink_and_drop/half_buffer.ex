defmodule Cloak.Query.ShrinkAndDrop.HalfBuffer do
  @moduledoc """
  Implements one side of the buffer for the shrink and drop algorithm. For an overwie see "Shrink and drop" in
  docs/anonymization.md.
  """

  alias Cloak.Query.ShrinkAndDrop.Buffer

  defstruct [:size, :comparator, :min, :max, :users]

  @type t :: %__MODULE__{
    size: pos_integer,
    comparator: comparator,
    min: nil | Buffer.row_value,
    max: nil | Buffer.row_value,
    users: %{Buffer.row_id => Buffer.row_data},
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
  def under?(%{max: max}, value), do: value <= max

  @doc "Returns true if the value is greater than or equal to the largest value in this buffer, false otherwise."
  @spec over?(t, Buffer.row_value) :: boolean
  def over?(%{max: nil}, _), do: false
  def over?(%{max: max}, value), do: value >= max

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
      min: min(buffer.min, value),
      max: max(buffer.max, value),
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
  @spec value_dropping(t, non_neg_integer) :: number
  def value_dropping(%{comparator: comparator, users: users}, n) do
    n = min(Enum.count(users) - 1, n)
    users |> Map.values() |> Enum.map(&(&1[:value])) |> Enum.sort_by(&(&1), comparator) |> Enum.drop(n) |> hd()
  end

  @doc "Returns all data in the buffer for which the value is included in the given interval."
  @spec inside(t, FixAlign.interval(number)) :: [Buffer.row_data]
  def inside(%{users: users}, {x, y}), do:
    users |> Map.values() |> Enum.filter(&(x <= &1[:value] && &1[:value] < y)) |> Enum.flat_map(&(&1[:rows]))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp pop_as_needed(buffer = %{size: size, users: users}), do:
    if Enum.count(users) > size, do: pop(buffer), else: {buffer, []}

  defp pop(buffer) do
    {user_to_pop, _} = Enum.reduce(buffer.users, Enum.at(buffer.users, 0), fn
      (user = {_, %{value: x}}, found = {_, %{value: y}}) -> if buffer.comparator.(y, x), do: user, else: found
    end)
    {%{rows: rows}, new_users} = Map.pop(buffer.users, user_to_pop)

    {
      %{
        buffer |
        users: new_users,
        min: new_users |> Map.values() |> Enum.map(&(&1[:value])) |> Enum.min(),
        max: new_users |> Map.values() |> Enum.map(&(&1[:value])) |> Enum.max(),
      },
      rows
    }
  end
end
