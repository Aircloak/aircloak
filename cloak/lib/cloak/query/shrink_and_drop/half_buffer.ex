defmodule Cloak.Query.ShrinkAndDrop.HalfBuffer do
  defstruct [:size, :comparator, :min, :max, :users]

  def new(size, comparator), do: %__MODULE__{size: size, comparator: comparator, min: nil, max: nil, users: %{}}

  def under?(%{min: nil}, _), do: false
  def under?(%{max: max}, value), do: value <= max

  def over?(%{max: nil}, _), do: false
  def over?(%{max: max}, value), do: value >= max

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

  def includes?(%{users: users}, user_id), do: Map.has_key?(users, user_id)

  def count(%{users: users}), do: Enum.count(users)

  def has_space?(buffer), do: count(buffer) < buffer.size

  def value_dropping(%{comparator: comparator, users: users}, n) do
    n = min(Enum.count(users) - 1, n)
    users |> Map.values() |> Enum.map(&(&1[:value])) |> Enum.sort_by(&(&1), comparator) |> Enum.drop(n) |> hd()
  end

  def inside(%{users: users}, {x, y}), do:
    users |> Map.values() |> Enum.filter(&(x <= &1[:value] && &1[:value] < y)) |> Enum.flat_map(&(&1[:rows]))

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
