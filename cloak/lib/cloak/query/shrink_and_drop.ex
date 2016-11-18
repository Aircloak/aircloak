defmodule Cloak.Query.ShrinkAndDrop do
  alias Cloak.Query.Anonymizer
  alias Cloak.Aql.{FixAlign, Function}

  defmodule HalfBuffer do
    defstruct [:size, :min, :max, :users]

    def new(size), do: %HalfBuffer{size: size, min: nil, max: nil, users: %{}}

    def under?(%{min: nil}, _), do: false
    def under?(%{max: max}, value), do: value <= max

    def over?(%{max: nil}, _), do: false
    def over?(%{max: max}, value), do: value >= max

    def add(buffer = %{min: nil}, value, user_id, row, _comparator), do:
      {%{buffer | min: value, max: value, users: %{user_id => %{value: value, rows: [row]}}}, []}
    def add(buffer, value, user_id, row, comparator) do
      %{
        buffer |
        min: min(buffer.min, value),
        max: max(buffer.max, value),
        users: Map.update(buffer.users, user_id, %{value: value, rows: [row]}, fn(%{value: old_value, rows: rows}) ->
          %{value: (if comparator.(old_value, value), do: old_value, else: value), rows: [row | rows]}
        end),
      }
      |> pop_as_needed(comparator)
    end

    def includes?(%{users: users}, user_id), do: Map.has_key?(users, user_id)

    def all(%{users: users}), do: users |> Map.values() |> Enum.map(&(&1[:rows])) |> Enum.concat()

    defp pop_as_needed(buffer = %{size: size, users: users}, comparator) do
      if Enum.count(users) > size do
        pop(buffer, comparator)
      else
        {buffer, []}
      end
    end

    defp pop(buffer, comparator) do
      {user_to_pop, _} = Enum.reduce(buffer.users, Enum.at(buffer.users, 0), fn
        (user = {_, %{value: x}}, found = {_, %{value: y}}) -> if comparator.(y, x), do: user, else: found
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

  defmodule Buffer do
    defstruct [:left, :right]

    @extremes_to_keep 11

    def new, do: %Buffer{left: HalfBuffer.new(@extremes_to_keep), right: HalfBuffer.new(@extremes_to_keep)}

    def add(row, buffer) do
      {new_buffer, popped} = do_add(row, buffer)
      {new_buffer, popped} = Enum.reduce(popped, {new_buffer, []}, fn(row = {_, user_id, value, _}, {buffer, popped}) ->
        cond do
          HalfBuffer.includes?(buffer.left, user_id) ->
            {%{buffer | left: HalfBuffer.add(buffer.left, user_id, value, row, &Kernel.</2)}, popped}
          HalfBuffer.includes?(buffer.right, user_id) ->
            {%{buffer | right: HalfBuffer.add(buffer.right, user_id, value, row, &Kernel.>/2)}, popped}
          true -> {buffer, [row | popped]}
        end
      end)
      {new_buffer, Enum.uniq(popped)}
    end

    def all(%{left: left, right: right}) do
      HalfBuffer.all(left) ++ HalfBuffer.all(right) |> Enum.uniq()
    end

    defp do_add(row = {_, user_id, value, _}, buffer = %{left: left, right: right}) do
      cond do
        HalfBuffer.over?(left, value) && HalfBuffer.under?(right, value) -> {buffer, [row]}
        not HalfBuffer.over?(left, value) && not HalfBuffer.under?(right, value) ->
          {new_left, popped_left} = HalfBuffer.add(left, value, user_id, row, &Kernel.</2)
          {new_right, popped_right} = HalfBuffer.add(right, value, user_id, row, &Kernel.>/2)
          {%{buffer | left: new_left, right: new_right}, popped_left ++ popped_right |> Enum.uniq}
        not HalfBuffer.over?(left, value) ->
          {new_left, popped_left} = HalfBuffer.add(left, value, user_id, row, &Kernel.</2)
          {%{buffer | left: new_left}, popped_left}
        not HalfBuffer.under?(right, value) ->
          {new_right, popped_right} = HalfBuffer.add(right, value, user_id, row, &Kernel.>/2)
          {%{buffer | right: new_right}, popped_right}
      end
    end
  end

  def apply(rows, query), do: Enum.reduce(query.ranges, rows, &suppress_outliers/2)

  defp suppress_outliers({column, _range}, rows) do
    rows
    |> Stream.concat([:done])
    |> Stream.transform(%{next_id: 0, buffer: Buffer.new}, &do_suppress_outliers(&1, &2, column))
  end

  defp do_suppress_outliers(:done, %{buffer: buffer}, _column) do
    {buffer |> Buffer.all() |> Enum.map(&undecorate_row/1), nil}
  end
  defp do_suppress_outliers(row, %{next_id: next_id, buffer: buffer}, column) do
    {new_buffer, popped_rows} = row |> decorate_row(next_id, column) |> Buffer.add(buffer)
    {Enum.map(popped_rows, &undecorate_row/1), %{next_id: next_id + 1, buffer: new_buffer}}
  end

  defp decorate_row(row, id, column), do: {id, user_id(row), value(row, column), row}

  defp undecorate_row({_, _, _, row}), do: row

  defp user_id([id | _rest]), do: id

  defp value(row, column), do: Function.apply_to_db_row(column, row)

  defp compact_range(rows, column) do
    values = Enum.map(rows, &Function.apply_to_db_row(column, &1))
    case {Enum.min(values), Enum.max(values)} do
      {x, x} -> {x, x + 1}
      {x, y} -> FixAlign.align_interval({x, y})
    end
  end
end
