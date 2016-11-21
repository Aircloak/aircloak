defmodule Cloak.Query.ShrinkAndDrop do
  alias Cloak.Query.Anonymizer
  alias Cloak.Aql.{FixAlign, Function}

  defmodule HalfBuffer do
    defstruct [:size, :comparator, :min, :max, :users]

    def new(size, comparator), do: %HalfBuffer{size: size, comparator: comparator, min: nil, max: nil, users: %{}}

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

  defmodule Buffer do
    defstruct [:left, :right]

    @extremes_to_keep 11

    def new, do: %Buffer{
      left: HalfBuffer.new(@extremes_to_keep, &Kernel.</2),
      right: HalfBuffer.new(@extremes_to_keep, &Kernel.>/2)
    }

    def add(row, buffer) do
      {new_buffer, popped} = do_add(row, buffer)
      {new_buffer, popped} = Enum.reduce(popped, {new_buffer, []}, fn(row = {_, user_id, value, _}, {buffer, popped}) ->
        cond do
          HalfBuffer.includes?(buffer.left, user_id) ->
            {new_left, popped_left} = HalfBuffer.add(buffer.left, user_id, value, row)
            {%{buffer | left: new_left}, popped_left ++ popped}
          HalfBuffer.includes?(buffer.right, user_id) ->
            {new_right, popped_right} = HalfBuffer.add(buffer.right, user_id, value, row)
            {%{buffer | right: new_right}, popped_right ++ popped}
          true -> {buffer, [row | popped]}
        end
      end)
      {new_buffer, Enum.uniq(popped)}
    end

    def inside(%{left: left, right: right}, interval), do:
      HalfBuffer.inside(left, interval) ++ HalfBuffer.inside(right, interval) |> Enum.uniq()

    def count(%{left: left, right: right}), do:
      max(HalfBuffer.count(left), HalfBuffer.count(right))

    def empty?(buffer), do: count(buffer) == 0

    def range_dropping(%{left: left, right: right}, n) do
      x = HalfBuffer.value_dropping(left, n)
      y = HalfBuffer.value_dropping(right, n)
      {min(x, y), max(x, y)}
    end

    defp do_add(row = {_, user_id, value, _}, buffer = %{left: left, right: right}) do
      cond do
        HalfBuffer.has_space?(left) && HalfBuffer.has_space?(right) ->
          {new_left, []} = HalfBuffer.add(left, value, user_id, row)
          {new_right, []} = HalfBuffer.add(right, value, user_id, row)
          {%{buffer | left: new_left, right: new_right}, []}
        HalfBuffer.over?(left, value) && HalfBuffer.under?(right, value) -> {buffer, [row]}
        not HalfBuffer.over?(left, value) && not HalfBuffer.under?(right, value) ->
          {new_left, popped_left} = HalfBuffer.add(left, value, user_id, row)
          {new_right, popped_right} = HalfBuffer.add(right, value, user_id, row)
          {%{buffer | left: new_left, right: new_right}, popped_left ++ popped_right |> Enum.uniq}
        not HalfBuffer.over?(left, value) ->
          {new_left, popped_left} = HalfBuffer.add(left, value, user_id, row)
          {%{buffer | left: new_left}, popped_left}
        not HalfBuffer.under?(right, value) ->
          {new_right, popped_right} = HalfBuffer.add(right, value, user_id, row)
          {%{buffer | right: new_right}, popped_right}
      end
    end
  end

  def apply(rows, query), do: Enum.reduce(query.ranges, rows, &suppress_outliers/2)

  @supported_types [:integer, :real]
  defp suppress_outliers({column, _range}, rows) do
    if Enum.member?(@supported_types, Function.type(column)) do
      rows
      |> Stream.concat([:done])
      |> Stream.transform(%{next_id: 0, buffer: Buffer.new}, &do_suppress_outliers(&1, &2, column))
    else
      rows
    end
  end

  defp do_suppress_outliers(:done, %{buffer: buffer}, _column) do
    if Buffer.empty?(buffer) do
      {[], nil}
    else
      emit_buffer(buffer)
    end
  end
  defp do_suppress_outliers(row, %{next_id: next_id, buffer: buffer}, column) do
    {new_buffer, popped_rows} = row |> decorate_row(next_id, column) |> Buffer.add(buffer)
    {Enum.map(popped_rows, &undecorate_row/1), %{next_id: next_id + 1, buffer: new_buffer}}
  end

  defp emit_buffer(buffer) do
    interval =
      buffer
      |> Buffer.range_dropping(Buffer.count(buffer) - 1)
      |> case do
        {x, x} -> {x, x + epsilon(x)}
        interval -> FixAlign.align_interval(interval)
      end

    {buffer |> Buffer.inside(interval) |> Enum.map(&undecorate_row/1), nil}
  end

  # Very small number such that x + epsilon(x) > x
  defp epsilon(x), do: x / 100000000000000

  defp decorate_row(row, id, column), do: {id, user_id(row), value(row, column), row}

  defp undecorate_row({_, _, _, row}), do: row

  defp user_id([id | _rest]), do: id

  defp value(row, column), do: Function.apply_to_db_row(column, row)
end
