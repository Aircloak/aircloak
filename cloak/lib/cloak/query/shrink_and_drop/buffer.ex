defmodule Cloak.Query.ShrinkAndDrop.Buffer do
  alias Cloak.Query.ShrinkAndDrop.HalfBuffer

  defstruct [:left, :right]

  def new(size), do: %__MODULE__{
    left: HalfBuffer.new(size, &Kernel.</2),
    right: HalfBuffer.new(size, &Kernel.>/2)
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
