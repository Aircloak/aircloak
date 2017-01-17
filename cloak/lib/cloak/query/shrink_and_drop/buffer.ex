defmodule Cloak.Query.ShrinkAndDrop.Buffer do
  @moduledoc """
  Implements the buffer used for the shrink and drop algorithm. For an overview see "Shrink and drop" in
  docs/anonymization.md.
  """

  alias Cloak.Query.ShrinkAndDrop.HalfBuffer
  alias Cloak.Sql.FixAlign

  defstruct [:left, :right]

  @type t :: %__MODULE__{left: HalfBuffer.t, right: HalfBuffer.t}
  @type row :: {row_id, user_id, row_value, row_data}
  @type row_id :: integer
  @type user_id :: any
  @type row_value :: number
  @type row_data :: any


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns an empty buffer with the given size limit for each side."
  @spec new(pos_integer) :: t
  def new(size), do: %__MODULE__{
    left: HalfBuffer.new(size, &Cloak.Data.lt_eq/2),
    right: HalfBuffer.new(size, &Cloak.Data.gt_eq/2)
  }

  @doc """
  Adds the row to the buffer. Returns the new state of the buffer along with any rows that needed to be removed and are
  now safe to emit due to the buffer overflowing.
  """
  @spec add(t, row) :: {t, [row]}
  def add(buffer, row) do
    {new_buffer, popped} = do_add(row, buffer)
    {new_buffer, popped} = Enum.reduce(popped, {new_buffer, []}, fn(row = {_, user_id, value, _}, {buffer, popped}) ->
      cond do
        HalfBuffer.includes?(buffer.left, user_id) ->
          {new_left, popped_left} = HalfBuffer.add(buffer.left, value, user_id, row)
          {%{buffer | left: new_left}, popped_left ++ popped}
        HalfBuffer.includes?(buffer.right, user_id) ->
          {new_right, popped_right} = HalfBuffer.add(buffer.right, value, user_id, row)
          {%{buffer | right: new_right}, popped_right ++ popped}
        true -> {buffer, [row | popped]}
      end
    end)
    {new_buffer, Enum.uniq(popped)}
  end

  @doc "Returns all rows in this buffer that fall in the given interval."
  @spec inside(t, FixAlign.interval(number)) :: [row]
  def inside(%{left: left, right: right}, interval), do:
    HalfBuffer.inside(left, interval) ++ HalfBuffer.inside(right, interval) |> Enum.uniq()

  @doc "Returns true if the buffer is empty, false otherwise."
  @spec empty?(t) :: boolean
  def empty?(%{left: left, right: right}), do: HalfBuffer.empty?(left) && HalfBuffer.empty?(right)

  @doc "Returns a pair {x, y} such that when ignoring n outermost entries all data in the buffer is >= x and <= y."
  @spec range_except_extreme(t, non_neg_integer) :: FixAlign.interval(number)
  def range_except_extreme(%{left: left, right: right}, n) do
    x = HalfBuffer.values_except_extreme(left, n)
    y = HalfBuffer.values_except_extreme(right, n)
    {Cloak.Data.min(x, y), Cloak.Data.max(x, y)}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_add(row = {_, user_id, value, _}, buffer = %{left: left, right: right}) do
    cond do
      HalfBuffer.has_space?(left) && HalfBuffer.has_space?(right) ->
        # <--LEFT-->    <--RIGHT-->
        #            x
        # The buffers have still not reached their capacity, so they need to both be extended with the row.
        {new_left, []} = HalfBuffer.add(left, value, user_id, row)
        {new_right, []} = HalfBuffer.add(right, value, user_id, row)
        {%{buffer | left: new_left, right: new_right}, []}

      HalfBuffer.over?(left, value) && HalfBuffer.under?(right, value) ->
        # <------LEFT------>  x  <------RIGHT----->
        # The row is safe to emit, because it won't ever fall into either half of the buffer.
        {buffer, [row]}

      not HalfBuffer.over?(left, value) && not HalfBuffer.under?(right, value) ->
        # <------LEFT------>
        #             <------RIGHT----->
        #               x
        #
        # The buffers currently overlap and the row must go into both of them.
        {new_left, popped_left} = HalfBuffer.add(left, value, user_id, row)
        {new_right, popped_right} = HalfBuffer.add(right, value, user_id, row)
        {%{buffer | left: new_left, right: new_right}, popped_left ++ popped_right |> Enum.uniq}

      not HalfBuffer.over?(left, value) ->
        # <------LEFT------>
        #             <------RIGHT----->
        #       x
        #
        # The row is outside of the right buffer, so it only goes into the left one. They might overlap, but it doesn't
        # matter.
        {new_left, popped_left} = HalfBuffer.add(left, value, user_id, row)
        {%{buffer | left: new_left}, popped_left}

      not HalfBuffer.under?(right, value) ->
        # <------LEFT------>
        #             <------RIGHT----->
        #                        x
        #
        # The row is outside of the left buffer, so it only goes into the right one. They might overlap, but it doesn't
        # matter.
        {new_right, popped_right} = HalfBuffer.add(right, value, user_id, row)
        {%{buffer | right: new_right}, popped_right}
    end
  end
end
