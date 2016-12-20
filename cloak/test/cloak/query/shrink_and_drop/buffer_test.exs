defmodule Cloak.Query.ShrinkAndDrop.Buffer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.ShrinkAndDrop.Buffer

  test "pushing data out" do
    buffer =
      Buffer.new(2)
      |> add_to_buffer({1, "user1", 90, []})
      |> add_to_buffer({2, "user2", 180, []})
      |> add_to_buffer({3, "user3", 180, []})

    assert {180, 180} = Buffer.range_except_extreme(buffer, 1)
  end

  defp add_to_buffer(buffer, value) do
    {new_buffer, _popped} = Buffer.add(buffer, value)
    new_buffer
  end
end
