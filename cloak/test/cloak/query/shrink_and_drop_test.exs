defmodule Cloak.Query.ShrinkAndDrop.Test do
  use ExUnit.Case, async: true

  alias Cloak.Aql.{Expression, Range}
  alias Cloak.Query.ShrinkAndDrop

  test "when the stream is empty the result is empty" do
    data = []
    query = %{ranges: [Range.new(%Expression{type: :integer, row_index: 1}, {0, 20}, :having)]}

    assert ShrinkAndDrop.apply(data, query) |> Enum.into([]) == []
  end

  test "when all values are the same nothing is dropped" do
    data = [["user1", 10], ["user2", 10], ["user3", 10]]
    query = %{ranges: [Range.new(%Expression{type: :integer, row_index: 1}, {0, 20}, :having)]}

    assert ShrinkAndDrop.apply(data, query) |> Enum.sort() == Enum.sort(data)
  end

  test "a basic scenario" do
    data = [["user1", 10], ["user1", 15], ["user2", 20], ["user2", 25], ["user3", 30]]
    query = %{ranges: [Range.new(%Expression{type: :integer, row_index: 1}, {0, 50}, :having)]}

    assert ShrinkAndDrop.apply(data, query) |> Enum.sort() ==
      [["user1", 10], ["user1", 15], ["user2", 20], ["user2", 25]]
  end
end
