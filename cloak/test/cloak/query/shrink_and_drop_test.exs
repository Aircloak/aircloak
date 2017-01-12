defmodule Cloak.Query.ShrinkAndDrop.Test do
  use ExUnit.Case, async: true
  use ExCheck

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

  property "order-independence" do
    for_all data in data_stream(0, 100) do
      query = %{ranges: [Range.new(%Expression{type: :integer, row_index: 1}, {0, 100}, :having)]}

      :rand.seed(:exsplus, {1, 2, 3})
      (data |> ShrinkAndDrop.apply(query) |> Enum.into(MapSet.new())) ==
        (data |> Enum.shuffle() |> ShrinkAndDrop.apply(query) |> Enum.into(MapSet.new()))
    end
  end

  defp data_stream(low, high) do
    domain(
      :data_stream,
      _generate = fn(domain, size) ->
        user_size = size |> :math.sqrt() |> round()
        {
          domain,
          for _ <- 1..size do
            [draw(pos_integer(), user_size), draw(int(low, high), size)]
          end
        }
      end,
      _shrink = fn(domain, item) -> {domain, item} end
    )
  end

  defp draw(domain, size) do
    {_, result} = pick(domain, size)
    result
  end
end
