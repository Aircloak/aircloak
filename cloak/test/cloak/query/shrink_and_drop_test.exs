defmodule Cloak.Query.ShrinkAndDrop.Test do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Column
  alias Cloak.Query.ShrinkAndDrop
  alias Cloak.Query.ShrinkAndDrop.HalfBuffer

  test "when all values are the same nothing is dropped" do
    data = [["user1", 10], ["user2", 10], ["user3", 10]]
    query = %{ranges: %{%Column{db_row_position: 1} => [0, 20]}}

    assert ShrinkAndDrop.apply(data, query) |> Enum.sort() == Enum.sort(data)
  end

  test "a basic scenario" do
    data = [["user1", 10], ["user1", 15], ["user2", 20], ["user2", 25], ["user3", 30]]
    query = %{ranges: %{%Column{db_row_position: 1} => [0, 50]}}

    assert ShrinkAndDrop.apply(data, query) |> Enum.sort() == Enum.sort(data)
  end

  describe "HalfBuffer" do
    test "adding to an empty buffer" do
      assert {%HalfBuffer{size: 3, min: 10, max: 10, users: %{"user1" => %{value: 10, rows: [:some_data]}}}, []} =
        HalfBuffer.add(HalfBuffer.new(3, &Kernel.</2), 10, "user1", :some_data)
    end

    test "adding when the user already in the buffer" do
      buffer = %HalfBuffer{size: 3, comparator: &Kernel.>/2, min: 10, max: 20, users: %{
        "user1" => %{value: 20, rows: [:some_data]}
      }}
      assert {%HalfBuffer{min: 10, max: 25, users: %{"user1" => %{value: 25, rows: [:more_data, :some_data]}}}, []} =
        HalfBuffer.add(buffer, 25, "user1", :more_data)
    end

    test "adding when the user is not in the buffer" do
      buffer = %HalfBuffer{size: 3, min: 10, max: 20, users: %{"user1" => :some_data}}
      assert HalfBuffer.add(buffer, 5, "user2", :more_data) ==
        {%HalfBuffer{size: 3, min: 5, max: 20, users: %{
          "user1" => :some_data,
          "user2" => %{value: 5, rows: [:more_data]}}}, []}
    end

    test "adding to a full buffer" do
      buffer = %HalfBuffer{size: 3, comparator: &Kernel.</2, min: 1, max: 3, users: %{
        "user1" => %{value: 1, rows: [:some_data]},
        "user2" => %{value: 2, rows: [:other_data]},
        "user3" => %{value: 3, rows: [:more_data]},
      }}

      assert {
        %HalfBuffer{size: 3, min: 0, max: 2, users: %{
          "user4" => %{value: 0, rows: [:even_more_data]},
          "user1" => %{value: 1, rows: [:some_data]},
          "user2" => %{value: 2, rows: [:other_data]},
        }},
        [:more_data]
      } = HalfBuffer.add(buffer, 0, "user4", :even_more_data)
    end

    test "dropping outside elements" do
      buffer = %HalfBuffer{comparator: &Kernel.</2, users: %{
        "user1" => %{value: 1, rows: [:some_data]},
        "user2" => %{value: 2, rows: [:other_data]},
        "user3" => %{value: 3, rows: [:more_data]},
        "user4" => %{value: 4, rows: [:some_data]},
      }}

      assert HalfBuffer.value_dropping(buffer, 2) == 3
    end
  end
end
