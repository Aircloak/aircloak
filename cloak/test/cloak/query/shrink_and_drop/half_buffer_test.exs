defmodule Cloak.Query.ShrinkAndDrop.HalfBuffer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.ShrinkAndDrop.HalfBuffer

  describe "working with numbers" do
    test "adding to an empty buffer" do
      assert {%HalfBuffer{size: 3, min: 10, max: 10, users: %{"user1" => %{value: 10, rows: [:some_data]}}}, []} =
        HalfBuffer.add(HalfBuffer.new(3, &Kernel.</2), 10, "user1", :some_data)
    end

    test "adding when the user already in the buffer" do
      buffer = %HalfBuffer{size: 3, comparator: &Kernel.>/2, min: 10, max: 20, users: %{
        "user1" => %{value: 20, rows: [:some_data]}
      }}
      assert {%HalfBuffer{users: %{"user1" => %{value: 25, rows: [:more_data, :some_data]}}}, []} =
        HalfBuffer.add(buffer, 25, "user1", :more_data)
    end

    test "adding when the user is not in the buffer" do
      buffer = %HalfBuffer{size: 3, min: 10, max: 20, users: %{"user1" => :some_data}}
      assert {%HalfBuffer{size: 3, users: %{"user1" => :some_data, "user2" => %{value: 5, rows: [:more_data]}}}, []}
        = HalfBuffer.add(buffer, 5, "user2", :more_data)
    end

    test "updating min" do
      buffer = %HalfBuffer{size: 3, min: 10, max: 20, users: %{}}
      assert {%HalfBuffer{min: 5}, []} = HalfBuffer.add(buffer, 5, "user2", :more_data)
    end

    test "updating max" do
      buffer = %HalfBuffer{size: 3, min: 10, max: 20, users: %{}}
      assert {%HalfBuffer{max: 25}, []} = HalfBuffer.add(buffer, 25, "user2", :more_data)
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

      assert HalfBuffer.values_except_extreme(buffer, 2) == 3
    end

    test "inside" do
      buffer = %HalfBuffer{comparator: &Kernel.</2, users: %{
        "user1" => %{value: 1, rows: [{nil, nil, 4, nil}]},
        "user2" => %{value: 2, rows: [{nil, nil, 4, nil}, {nil, nil, 5, nil}]},
        "user3" => %{value: 3, rows: [{nil, nil, 1, nil}]},
        "user4" => %{value: 4, rows: [{nil, nil, 2, nil}, {nil, nil, 3, nil}]},
      }}

      assert HalfBuffer.inside(buffer, {3, 5}) |> Enum.sort() ==
        [{nil, nil, 3, nil}, {nil, nil, 4, nil}, {nil, nil, 4, nil}]
    end
  end

  describe "working with dates" do
    test "adding to an empty buffer" do
      assert {
        %HalfBuffer{
          size: 3,
          min: ~D[2016-01-01],
          max: ~D[2016-01-01],
          users: %{"user1" => %{value: ~D[2016-01-01], rows: [:some_data]}}
        }, []} = HalfBuffer.add(HalfBuffer.new(3, &Cloak.Data.lt_eq/2), ~D[2016-01-01], "user1", :some_data)
    end

    test "updating min" do
      buffer = %HalfBuffer{size: 3, min: ~D[2016-01-01], max: ~D[2016-01-01], users: %{}}
      assert {%HalfBuffer{min: ~D[2015-06-10]}, []} = HalfBuffer.add(buffer, ~D[2015-06-10], "user2", :more_data)
    end

    test "updating max" do
      buffer = %HalfBuffer{size: 3, min: ~D[2016-01-01], max: ~D[2016-06-01], users: %{}}
      assert {%HalfBuffer{max: ~D[2017-01-01]}, []} = HalfBuffer.add(buffer, ~D[2017-01-01], "user2", :more_data)
    end

    test "dropping outside elements" do
      buffer = %HalfBuffer{comparator: &Cloak.Data.lt_eq/2, users: %{
        "user1" => %{value: ~D[2016-01-01], rows: [:some_data]},
        "user2" => %{value: ~D[2016-06-01], rows: [:other_data]},
        "user3" => %{value: ~D[2017-01-01], rows: [:more_data]},
        "user4" => %{value: ~D[2017-05-01], rows: [:some_data]},
      }}

      assert HalfBuffer.values_except_extreme(buffer, 2) == ~D[2017-01-01]
    end

    test "inside" do
      buffer = %HalfBuffer{comparator: &Cloak.Data.lt_eq/2, users: %{
        "user1" => %{value: ~D[2016-01-01], rows: [{nil, nil, ~D[2016-01-01], nil}]},
        "user2" => %{value: ~D[2016-02-01], rows: [{nil, nil, ~D[2016-02-01], nil}, {nil, nil, ~D[2017-01-01], nil}]},
        "user3" => %{value: ~D[2016-03-01], rows: [{nil, nil, ~D[2016-03-01], nil}]},
        "user4" => %{value: ~D[2016-04-01], rows: [{nil, nil, ~D[2016-04-01], nil}, {nil, nil, ~D[2016-03-02], nil}]},
      }}

      assert HalfBuffer.inside(buffer, {~D[2016-02-01], ~D[2016-04-01]}) |> Enum.sort() ==
        [{nil, nil, ~D[2016-02-01], nil}, {nil, nil, ~D[2016-03-01], nil}, {nil, nil, ~D[2016-03-02], nil}]
    end
  end
end
