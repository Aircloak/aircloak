defmodule Cloak.DataSource.Bounds.Query.Test do
  use ExUnit.Case, async: false

  alias Cloak.DataSource
  alias Cloak.DataSource.Bounds.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("bounds", "value INTEGER, string TEXT, pk INTEGER")

    :ok =
      Cloak.Test.DB.create_table(
        "bounds with spaces",
        "\"user id\" TEXT, \"val ue\" INTEGER",
        user_id: "user id"
      )

    :ok =
      Cloak.Test.DB.create_table("public bounds", "id INTEGER, value INTEGER, float REAL, string TEXT",
        user_id: nil,
        add_user_id: false,
        content_type: :public,
        keys: %{"id" => :product_id}
      )
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("bounds")
    :ok = Cloak.Test.DB.clear_table("bounds with spaces")
    :ok = Cloak.Test.DB.clear_table("public bounds")
  end

  test "computes min and max if there are > cutoff users" do
    :ok =
      Cloak.Test.DB.insert_data("bounds", ["user_id", "value"], [
        ["user1", 10],
        ["user2", 10],
        ["user3", 30],
        ["user4", 30],
        ["user5", 30],
        ["user6", 30]
      ])

    assert_bounds("bounds", "value", {1, 500})
  end

  test "ignores NULLs" do
    :ok =
      Cloak.Test.DB.insert_data("bounds", ["user_id", "value"], [
        ["user1", 10],
        ["user2", 10],
        ["user3", 30],
        ["user4", 30],
        ["user5", 30],
        ["user6", 30],
        ["user7", nil]
      ])

    assert_bounds("bounds", "value", {1, 500})
  end

  test "returns unknown if there are not enough users" do
    :ok =
      Cloak.Test.DB.insert_data("bounds", ["user_id", "value"], [
        ["user1", 10],
        ["user2", 10],
        ["user3", 30],
        ["user4", 30]
      ])

    assert_bounds("bounds", "value", :unknown)
  end

  test "user id bounds" do
    :ok =
      Cloak.Test.DB.create_table("bounds_user_id", "id INTEGER",
        user_id: "id",
        add_user_id: false
      )

    :ok = Cloak.Test.DB.insert_data("bounds_user_id", ["id"], [[10], [11], [12], [13], [20], [30]])

    assert_bounds("bounds_user_id", "id", {2, 100})
  end

  test "non-numeric columns have unknown bounds" do
    :ok =
      Cloak.Test.DB.insert_data("bounds", ["user_id", "string"], [
        ["user1", "a"],
        ["user2", "a"],
        ["user3", "b"],
        ["user4", "b"],
        ["user5", "b"],
        ["user6", "b"]
      ])

    assert_bounds("bounds", "string", :unknown)
  end

  test "names with spaces are handled properly" do
    :ok =
      Cloak.Test.DB.insert_data("bounds with spaces", ["\"user id\"", "\"val ue\""], [
        ["user1", 10],
        ["user2", 10],
        ["user3", 30],
        ["user4", 30],
        ["user5", 30],
        ["user6", 30]
      ])

    assert_bounds("bounds with spaces", "val ue", {1, 500})
  end

  test "computes the true min and max (extended) for public tables" do
    :ok =
      Cloak.Test.DB.insert_data("public bounds", ["id", "value"], [
        [1, 20],
        [2, 30]
      ])

    assert_bounds("public bounds", "value", {2, 300})
  end

  test "float column in public table" do
    :ok =
      Cloak.Test.DB.insert_data("public bounds", ["id", "float"], [
        [1, 21.11],
        [2, 30.33]
      ])

    assert_bounds("public bounds", "float", {2, 300})
  end

  test "public table with no data" do
    assert_bounds("public bounds", "value", :unknown)
  end

  test "public table with non-numeric column" do
    :ok =
      Cloak.Test.DB.insert_data("public bounds", ["id", "string"], [
        [1, "a"],
        [2, "b"]
      ])

    assert_bounds("public bounds", "string", :unknown)
  end

  test "ignores NULLs in public tables" do
    :ok =
      Cloak.Test.DB.insert_data("public bounds", ["id", "value"], [
        [1, 20],
        [2, nil]
      ])

    assert_bounds("public bounds", "value", {2, 200})
  end

  def assert_bounds(table, column, bounds) do
    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, table, column) == bounds
    end
  end
end
