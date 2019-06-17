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
      Cloak.Test.DB.create_table("public bounds", "id INTEGER, value INTEGER, string TEXT",
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

    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, "bounds", "value") == {5, 100}
    end
  end

  test "returns unknown if there are not enough users" do
    :ok =
      Cloak.Test.DB.insert_data("bounds", ["user_id", "value"], [
        ["user1", 10],
        ["user2", 10],
        ["user3", 30],
        ["user4", 30]
      ])

    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, "bounds", "value") == :unknown
    end
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

    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, "bounds", "string") == :unknown
    end
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

    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, "bounds with spaces", "val ue") == {5, 100}
    end
  end

  test "computes the true min and max (extended) for public tables" do
    :ok =
      Cloak.Test.DB.insert_data("public bounds", ["id", "value"], [
        [1, 20],
        [2, 30]
      ])

    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, "public bounds", "value") == {2, 300}
    end
  end

  test "public table with no data" do
    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, "public bounds", "value") == :unknown
    end
  end

  test "public table with non-numeric column" do
    :ok =
      Cloak.Test.DB.insert_data("public bounds", ["id", "string"], [
        [1, "a"],
        [2, "b"]
      ])

    for data_source <- DataSource.all() do
      assert Query.bounds(data_source, "public bounds", "string") == :unknown
    end
  end
end
