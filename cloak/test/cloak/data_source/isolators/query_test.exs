defmodule Cloak.DataSource.Isolators.Query.Test do
  use ExUnit.Case, async: false

  alias Cloak.DataSource
  alias Cloak.DataSource.Isolators.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("isolators", "value INTEGER")
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("isolators")
  end

  test "a column with many users per value is not isolating" do
    :ok =
      Cloak.Test.DB.add_users_data("isolators", ["value"], [
        ["user1", 10],
        ["user2", 10],
        ["user3", 30],
        ["user4", 30]
      ])

    for data_source <- DataSource.all() do
      refute Query.isolates_users?(data_source, "isolators", "value")
    end
  end

  test "a column with one user per value is isolating" do
    :ok =
      Cloak.Test.DB.add_users_data("isolators", ["value"], [
        ["user1", 10],
        ["user2", 20],
        ["user3", 30],
        ["user4", 40]
      ])

    for data_source <- DataSource.all() do
      assert Query.isolates_users?(data_source, "isolators", "value")
    end
  end
end
