defmodule Cloak.DataSource.Isolators.Query.Test do
  use ExUnit.Case, async: false

  alias Cloak.DataSource
  alias Cloak.DataSource.Isolators.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("isolators", "value INTEGER, pk INTEGER")

    :ok =
      Cloak.Test.DB.create_table(
        "isolators_projected",
        "value INTEGER, fk INTEGER",
        add_user_id: false,
        projection: %{
          table: "isolators",
          primary_key: "pk",
          foreign_key: "fk",
          user_id_alias: "uid"
        }
      )

    :ok =
      Cloak.Test.DB.create_table(
        "isolators with spaces",
        "\"user id\" INTEGER, \"val ue\" INTEGER",
        user_id: "user id"
      )
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("isolators")
    :ok = Cloak.Test.DB.clear_table("isolators with spaces")

    anonymizer_config = Application.get_env(:cloak, :anonymizer)
    Application.put_env(:cloak, :anonymizer, anonymizer_config |> Keyword.put(:isolating_column_threshold, 0.5))
    on_exit(fn -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)
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

  test "[BUG] a column with many rows per user" do
    :ok =
      Cloak.Test.DB.add_users_data("isolators", ["value"], [
        ["user1", 10],
        ["user1", 10],
        ["user2", 20],
        ["user2", 20],
        ["user3", 30],
        ["user3", 30]
      ])

    for data_source <- DataSource.all() do
      assert Query.isolates_users?(data_source, "isolators", "value")
    end
  end

  test "[BUG] null user ids do not produce crashes" do
    :ok =
      Cloak.Test.DB.add_users_data("isolators", ["value"], [
        ["user1", 10],
        ["user2", 10],
        ["user2", 20],
        ["user2", 30],
        ["user3", 30],
        [nil, 40]
      ])

    for data_source <- DataSource.all() do
      assert Query.isolates_users?(data_source, "isolators", "value") != nil
    end
  end

  test "a user id column is isolating" do
    for data_source <- DataSource.all() do
      assert Query.isolates_users?(data_source, "isolators", "user_id")
    end
  end

  test "projected table" do
    :ok =
      Cloak.Test.DB.add_users_data("isolators", ["pk"], [
        ["user1", 1],
        ["user2", 2],
        ["user3", 3],
        ["user4", 4]
      ])

    :ok =
      Cloak.Test.DB.insert_data("isolators_projected", ["fk", "value"], [
        [1, 10],
        [2, 20],
        [3, 30],
        [4, 40]
      ])

    for data_source <- DataSource.all() do
      assert Query.isolates_users?(data_source, "isolators_projected", "value")
    end
  end

  test "[BUG] names with spaces are handled properly" do
    :ok =
      Cloak.Test.DB.add_users_data("isolators with spaces", ["\"val ue\""], [
        ["user1", 10],
        ["user2", 10],
        ["user3", 30],
        ["user4", 30]
      ])

    for data_source <- DataSource.all() do
      refute Query.isolates_users?(data_source, "isolators with spaces", "val ue")
    end
  end
end
