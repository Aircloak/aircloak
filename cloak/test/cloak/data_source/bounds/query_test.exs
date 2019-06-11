defmodule Cloak.DataSource.Bounds.Query.Test do
  use ExUnit.Case, async: false

  alias Cloak.DataSource
  alias Cloak.DataSource.Bounds.Query

  setup_all do
    :ok = Cloak.Test.DB.create_table("bounds", "value INTEGER, pk INTEGER")

    :ok =
      Cloak.Test.DB.create_table(
        "bounds with spaces",
        "\"user id\" INTEGER, \"val ue\" INTEGER",
        user_id: "user id"
      )
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("bounds")
    :ok = Cloak.Test.DB.clear_table("bounds with spaces")

    anonymizer_config = Application.get_env(:cloak, :anonymizer)
    Application.put_env(:cloak, :anonymizer, anonymizer_config |> Keyword.put(:bound_size_cutoff, 5))
    on_exit(fn -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)
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

  test "non-numeric columns have unknown bounds"

  test "names with spaces are handled properly"
end
