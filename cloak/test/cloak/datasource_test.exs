defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: false

  setup do
    :ok = :db_test.setup()
    {:ok, _} = :db_test.create_test_schema()
    {:ok, _} = :db_test.create_table("test", "value INTEGER")
    data = [{"test", [
      {:columns, ["value"]},
      {:data, [[10], [20], [30]]}
    ]}]
    :ok = :db_test.add_users_data([{"user-id", data}])
    :ok
  end

  test "schema discovery" do
    assert(Cloak.DataSource.tables(:local) == [:test])
    assert(Cloak.DataSource.columns(:local, :test) == [{"value", :integer}])
  end

  test "data retrieval" do
    data = Cloak.DataSource.select(:local, %{statement: :select, columns: ["value"], from: "test"})
    assert(data == {:ok, {
      3,
      ["user_id", "value"],
      [["user-id", "10"], ["user-id", "20"], ["user-id", "30"]]
    }})
  end
end
