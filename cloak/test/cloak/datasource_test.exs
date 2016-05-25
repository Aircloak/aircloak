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
    [source_name, table_name] = String.split(:db_test.table_path("test"), "/")
    table_id = String.to_existing_atom(table_name)
    source_id = String.to_existing_atom(source_name)
    assert(Cloak.DataSource.tables(source_id) == [table_id])
    assert(Cloak.DataSource.columns(source_id, table_id) == [{"value", :integer}])
  end

  test "data retrieval" do
    [source_name, table_name] = String.split(:db_test.table_path("test"), "/")
    source_id = String.to_existing_atom(source_name)
    data = Cloak.DataSource.query(source_id, "SELECT value FROM #{table_name}")
    assert(data == {:ok, {
      3,
      ["user_id", "value"],
      [["user-id", "10"], ["user-id", "20"], ["user-id", "30"]]
    }})
  end
end
