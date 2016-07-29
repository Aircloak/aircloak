defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource

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
    assert(DataSource.tables(local_data_source()) == [:test])
    assert(DataSource.table(local_data_source(), :test).columns == [{"user_id", :text}, {"value", :integer}])
  end

  test "data retrieval" do
    assert {:ok, data} = DataSource.select(%{
      command: :select,
      columns: [%Cloak.SqlQuery.Column{table: %{name: "test", user_name: "test"}, name: "value"}],
      unsafe_filter_columns: [],
      where: [],
      group_by: [],
      data_source: local_data_source(),
      from: "test",
      selected_tables: [%{name: "cloak_test.test"}]
    })

    assert(data == {
      3,
      ["test.user_id", "test.value"],
      [["user-id", 10], ["user-id", 20], ["user-id", 30]]
    })
  end

  defp local_data_source() do
    Cloak.DataSource.fetch!(:local)
  end
end
