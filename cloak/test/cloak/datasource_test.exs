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
    column = %Cloak.SqlQuery.Column{table: %{name: "test", user_name: "test"}, name: "value"}
    assert {:ok, columns, rows} = DataSource.select(%{
      command: :select,
      columns: [column],
      db_columns: [column],
      unsafe_filter_columns: [],
      where: [],
      group_by: [],
      data_source: local_data_source(),
      from: "test",
      selected_tables: [%{name: "cloak_test.test"}]
    })

    assert [[10], [20], [30]] == rows
    assert 10 == DataSource.fetch_value!(hd(rows), columns, column)
  end

  defp local_data_source() do
    Cloak.DataSource.fetch!(:local)
  end
end
