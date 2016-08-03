defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: false

  alias Cloak.SqlQuery
  alias Cloak.DataSource

  setup do
    :ok = Cloak.Test.DB.setup()
    {:ok, _} = Cloak.Test.DB.create_test_schema()
    {:ok, _} = Cloak.Test.DB.create_table("test", "value INTEGER")
    data = [{"test", [
      {:columns, ["value"]},
      {:data, [[10], [20], [30]]}
    ]}]
    :ok = Cloak.Test.DB.add_users_data([{"user-id", data}])
    :ok
  end

  test "schema discovery" do
    assert(DataSource.tables(local_data_source()) == [:test])
    assert(DataSource.table(local_data_source(), :test).columns == [{"user_id", :text}, {"value", :integer}])
  end

  test "data retrieval" do
    column = %Cloak.SqlQuery.Column{table: %{db_name: "test", name: "test"}, name: "value"}
    assert {:ok, rows} = DataSource.select(%SqlQuery{
      command: :select,
      columns: [column],
      db_columns: [column],
      unsafe_filter_columns: [],
      where: [],
      group_by: [],
      data_source: local_data_source(),
      from: "test",
      selected_tables: [%{db_name: "cloak_test.test"}]
    })

    assert [[10], [20], [30]] == rows
  end

  defp local_data_source() do
    Cloak.DataSource.fetch!(:local)
  end
end
