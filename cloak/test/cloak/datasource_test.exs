defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: false

  alias Cloak.Aql.Query
  alias Cloak.DataSource

  setup do
    :ok = Cloak.Test.DB.setup()
    {:ok, _} = Cloak.Test.DB.create_test_schema()
    :ok = Cloak.Test.DB.create_table("test", "value INTEGER")
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
    id_column = %Cloak.Aql.Column{table: %{db_name: "test", name: "test"}, name: "user_id"}
    data_column = %Cloak.Aql.Column{table: %{db_name: "test", name: "test"}, name: "value"}
    assert {:ok, rows} = DataSource.select(%Query{
      command: :select,
      columns: [data_column],
      db_columns: [id_column, data_column],
      unsafe_filter_columns: [],
      where: [],
      group_by: [],
      data_source: local_data_source(),
      from: "test",
      selected_tables: [%{db_name: "cloak_test.test", name: "test"}]
    }, &Enum.to_list/1)

    assert [["user-id", 10], ["user-id", 20], ["user-id", 30]] == rows
  end

  defp local_data_source() do
    Cloak.DataSource.fetch!(:local)
  end
end
