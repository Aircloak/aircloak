defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Query
  alias Cloak.DataSource

  import ExUnit.CaptureLog

  setup_all do
    :ok = Cloak.Test.DB.create_table("test", "value INTEGER")
    :ok = Cloak.Test.DB.add_users_data("test", ["value"], [["user1", 10], ["user1", 20], ["user1", 30]])
    :ok
  end

  test "schema discovery" do
    for data_source <- DataSource.all() do
      assert(data_source.tables[:test] != nil)
      assert(DataSource.table(data_source, :test).columns ==
        [%{name: "user_id", type: :text, visible?: true}, %{name: "value", type: :integer, visible?: true}])
    end
  end

  test "data retrieval" do
    id_column = %Cloak.Sql.Expression{table: %{db_name: "test", name: "test"}, name: "user_id"}
    data_column = %Cloak.Sql.Expression{table: %{db_name: "test", name: "test"}, name: "value"}
    query = %Query{
      command: :select,
      columns: [data_column],
      db_columns: [id_column, data_column],
      where: nil,
      group_by: [],
      data_source: nil,
      from: "test",
      selected_tables: [%{db_name: "cloak_test.test", name: "test"}]
    }
    for data_source <- DataSource.all() do
      rows = DataSource.select!(%{query | data_source: data_source}, &Enum.concat/1)
      assert [["user1", 10], ["user1", 20], ["user1", 30]] == rows
    end
  end

  test "continue working when table is missing" do
    missing_table = DataSource.Table.new("missing_table", "user_id", db_name: "table")
    for data_source <- DataSource.all() do
      data_source = %{data_source | initial_tables: %{missing_table: missing_table}}
      log = capture_log(fn -> assert %{tables: %{}} = DataSource.add_tables(data_source) end)
      assert log =~ "Load error for table `missing_table`:"
    end
  end
end
