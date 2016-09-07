defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Query
  alias Cloak.DataSource

  setup_all do
    :ok = Cloak.Test.DB.create_table("test", "value INTEGER")
    :ok = Cloak.Test.DB.add_users_data("test", ["value"], [["user1", 10], ["user1", 20], ["user1", 30]])
    :ok
  end

  test "schema discovery" do
    for {_global_id, data_source} <- DataSource.all() do
      assert(Enum.member?(DataSource.tables(data_source), :test))
      assert(DataSource.table(data_source, :test).columns == [{"user_id", :text}, {"value", :integer}])
    end
  end

  test "data retrieval" do
    id_column = %Cloak.Aql.Column{table: %{db_name: "test", name: "test"}, name: "user_id"}
    data_column = %Cloak.Aql.Column{table: %{db_name: "test", name: "test"}, name: "value"}
    query = %Query{
      command: :select,
      columns: [data_column],
      db_columns: [id_column, data_column],
      unsafe_filter_columns: [],
      where: [],
      group_by: [],
      data_source: nil,
      from: "test",
      selected_tables: [%{db_name: "cloak_test.test", name: "test"}]
    }
    for {_global_id, data_source} <- DataSource.all() do
      assert {:ok, rows} = DataSource.select(%{query | data_source: data_source}, &Enum.to_list/1)
      assert [["user1", 10], ["user1", 20], ["user1", 30]] == rows
    end
  end
end
