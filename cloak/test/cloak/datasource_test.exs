defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource
  alias Cloak.DataSource.Row

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
    assert(DataSource.columns(local_data_source(), :test) == [{"value", :integer}])
  end

  test "data retrieval" do
    assert {:ok, rows} = DataSource.select(local_data_source(), %{
      command: :select,
      columns: ["value"],
      unsafe_filter_columns: [],
      from: "test"
    })

    assert 3 == length(rows)
    assert Enum.all?(rows, &(&1.columns == ["user_id", "value"]))
    assert [["user-id", 10], ["user-id", 20], ["user-id", 30]] == Enum.map(rows, &Row.values/1)
  end

  defp local_data_source() do
    Cloak.DataSource.fetch!(:local)
  end
end
