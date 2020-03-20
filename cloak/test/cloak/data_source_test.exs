defmodule Cloak.DataSourceTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource

  import ExUnit.CaptureLog

  setup_all do
    :ok =
      Cloak.Test.DB.create_table("test", "value INTEGER, ignored_column INTEGER",
        blacklisted_columns: ["ignored_column"]
      )

    :ok =
      Cloak.Test.DB.insert_data("test", ["user_id", "value", "ignored_column"], [
        ["user1", 10, 100],
        ["user1", 20, 200],
        ["user1", 30, 300]
      ])

    :ok
  end

  test "schema discovery" do
    for data_source <- DataSource.all() do
      assert(data_source.tables[:test] != nil)

      assert(
        DataSource.table(data_source, :test).columns ==
          [
            %{name: "user_id", type: :text, visible?: true},
            %{name: "value", type: :integer, visible?: true}
          ]
      )
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

  test "public. is not allowed as a table name" do
    some_table = DataSource.Table.new("some_table", "user_id", db_name: "table")

    for data_source <- DataSource.all() do
      data_source = %{data_source | initial_tables: %{"public.some_table": some_table}}
      log = capture_log(fn -> assert %{tables: %{}} = DataSource.add_tables(data_source) end)
      assert log =~ "Load error for table `public.some_table`: table name can't start with `public.`."
    end
  end
end
