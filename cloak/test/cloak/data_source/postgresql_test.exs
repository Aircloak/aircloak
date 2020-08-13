defmodule Cloak.DataSource.PostgreSQL.Test do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("postgres_test", "age INTEGER")
  end

  test "[Issue #3841] treating an integer constant as a float" do
    :ok = insert_rows(_user_ids = 1..10, "postgres_test", ["age"], [70])

    assert_query("select age * 99999999999999999 * 2 / 1 from postgres_test", %{rows: [%{row: [nil], occurrences: 10}]})
  end
end
