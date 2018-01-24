defmodule Cloak.Query.VirtualTableTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  import ExUnit.CaptureLog

  setup_all do
    :ok = Cloak.Test.DB.create_table("vtt_real", "ival INTEGER")
    :ok = insert_rows(_user_ids = 1..10, "vtt_real", ["ival"], [10])
    :ok = insert_rows(_user_ids = 1..10, "vtt_real", ["ival"], [20])
    :ok
  end

  defp create_virtual_table!(query) do
    :ok = Cloak.Test.DB.create_table("vtt_fake", nil, [skip_db_create: true, query: query])
    on_exit(fn -> Cloak.Test.DB.delete_table("vtt_fake") end)
    :ok
  end

  defp virtual_table_error(query), do:
    capture_log(fn -> create_virtual_table!(query) end)

  test "error on query parsing" do
    assert virtual_table_error("_select * from vtt_real") =~
      ~r/Failed to parse the query for virtual table `vtt_fake`/
  end

  test "error on query compilation" do
    assert virtual_table_error("select x from cloak_test.vtt_real") =~
      ~r/Failed to compile the query for virtual table `vtt_fake`/
  end

  test "error on invalid column name" do
    assert virtual_table_error("select user_id, ival * 2 from cloak_test.vtt_real") =~
      ~r/Invalid column name `\*` in virtual table `vtt_fake`/
  end

  test "error on invalid query" do
    assert virtual_table_error("select user_id, xxx(ival) as x from cloak_test.vtt_real") =~
      ~r/Unknown function `xxx`/
  end

  test "duplicated columns are dropped" do
    create_virtual_table!("select user_id, * from cloak_test.vtt_real")
    assert_query "show columns from  vtt_fake", %{rows: [
      %{row: ["user_id", "text"]},
      %{row: ["ival", "integer"]},
    ]}
  end

  test "virtual columns" do
    create_virtual_table!("select user_id, ival * 2 as x from cloak_test.vtt_real")
    assert_query "select sum(x) from vtt_fake", %{rows: [%{row: [600]}]}
  end

  test "filtered virtual table" do
    create_virtual_table!("select * from cloak_test.vtt_real where ival = 10")
    assert_query "select sum(ival) from vtt_fake", %{rows: [%{row: [100]}]}
  end

  test "virtual table with subquery" do
    create_virtual_table!("select * from (select user_id, ival from cloak_test.vtt_real) as t")
    assert_query "select sum(ival) from vtt_fake", %{rows: [%{row: [300]}]}
  end

  test "virtual table from joined tables" do
    create_virtual_table!("""
      select t1.user_id, t1.ival as i1, t2.ival as i2
      from cloak_test.vtt_real as t1 join cloak_test.vtt_real as t2
      on t1.user_id = t2.user_id and t1.ival <> t2.ival
    """)
    assert_query "select sum(i1+i2) from vtt_fake", %{rows: [%{row: [600]}]}
  end
end
