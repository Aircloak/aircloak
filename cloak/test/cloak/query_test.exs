defmodule Cloak.QueryTest do
  use ExUnit.Case, async: false

  alias Cloak.Query

  setup_all do
    :db_test.setup()
    :db_test.create_test_schema()
    :db_test.create_table("heights", "height INTEGER, name TEXT")

    :meck.new(:cloak_distributions)
    :meck.expect(:cloak_distributions, :gauss, fn(_sigma, n) -> round(n) end)
    :meck.expect(:cloak_distributions, :gauss_s, fn(_sigma, n, _seed) -> round(n) end)

    on_exit(fn -> :meck.unload() end)

    :ok
  end

  setup do
    :db_test.clear_table("heights")
    :ok
  end

  test "show tables" do
    :ok = start_query("show tables")

    assert_receive {:reply, %{query_id: "1", columns: ["name"], rows: [[:heights]]}}
  end

  test "show columns" do
    :ok = start_query("show columns from heights")

    assert_receive {:reply, %{query_id: "1", columns: ["name", "type"], rows: rows}}
    assert Enum.sort(rows) == [["height", :integer], ["name", :text]]
  end

  test "query execution" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = start_query("select height from heights")

    assert_receive {:reply, %{query_id: "1", columns: ["height"], rows: rows}}
    assert 100 == length(rows)
    assert Enum.all?(rows, &(&1 == [180]))
  end

  test "should return LCF property when sufficient rows are filtered" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    for id <- 5..9 do
      range = (id * 4)..(id * 4 + 3)
      assert :ok = insert_rows(_user_ids = range, "heights", ["height"], [100 + id])
    end

    :ok = start_query("select height from heights")

    assert_receive {:reply, %{query_id: "1", columns: ["height"], rows: rows}}
    groups = rows
    |> Enum.group_by(&(&1))
    |> Enum.map(fn({k, values}) -> {k, Enum.count(values)} end)
    |> Enum.sort()

    assert groups == [{[180], 20}, {["*"], 20}]
  end

  test "should produce counts" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])

    :ok = start_query("select count(*) from heights")

    assert_receive {:reply, %{query_id: "1", columns: ["count(*)"], rows: [[20]]}}
  end

  test "should allow ranges for where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])
    :ok = start_query("select count(*) from heights where height > 170 and height < 190")
    assert_receive {:reply, %{query_id: "1", columns: ["count(*)"], rows: [[20]]}}
  end

  test "should allow LIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "bob"])
    :ok = start_query("select count(*) from heights where name LIKE 'b%'")
    assert_receive {:reply, %{query_id: "1", columns: ["count(*)"], rows: [[20]]}}
  end

  test "should allow IN in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])
    :ok = start_query("select count(*) from heights where height IN (170, 180, 190)")
    assert_receive {:reply, %{query_id: "1", columns: ["count(*)"], rows: [[60]]}}
  end

  test "should order rows when instructed" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = start_query("select height from heights order by height")
    assert_receive {:reply, %{query_id: "1", columns: ["height"], rows: rows}}
    assert rows == Enum.sort(rows)
  end

  test "query reports an error on invalid statement" do
    :ok = start_query("invalid statement")
    assert_receive {:reply, %{query_id: "1", error: "Expected `select or show` at line 1, column 1."}}
  end

  test "query reports an error on invalid column" do
    :ok = start_query("select invalid_column from heights")
    assert_receive {:reply, %{query_id: "1", error: error}}
    assert ~s/Column "invalid_column" doesn't exist./ == error
  end

  test "query reports an error on invalid table" do
    :ok = start_query("select column from invalid_table")
    assert_receive {:reply, %{query_id: "1", error: error}}
    assert ~s/Table "invalid_table" doesn't exist./ == error
  end

  test "query reports an error when mixing aggregated and normal columns" do
    :ok = start_query("select count(*), height from heights")

    assert_receive {:reply, %{query_id: "1", error: error}}
    assert error =~ ~r/height need to appear in the `group by` clause/
  end

  test "query reports an error when grouping by nonexistent columns" do
    :ok = start_query("select count(*) from heights group by nothing")

    assert_receive {:reply, %{query_id: "1", error: error}}
    assert error =~ ~r/Column "nothing" doesn't exist./
  end

  test "query reports an error when not grouping by some selected columns" do
    :ok = start_query("select name, height from heights group by height")

    assert_receive {:reply, %{query_id: "1", error: error}}
    assert error =~ ~r/name need to appear in the `group by` clause/
  end

  test "query allows mixing aggregated and grouped columns" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    :ok = start_query("select count(*), height from heights group by height")

    assert_receive {:reply, %{columns: ["count(*)", "height"], rows: rows}}
    assert Enum.sort(rows) == [[10, 180], [20, 160]]
  end

  test "grouping works when the column is not selected" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    :ok = start_query("select count(*) from heights group by height")

    assert_receive {:reply, %{columns: ["count(*)"], rows: rows}}
    assert Enum.sort(rows) == [[10], [20]]
  end

  test "grouping and sorting by a count" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    :ok = start_query("select count(*), height from heights group by height order by count(*) asc")

    assert_receive {:reply, %{columns: ["count(*)", "height"], rows: rows}}
    assert rows == [[10, 180], [20, 160], [30, 150]]
  end

  test "query reports an error on invalid where clause identifier" do
    :ok = start_query("select height from heights where nonexistant > 10")
    assert_receive {:reply, %{query_id: "1", error: error}}
    assert ~s/Column "nonexistant" doesn't exist./ == error
  end

  test "query reports an error on invalid order by field" do
    :ok = start_query("select height from heights order by age")
    assert_receive {:reply, %{query_id: "1", error: error}}
    assert ~s/Non-selected field specified in 'order by' clause: "age"./ == error
  end

  test "query reports an error on runner crash" do
    ExUnit.CaptureLog.capture_log(fn ->
      :ok = start_query(:invalid_query_type)
      assert_receive {:reply, %{query_id: "1", error: "Cloak error"}}
    end)
  end

  defp start_query(statement) do
    Query.start(%Query{id: "1", statement: statement}, {:process, self()})
  end

  defp insert_rows(user_id_range, table, columns, values) do
    :db_test.add_users_data(
      Enum.map(user_id_range,
        &{"user#{&1}", [
          {table, [
            columns: columns,
            data: [values]
          ]}
        ]}
      )
    )
  end
end
