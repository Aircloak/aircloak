defmodule Cloak.QueryTest do
  use ExUnit.Case, async: false

  alias Cloak.Query

  setup_all do
    :db_test.setup()
    :db_test.create_test_schema()
    :db_test.create_table("heights", "height INTEGER")

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

  test "query execution" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = start_query("select height from heights")

    assert_receive {:reply, %{query_id: "1", columns: ["height"], rows: rows}}
    assert 100 == length(rows)
    assert Enum.all?(rows, &(&1 == ["180"]))
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

    assert groups == [{["*"], 20}, {["180"], 20}]
  end

  test "query reports an error on invalid statement" do
    :ok = start_query("invalid statement")
    assert_receive {:reply, %{query_id: "1", error: "Expected `select` at line 1, column 1.," <>
      " or: Expected `show tables` at line 1, column 1., or: Expected `show columns` at line 1, column 1."}}
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
