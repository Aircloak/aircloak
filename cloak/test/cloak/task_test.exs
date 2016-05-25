defmodule Cloak.TaskTest do
  use ExUnit.Case, async: false

  alias Cloak.Task

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

  test "task execution" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = start_task("select height from cloak_test.heights")

    assert_receive {:reply, %{task_id: "1", columns: ["height"], rows: rows}}
    assert 100 == length(rows)
    assert Enum.all?(rows, &(&1 == [180]))
  end

  test "should return LCF property when sufficient rows are filtered" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    for id <- 5..9 do
      range = (id * 4)..(id * 4 + 3)
      assert :ok = insert_rows(_user_ids = range, "heights", ["height"], [100 + id])
    end

    :ok = start_task("select height from cloak_test.heights")

    assert_receive {:reply, %{task_id: "1", columns: ["height"], rows: rows}}
    groups = rows
    |> Enum.group_by(&(&1))
    |> Enum.map(fn({k, values}) -> {k, Enum.count(values)} end)

    assert groups == [{[180], 20}, {["*"], 20}]
  end

  test "task reports an error on invalid statement" do
    :ok = start_task("invalid statement")
    assert_receive {:reply, %{task_id: "1", error: _}}
  end

  test "task reports an error on invalid data source" do
    :ok = start_task("select invalid_column from non_existent_source")
    assert_receive {:reply, %{task_id: "1", error: _}}
  end

  test "task reports an error on runner crash" do
    ExUnit.CaptureLog.capture_log(fn ->
      :ok = start_task(:invalid_query_type)
      assert_receive {:reply, %{task_id: "1", error: "Cloak error"}}
    end)
  end

  defp start_task(query) do
    Task.start(%Task{id: "1", query: query}, {:process, self()})
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
