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

  def setup do
    :db_test.clear_table("heights")
  end

  test "task execution" do
    assert :ok = insert_rows(100, "heights", ["height"], [180])

    assert :ok == start_task("select height from cloak_test.heights")
    assert_receive {:reply, %{task_id: "1", columns: ["height"], rows: rows}}
    assert 100 == length(rows)
    assert Enum.all?(rows, &(&1 == [180]))
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

  defp insert_rows(count, table, columns, values) do
    :db_test.add_users_data(
      Enum.map(1..count,
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
