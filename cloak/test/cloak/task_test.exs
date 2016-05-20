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

    assert :ok == Task.run(
      %Task{
        id: "1",
        query: "select height from cloak_test.heights"
      },
      {:process, self()}
    )

    assert_receive {:reply, %{task_id: "1", columns: ["height"], rows: rows}}
    assert 100 == length(rows)
    assert Enum.all?(rows, &(&1 == 180))
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
