defmodule Cloak.TaskTest do
  use ExUnit.Case, async: true

  alias Cloak.Task

  setup do
    :db_test.setup()
    :db_test.create_test_schema()
    :db_test.create_table("heights", "height INTEGER")
    :ok
  end

  test "task execution" do
    Task.run(
      %Task{
        id: "1",
        query: "select user_id, height from cloak_test.heights"
      },
      {:process, self()}
    )

    assert_receive {:reply, %{task_id: "1", columns: ["height"], rows: []}}
  end
end
