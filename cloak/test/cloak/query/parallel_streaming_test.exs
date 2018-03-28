defmodule Cloak.Query.ParallelStreamingTest do
  use ExUnit.Case, async: true

  @table "parallel_streaming"

  import Cloak.Test.QueryHelpers, only: [insert_rows: 4]
  alias Cloak.Query.Runner

  setup_all do
    :ok = Cloak.Test.DB.create_table(@table, "value INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table(@table)
    :ok
  end

  defp data_source() do
    data_source = Enum.find(Cloak.DataSource.all(), &(&1.driver === Cloak.DataSource.PostgreSQL))
    %{data_source | concurrency: 3}
  end

  defmacrop assert_query(query, expected_response) do
    quote do
      result =
        Runner.run_sync(
          "#{:erlang.unique_integer([:positive])}",
          data_source(),
          unquote(query),
          [],
          %{}
        )

      assert unquote(expected_response) = result
    end
  end

  test "parallel data ingestion" do
    :ok = insert_rows(_user_ids = 1..700, @table, ["value"], [1])
    :ok = insert_rows(_user_ids = 1..1000, @table, ["value"], [2])
    :ok = insert_rows(_user_ids = 1..500, @table, ["value"], [-1])
    :ok = insert_rows(_user_ids = 1..800, @table, ["value"], [0])
    :ok = insert_rows(_user_ids = 1..1200, @table, ["value"], [-2])

    assert_query("SELECT COUNT(*), COUNT(DISTINCT value), MIN(value), MEDIAN(value), SUM(value) FROM #{@table}", %{
      rows: [%{row: [4200, 5, -2, 0, -200]}]
    })
  end
end
