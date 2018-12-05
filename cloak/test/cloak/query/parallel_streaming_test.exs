defmodule Cloak.Query.ParallelStreamingTest do
  use ExUnit.Case, async: false

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

  defp data_source(concurrency) do
    data_source = Enum.find(Cloak.DataSource.all(), &(&1.driver === Cloak.DataSource.PostgreSQL))
    %{data_source | concurrency: concurrency}
  end

  defmacrop assert_query(query, expected_response, opts) do
    quote do
      result =
        Runner.run_sync(
          "#{:erlang.unique_integer([:positive])}",
          data_source(Keyword.get(unquote(opts), :concurrency, 3)),
          unquote(query),
          [],
          %{}
        )

      assert unquote(expected_response) = result
    end
  end

  test "parallel data ingestion for uid anonymization" do
    :ok = insert_rows(_user_ids = 1..700, @table, ["value"], [1])
    :ok = insert_rows(_user_ids = 1..1000, @table, ["value"], [2])
    :ok = insert_rows(_user_ids = 1..500, @table, ["value"], [-1])
    :ok = insert_rows(_user_ids = 1..800, @table, ["value"], [0])
    :ok = insert_rows(_user_ids = 1..1200, @table, ["value"], [-2])

    for concurrency <- 2..5 do
      assert_query(
        "SELECT COUNT(*), COUNT(DISTINCT value), MIN(value), MEDIAN(value), SUM(value) FROM #{@table}",
        %{rows: [%{row: [4200, 5, -2, 0, -200]}]},
        concurrency: concurrency
      )
    end
  end

  test "parallel data ingestion for no-uid anonymization" do
    for i <- 1..1000 do
      :ok = insert_rows(_user_ids = 1..10, @table, ["value"], [i])
    end

    for concurrency <- 2..5 do
      assert_query(
        "SELECT BUCKET(value BY 500), COUNT(*) FROM #{@table} GROUP BY 1 ORDER BY 1",
        %{rows: [%{row: [0.0, 5986]}, %{row: [500.0, 5998]}, %{row: [1000.0, 10]}]},
        concurrency: concurrency
      )
    end
  end

  test "error during parallel ingestion" do
    :ok = Cloak.Test.DB.create_table("invalid_table", "value INTEGER")

    try do
      Cloak.Test.DB.execute!("DROP TABLE cloak_test.invalid_table")

      for i <- 1..1000 do
        :ok = insert_rows(_user_ids = 1..10, @table, ["value"], [i])
      end

      for concurrency <- 2..5 do
        assert_query(
          "SELECT BUCKET(value BY 500), COUNT(*) FROM invalid_table GROUP BY 1 ORDER BY 1",
          %{error: error},
          concurrency: concurrency
        )

        assert error =~ ~r/relation "cloak_test.invalid_table" does not exist/
      end
    after
      Cloak.Test.DB.delete_table("invalid_table")
    end
  end
end
