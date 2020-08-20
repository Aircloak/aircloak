defmodule Cloak.Query.ParallelStreamingTest do
  use ExUnit.Case, async: false

  @table "parallel_streaming"

  import Cloak.Test.QueryHelpers, only: [insert_rows: 4, default_data_source: 0]
  alias Cloak.Query.Runner

  setup_all do
    data_sources = Cloak.DataSource.all()
    on_exit(fn -> Cloak.DataSource.replace_all_data_source_configs(data_sources) end)
    streaming_data_source = default_data_source() |> Map.put(:statistics_anonymization, false)
    Cloak.DataSource.replace_all_data_source_configs([streaming_data_source])

    :ok = Cloak.Test.DB.create_table(@table, "value INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table(@table)
    :ok
  end

  defp data_source(concurrency), do: %{default_data_source() | concurrency: concurrency}

  defmacrop assert_query(query, expected_response, opts) do
    quote do
      result =
        Runner.run_sync(
          "#{:erlang.unique_integer([:positive])}",
          nil,
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
        "SELECT COUNT(*), COUNT(DISTINCT value), MIN(value), round(VARIANCE(value)), SUM(value) FROM #{@table}",
        %{rows: [%{row: [4200, 5, -2, 2, -200]}]},
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
        %{rows: [%{row: [0.0, row1]}, %{row: [500.0, row2]}, %{row: [1000.0, 10]}]},
        concurrency: concurrency
      )

      assert_in_delta(row1, 5000, 50)
      assert_in_delta(row2, 5000, 50)
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
