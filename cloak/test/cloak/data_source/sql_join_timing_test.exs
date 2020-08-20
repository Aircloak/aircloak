defmodule Cloak.DataSource.SQLJoinTimingTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  import Aircloak.AssertionHelper

  @moduletag :exclude_in_dev

  setup_all do
    :ok = Cloak.Test.DB.create_table("jt_left", "id INTEGER, val INTEGER")
    :ok = Cloak.Test.DB.create_table("jt_right", "id INTEGER")

    :ok = insert_rows(_user_ids = 1..10, "jt_left", ["id", "val"], [1, 0])
    :ok = insert_rows(_user_ids = 1..30_000, "jt_right", ["id"], [1])
    :ok = insert_rows(_user_ids = 1..30_000, "jt_right", ["id"], [2])
    :ok = insert_rows(_user_ids = 1..30_000, "jt_right", ["id"], [3])
  end

  @attack_statements [
    """
      SELECT count(t2.id) FROM
        (SELECT * FROM (SELECT user_id FROM jt_left WHERE id = <matched_id>) t) t1
      INNER JOIN
        (SELECT DISTINCT user_id, id FROM jt_right) t2
      ON t1.user_id = t2.user_id
    """,
    """
      SELECT count(t2.id) FROM
        (SELECT DISTINCT user_id, id FROM jt_right) t2
      RIGHT JOIN
        (SELECT user_id FROM jt_left WHERE id = <matched_id>) t1
      ON t1.user_id = t2.user_id
    """,
    """
      SELECT count(t3.id) FROM
        (SELECT user_id FROM jt_left) t1
      INNER JOIN
        (SELECT user_id FROM jt_left WHERE id = <matched_id>) t2
      ON t1.user_id = t2.user_id
      INNER JOIN
        (SELECT DISTINCT user_id, id FROM jt_right) t3
      ON t2.user_id = t3.user_id
    """,
    """
      SELECT count(t3.id) FROM
        jt_left t1
      INNER JOIN
        (SELECT user_id FROM jt_left WHERE id = <matched_id>) t2
      ON t1.user_id = t2.user_id
      INNER JOIN
        (SELECT DISTINCT user_id, id FROM jt_right) t3
      ON t2.user_id = t3.user_id
      WHERE val = 0
    """,
    """
      SELECT count(t3.id) FROM
        jt_left t1
      INNER JOIN
        (SELECT user_id FROM jt_left WHERE id = <matched_id>) t2
      ON t1.user_id = t2.user_id
      LEFT JOIN
        (SELECT DISTINCT user_id, id FROM jt_right) t3
      ON t2.user_id = t3.user_id
      WHERE val = 0
    """
  ]

  for {attack_statement, index} <- Enum.with_index(@attack_statements, 1) do
    test "join timing vulnerability (#{index})" do
      soon attempts: 2 do
        time1 = unquote(attack_statement) |> String.replace("<matched_id>", "1") |> benchmark()
        time2 = unquote(attack_statement) |> String.replace("<matched_id>", "0") |> benchmark()

        assert_in_delta time1, time2, 0.25 * max(time1, time2)
      end
    end
  end

  @iterations 25
  defp benchmark(query) do
    data_source = default_data_source() |> Map.put(:statistics_anonymization, false)
    warmup(data_source, query)

    start_time = :erlang.monotonic_time(:milli_seconds)
    for _i <- 1..@iterations, do: run_query(data_source, query)
    end_time = :erlang.monotonic_time(:milli_seconds) - start_time
    end_time / @iterations
  end

  defp warmup(data_source, query), do: run_query(data_source, query)

  defp run_query(data_source, query) do
    %{rows: [%{row: [_]}]} = Cloak.Query.Runner.run_sync("1", nil, data_source, query, [], %{})
  end
end
