defmodule Cloak.DataSource.MongoDBJoinTimingTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource.MongoDB

  import Cloak.Test.MongoHelpers

  @moduletag :exclude_in_dev
  @moduletag :mongodb

  setup_all do
    parameters = [hostname: "localhost", database: "cloaktest"]
    {:ok, conn} = Mongo.start_link(parameters)
    Mongo.delete_many(conn, "jt_left", %{})
    Mongo.delete_many(conn, "jt_right", %{})

    for i <- 1..5, do: Mongo.insert_one!(conn, "jt_left", %{id: i})

    data = for i <- 1..50_000, do: %{id: i}
    Mongo.insert_many!(conn, "jt_right", data)

    tables = %{
      "jt_left" => Cloak.DataSource.Table.new("jt_left", "id", db_name: "jt_left"),
      "jt_right" => Cloak.DataSource.Table.new("jt_right", "id", db_name: "jt_right", sample_rate: 1)
    }

    data_source =
      %{
        name: "mongo_db_join_timing",
        concurrency: 0,
        lcf_buckets_aggregation_limit: nil,
        statistics_anonymization: false,
        driver: MongoDB,
        driver_info: MongoDB.driver_info(conn),
        parameters: parameters,
        tables: tables,
        errors: []
      }
      |> Cloak.DataSource.Table.load(conn)

    GenServer.stop(conn, :normal, :timer.seconds(5))
    {:ok, data_source: data_source}
  end

  @attack_statement """
    SELECT count(distinct t1.id) FROM 
      (SELECT id FROM jt_left WHERE id = <matched_id>) t1
    INNER JOIN
      jt_right t2
    ON t1.id = t2.id
  """

  test "query correctness as run_query! doesn't propagate errors properly", context do
    assert_query(
      context,
      String.replace(@attack_statement, "<matched_id>", "0"),
      %{rows: [%{row: [0]}]}
    )
  end

  test "join timing vulnerability", context do
    time1 =
      benchmark(
        context,
        String.replace(@attack_statement, "<matched_id>", "1")
      )

    time2 =
      benchmark(
        context,
        String.replace(@attack_statement, "<matched_id>", "0")
      )

    assert_in_delta time1, time2, 0.25 * max(time1, time2)
  end

  @iterations 25
  defp benchmark(context, query) do
    start_time = :erlang.monotonic_time(:milli_seconds)
    for _i <- 1..@iterations, do: run_query!(context.data_source, query)
    end_time = :erlang.monotonic_time(:milli_seconds) - start_time
    end_time / @iterations
  end
end
