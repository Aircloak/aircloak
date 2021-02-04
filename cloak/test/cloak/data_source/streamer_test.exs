defmodule Cloak.DataSource.StreamerTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource.Streamer

  import Cloak.Test.QueryHelpers, only: [default_data_source: 0]
  import Aircloak.AssertionHelper

  setup_all do
    :ok = Cloak.Test.DB.create_table("test_streamer", "intval INTEGER")
    :ok = Cloak.Test.QueryHelpers.insert_rows(_user_ids = 1..500, "test_streamer", ["intval"], [42])
  end

  test "streaming" do
    assert {:ok, rows_stream} = rows("select stddev(intval) from test_streamer")
    rows = Enum.to_list(rows_stream)

    assert length(rows) == 500
    assert Enum.all?(rows, &match?([_user_id, _intval = 42], &1))
  end

  for concurrency <- [2, 3, 5, 7] do
    test "concurrent stream processing with #{concurrency} workers" do
      {:ok, rows_stream} = rows("select stddev(intval) from test_streamer")
      sequential_rows = Enum.to_list(rows_stream)

      {:ok, rows_stream} = rows("select stddev(intval) from test_streamer")

      concurrent_rows =
        1..unquote(concurrency)
        |> Enum.map(fn _ -> Task.async(fn -> Enum.to_list(rows_stream) end) end)
        |> Enum.flat_map(&Task.await/1)

      assert MapSet.equal?(MapSet.new(concurrent_rows), MapSet.new(sequential_rows))
    end
  end

  test "connection failure" do
    with_short_connection_timeout(fn ->
      soon do
        ExUnit.CaptureLog.capture_log(fn ->
          assert {:error, error} = rows("select intval from test_streamer", data_source(%{hostname: "invalid_host"}))
          assert error =~ ~r/Failed to establish a connection to the database/
        end)
      end
    end)
  end

  test "connection timeout" do
    with_short_connection_timeout(0, fn ->
      assert rows("select intval from test_streamer", data_source()) == {:error, "Timeout connecting to the database."}
    end)
  end

  test "SQL error is properly reported" do
    Cloak.Test.DB.create_table("temp_table", "intval INTEGER")

    query =
      Cloak.Sql.Parser.parse!("select intval from temp_table")
      |> Cloak.Sql.Compiler.compile!(nil, data_source(), [], %{})
      |> Cloak.Sql.Query.resolve_db_columns()

    Cloak.Test.DB.delete_table("temp_table")

    assert {:ok, stream} = Streamer.rows(query)
    e = assert_raise Cloak.Query.ExecutionError, fn -> Stream.run(stream) end
    assert e.message =~ ~r/relation "cloak_test.temp_table" does not exist/
  end

  defp rows(query, data_source \\ data_source()) do
    Cloak.Sql.Parser.parse!(query)
    |> Cloak.Sql.Compiler.compile!(nil, data_source, [], %{})
    |> Cloak.Sql.Query.resolve_db_columns()
    |> Streamer.rows()
  end

  defp with_short_connection_timeout(timeout \\ 50, fun) do
    data_source_config = Application.get_env(:cloak, :data_source)
    Application.put_env(:cloak, :data_source, Keyword.put(data_source_config, :connect_timeout, timeout))

    try do
      fun.()
    after
      Application.put_env(:cloak, :data_source, data_source_config)
    end
  end

  defp data_source(extra_params \\ %{}) do
    default_data_source()
    |> update_in([:parameters], &Map.merge(&1, extra_params))
  end
end
