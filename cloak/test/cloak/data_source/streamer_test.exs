defmodule Cloak.DataSource.StreamerTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource.Streamer

  setup_all do
    :ok = Cloak.Test.DB.create_table("test_streamer", "intval INTEGER")
    :ok = Cloak.Test.QueryHelpers.insert_rows(_user_ids = 1..500, "test_streamer", ["intval"], [42])
  end

  test "streaming" do
    assert {:ok, chunks_stream} = chunks("select intval from test_streamer")
    chunks = Enum.to_list(chunks_stream)
    assert length(chunks) == 5

    assert Enum.all?(rows(chunks), &match?([_user_id, _intval = 42], &1))
  end

  test "concurrent stream processing" do
    assert {:ok, chunks_stream} = chunks("select intval from test_streamer")

    [chunks1, chunks2] =
      1..2
      |> Enum.map(fn _ -> Task.async(fn -> Enum.to_list(chunks_stream) end) end)
      |> Enum.map(&Task.await/1)

    assert length(chunks1) + length(chunks2) == 5
    refute Enum.empty?(chunks1)
    refute Enum.empty?(chunks2)

    rows1 = rows(chunks1)
    rows2 = rows(chunks2)

    assert MapSet.disjoint?(MapSet.new(rows1), MapSet.new(rows2))
    assert Enum.all?(Enum.concat(rows1, rows2), &match?([_user_id, _intval = 42], &1))
  end

  test "connection failure" do
    with_short_connection_timeout(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        assert {:error, error} = chunks("select * from test_streamer", data_source(%{hostname: "invalid_host"}))
        assert error =~ ~r/Failed to establish a connection to the database/
      end)
    end)
  end

  test "SQL error is properly reported" do
    ExUnit.CaptureLog.capture_log(fn ->
      Cloak.Test.DB.create_table("temp_table", "intval INTEGER")

      query =
        Cloak.Sql.Parser.parse!("select * from temp_table")
        |> Cloak.Sql.Compiler.compile!(data_source(), [], %{})
        |> Cloak.Sql.Query.resolve_db_columns()

      Cloak.Test.DB.delete_table("temp_table")

      assert {:error, reason} = Streamer.chunks(query)
      assert reason =~ ~r/relation "cloak_test.temp_table" does not exist/
    end)
  end

  defp chunks(query, data_source \\ data_source()) do
    Cloak.Sql.Parser.parse!(query)
    |> Cloak.Sql.Compiler.compile!(data_source, [], %{})
    |> Cloak.Sql.Query.resolve_db_columns()
    |> Streamer.chunks()
  end

  defp rows(chunks), do: Enum.flat_map(chunks, & &1)

  defp with_short_connection_timeout(fun) do
    connect_retries = Application.get_env(:cloak, :connect_retries)
    data_source_config = Application.get_env(:cloak, :data_source)

    Application.put_env(:cloak, :connect_retries, 0)
    Application.put_env(:cloak, :data_source, Keyword.put(data_source_config, :connect_timeout, 50))

    try do
      fun.()
    after
      Application.put_env(:cloak, :connect_retries, connect_retries)
      Application.put_env(:cloak, :data_source, data_source_config)
    end
  end

  defp data_source(extra_params \\ %{}) do
    Cloak.DataSource.all()
    |> hd()
    |> update_in([:parameters], &Map.merge(&1, extra_params))
  end
end
