defmodule Cloak.ResultSender.Test do
  use ExUnit.Case, async: true

  alias Cloak.ResultSender

  describe "encode_result" do
    for key <- ~w/query_id columns features error cancelled info execution_time log/a do
      test "preserves #{key}" do
        assert {:ok, %{unquote(key) => :something}} = ResultSender.encode_result(%{unquote(key) => :something})
      end
    end

    test "includes a total row count" do
      assert {:ok, %{row_count: 8}} = ResultSender.encode_result(%{rows: [%{occurrences: 3}, %{occurrences: 5}]})
    end

    test "encodes rows in chunks" do
      assert {:ok, encoded} = ResultSender.encode_result(%{rows: Enum.map(1..1001, &%{occurrences: 1, row: [&1]})})
      assert %{chunks: [%{encoded_data: chunk1}, %{encoded_data: chunk2}]} = encoded
      assert [%{"occurrences" => 1, "row" => [1]} | _] = chunk1 |> :zlib.gunzip() |> Poison.decode!()
      assert [%{"occurrences" => 1, "row" => [1001]}] = chunk2 |> :zlib.gunzip() |> Poison.decode!()
    end
  end
end
