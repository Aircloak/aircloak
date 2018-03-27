defmodule Cloak.ResultSender.Test do
  use ExUnit.Case, async: true

  alias Cloak.ResultSender

  describe "send_result" do
    for key <- ~w/query_id columns features error cancelled info execution_time/a do
      test "preserves #{key}" do
        ResultSender.send_result({:process_encoded, self()}, %{unquote(key) => :something})
        assert_receive %{unquote(key) => :something}
      end
    end

    test "provides a total row count" do
      ResultSender.send_result({:process_encoded, self()}, %{
        rows: [%{occurrences: 3}, %{occurrences: 5}]
      })

      assert_receive %{row_count: 8}
    end

    test "encodes rows in chunks" do
      rows = Enum.map(1..1001, fn n -> %{occurrences: 1, row: [n]} end)
      ResultSender.send_result({:process_encoded, self()}, %{rows: rows})

      assert_receive %{chunks: [%{encoded_data: chunk1}, %{encoded_data: chunk2}]}

      assert [%{"occurrences" => 1, "row" => [1]} | _] =
               chunk1 |> :zlib.gunzip() |> Poison.decode!()

      assert [%{"occurrences" => 1, "row" => [1001]}] =
               chunk2 |> :zlib.gunzip() |> Poison.decode!()
    end
  end
end
