defmodule Cloak.Query.DataDecoderTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    decoder = %{method: "base64", columns: ["value"]}
    :ok = Cloak.Test.DB.create_table("encoded", "value TEXT", [decoders: [decoder]])
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("encoded")
    :ok
  end

  test "decoding of encoded data" do
    :ok = insert_rows(_user_ids = 1..10, "encoded", ["value"], [Base.encode64("aaa")])
    :ok = insert_rows(_user_ids = 11..20, "encoded", ["value"], [Base.encode64("bbb")])
    :ok = insert_rows(_user_ids = 21..30, "encoded", ["value"], [nil])

    assert_query "select value from encoded where value is not null order by value",
      %{rows: [%{occurrences: 10, row: ["aaa"]}, %{occurrences: 10, row: ["bbb"]}]}
    assert_query "select value from encoded where value = 'aaa'",
      %{rows: [%{occurrences: 10, row: ["aaa"]}]}
    assert_query "select value from encoded where value <> 'aaa'",
      %{rows: [%{occurrences: 10, row: ["bbb"]}]}
  end
end
