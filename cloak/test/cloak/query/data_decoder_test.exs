defmodule Cloak.Query.DataDecoderTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    decoder = %{method: "text_to_integer", columns: ["value"]}
    :ok = Cloak.Test.DB.create_table("encoded", "value TEXT", [decoders: [decoder]])
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("encoded")
    :ok
  end

  test "decoding of encoded data" do
    :ok = insert_rows(_user_ids = 1..10, "encoded", ["value"], ["111"])
    :ok = insert_rows(_user_ids = 11..20, "encoded", ["value"], ["222"])
    :ok = insert_rows(_user_ids = 21..30, "encoded", ["value"], [nil])

    assert_query "select value from encoded where value is not null order by value",
      %{rows: [%{occurrences: 10, row: [111]}, %{occurrences: 10, row: [222]}]}
    assert_query "select value from encoded where value = 111",
      %{rows: [%{occurrences: 10, row: [111]}]}
    assert_query "select value from encoded where value <> 222",
      %{rows: [%{occurrences: 10, row: [111]}]}
  end
end
