defmodule Cloak.Query.DataDecoderTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    decoders = [
      %{method: "text_to_integer", columns: ["int_val"]},
      %{method: "text_to_real", columns: ["real_val"]},
      %{method: "text_to_boolean", columns: ["bool_val"]},

    ]
    :ok = Cloak.Test.DB.create_table("encoded", "int_val TEXT, real_val TEXT, bool_val TEXT", [decoders: decoders])
    :ok = Cloak.Test.DB.create_table("encoded_join", "_dummy INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("encoded")
    :ok
  end

  test "simple decoding of encoded booleans" do
    :ok = insert_rows(_user_ids = 1..10, "encoded", ["bool_val"], ["true"])
    :ok = insert_rows(_user_ids = 11..20, "encoded", ["bool_val"], ["yes"])
    :ok = insert_rows(_user_ids = 21..30, "encoded", ["bool_val"], ["YES"])
    :ok = insert_rows(_user_ids = 31..40, "encoded", ["bool_val"], ["TRUE"])
    :ok = insert_rows(_user_ids = 41..50, "encoded", ["bool_val"], ["No"])
    :ok = insert_rows(_user_ids = 51..60, "encoded", ["bool_val"], ["False"])
    :ok = insert_rows(_user_ids = 61..70, "encoded", ["bool_val"], [nil])

    assert_query "select bool_val from encoded where bool_val is not null order by bool_val",
      %{rows: [%{occurrences: 20, row: [false]}, %{occurrences: 40, row: [true]}]}
    assert_query "select bool_val from encoded where bool_val = true",
      %{rows: [%{occurrences: 40, row: [true]}]}
    assert_query "select bool_val from encoded where bool_val <> true",
      %{rows: [%{occurrences: 20, row: [false]}]}
  end

  test "simple decoding of encoded reals" do
    :ok = insert_rows(_user_ids = 1..10, "encoded", ["real_val"], ["1.22"])
    :ok = insert_rows(_user_ids = 11..20, "encoded", ["real_val"], ["22.0"])
    :ok = insert_rows(_user_ids = 21..30, "encoded", ["real_val"], [nil])

    assert_query "select real_val from encoded where real_val is not null order by real_val",
      %{rows: [%{occurrences: 10, row: [1.22]}, %{occurrences: 10, row: [22.0]}]}
    assert_query "select real_val from encoded where real_val = 1.22",
      %{rows: [%{occurrences: 10, row: [1.22]}]}
    assert_query "select real_val from encoded where real_val <> 1.22",
      %{rows: [%{occurrences: 10, row: [22.0]}]}
  end

  test "simple decoding of encoded integers" do
    :ok = insert_rows(_user_ids = 1..10, "encoded", ["int_val"], ["111"])
    :ok = insert_rows(_user_ids = 11..20, "encoded", ["int_val"], ["222"])
    :ok = insert_rows(_user_ids = 21..30, "encoded", ["int_val"], [nil])

    assert_query "select int_val from encoded where int_val is not null order by int_val",
      %{rows: [%{occurrences: 10, row: [111]}, %{occurrences: 10, row: [222]}]}
    assert_query "select int_val from encoded where int_val = 111",
      %{rows: [%{occurrences: 10, row: [111]}]}
    assert_query "select int_val from encoded where int_val <> 222",
      %{rows: [%{occurrences: 10, row: [111]}]}
  end

  test "decoding of encoded data from a join" do
    :ok = insert_rows(_user_ids = 1..15, "encoded", ["int_val"], ["123"])
    :ok = insert_rows(_user_ids = 10..20, "encoded_join", [], [])

    assert_query """
      select int_val from
      encoded as t1 left join encoded_join as t2 on t1.user_id = t2.user_id
      where t2.user_id is null
    """, %{rows: [%{occurrences: 9, row: [123]}]}
  end
end
