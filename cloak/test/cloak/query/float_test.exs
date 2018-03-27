defmodule Cloak.Query.FloatTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("floats", "value REAL")
  end

  setup do
    Cloak.Test.DB.clear_table("floats")
    :ok
  end

  test "unary trunc" do
    :ok = insert_rows(_user_ids = 1..10, "floats", ["value"], [12.234])

    assert_query("select trunc(value) from floats", %{
      columns: ["trunc"],
      rows: [%{occurrences: 10, row: [12]}]
    })
  end

  test "binary trunc" do
    :ok = insert_rows(_user_ids = 1..10, "floats", ["value"], [12.234])

    assert_query("select trunc(value, 2) from floats", %{
      columns: ["trunc"],
      rows: [%{occurrences: 10, row: [12.23]}]
    })
  end

  test "binary trunc in a grouped query" do
    :ok = insert_rows(_user_ids = 1..10, "floats", ["value"], [12.234])

    assert_query("select trunc(value, 2) from floats group by value", %{
      columns: ["trunc"],
      rows: [%{occurrences: 1, row: [12.23]}]
    })
  end

  test "arithmetic expressions" do
    :ok = insert_rows(_user_ids = 1..10, "floats", ["value"], [4])

    assert_query("select 2 ^ 3 * (3 + 4 - 1) / 5 from floats", %{
      columns: [_],
      rows: [%{row: [9.6]}]
    })
  end

  test "function on an aggregated value" do
    :ok = insert_rows(_user_ids = 1..10, "floats", ["value"], [4])
    :ok = insert_rows(_user_ids = 11..100, "floats", ["value"], [9])

    assert_query("select round(avg(value)) from floats", %{
      columns: ["round"],
      rows: [%{row: [9], occurrences: 1}]
    })
  end

  test "nested function call" do
    :ok = insert_rows(_user_ids = 1..10, "floats", ["value"], [4])

    assert_query("select sqrt(abs(value)) from floats", %{
      columns: ["sqrt"],
      rows: [%{row: [value], occurrences: 10}]
    })

    assert_in_delta value, 2, 0.1
  end

  test "aggregating a function" do
    :ok = insert_rows(_user_ids = 1..10, "floats", ["value"], [4])
    :ok = insert_rows(_user_ids = 11..20, "floats", ["value"], [9])

    assert_query("select avg(sqrt(value)) from floats", %{
      columns: ["avg"],
      rows: [%{row: [value], occurrences: 1}]
    })

    assert_in_delta value, 2.5, 0.01
  end
end
