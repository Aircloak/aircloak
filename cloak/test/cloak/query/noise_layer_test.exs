defmodule Cloak.Query.NoiseLayerTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("noise_layers", "number REAL, other REAL")
  end

  setup do
    Cloak.Test.DB.clear_table("noise_layers")

    anonymizer_config = Application.get_env(:cloak, :anonymizer)
    Application.put_env(:cloak, :anonymizer,
      anonymizer_config
      |> Keyword.put(:outliers_count, {4, 1})
      |> Keyword.put(:low_count_soft_lower_bound, {5, 1})
    )
    on_exit(fn() -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)

    :ok
  end

  test "count(*) uses a different noise layer than count(column)" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [6])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [3])

    assert_query "select count(*), count(number) from noise_layers where number <> 0",
      %{rows: [%{row: [value1, value2]}]}
    assert value1 != value2
  end

  test "noise layers on different columns" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [6, 7])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [6, 7])

    assert_query "select avg(number) from noise_layers where number = 6",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers where other = 7",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "multiple noise layers on same column" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [7])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [7])

    assert_query "select avg(number) from noise_layers where number = 7",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers where number = 7 group by number",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "noise layers in subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [7])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [7])

    assert_query "select avg(number) from (select user_id, number from noise_layers where number = 7) foo",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "noise layers in aggregating subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [7, 8])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [7, 9])

    assert_query "select avg(other) from (select user_id, other from noise_layers group by user_id, other) foo",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(other) from noise_layers",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "noise layers in hiding the low-count row" do
    # If this test starts failing due to some unrelated change in how the random numbers in aggregator are computed
    # you can get another "roll" by changing this number. To make sure it is still valid check that it fails when
    # Aggregator uses an Anonymizer built with only the user_id layer for the decision to hide the low-count row.
    other = 20

    for i <- 1..5, do:
      :ok = insert_rows(_user_ids = [i], "noise_layers", ["number", "other"], [i, other])

    assert_query "select number from noise_layers where other = #{other}", %{rows: rows1}
    assert_query "select number from noise_layers", %{rows: rows2}
    assert rows1 != rows2
  end
end
