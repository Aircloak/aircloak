defmodule Cloak.Query.NoiseLayerTest do
  @moduledoc """
  If tests in this module start failing due to some unrelated change in how the random numbers in aggregator are
  computed you can get another "roll" by changing the constants that noise layers are applied over. To make sure they
  are still valid you can disable noise layer computation entirely - all except for the count(*) vs count(column) one
  should fail.
  """

  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("noise_layers", "number REAL, other REAL, string TEXT")
  end

  setup do
    Cloak.Test.DB.clear_table("noise_layers")

    anonymizer_config = Application.get_env(:cloak, :anonymizer)
    Application.put_env(:cloak, :anonymizer,
      anonymizer_config
      |> Keyword.put(:outliers_count, {4, 0.5})
      |> Keyword.put(:low_count_soft_lower_bound, {5, 1})
      |> Keyword.put(:sum_noise_sigma, 1)
    )
    on_exit(fn() -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)

    :ok
  end

  test "count(*) uses a different noise layer than count(column)" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [15])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [3])
    :ok = insert_rows(_user_ids = 101..111, "noise_layers", ["number"], [0])

    assert_query "select count(*), count(number) from noise_layers where number <> 0",
      %{rows: [%{row: [value1, value2]}]}
    assert value1 != value2
  end

  test "an unclear condition produces the same noise as an equivalent clear condition" do
    for i <- 1..100, do:
      :ok = insert_rows(_user_ids = i..i, "noise_layers", ["number", "other"], [i, 3])

    assert_query "select avg(number) from noise_layers where other = 3", %{rows: [%{row: [result1]}]}
    assert_query "select avg(number) from noise_layers where other + 1 = 4", %{rows: [%{row: [^result1]}]}
  end

  test "noise layers on different columns" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [6, 9])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [6, 9])

    assert_query "select avg(number) from noise_layers where number = 6",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers where other = 9",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "multiple noise layers on same column are joined" do
    number = 10
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [number])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [number])

    assert_query "select avg(number) from noise_layers where number = #{number}",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers where number = #{number} group by number",
      %{rows: [%{row: [value2]}]}
    assert value1 == value2
  end

  test "noise layers in subqueries" do
    number = 14
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [number])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [number])

    assert_query "select avg(number) from (select user_id, number from noise_layers where number = #{number}) foo",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "noise layers in aggregating subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [7, 8])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [7, 14])

    assert_query "select avg(other) from (select user_id, other from noise_layers group by user_id, other) foo",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(other) from noise_layers",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "range noise layers" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [6, 11])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [6, 11])

    assert_query "select avg(number) from noise_layers where other between 0 and 100",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "range noise layers are sensitive to constants" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [6, 11])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [6, 11])

    assert_query "select avg(number) from noise_layers where other between 0 and 100",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers where other between 0 and 1000",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  test "noise layers in hiding the low-count row" do
    other = 26

    for i <- 1..5, do:
      :ok = insert_rows(_user_ids = [i], "noise_layers", ["number", "other"], [i, other])

    assert_query "select number from noise_layers where other = #{other}", %{rows: rows1}
    assert_query "select number from noise_layers", %{rows: rows2}
    assert rows1 != rows2
  end

  test "noise layers in hiding the user_count" do
    number = 15
    :ok = insert_rows(_user_ids = 1..5, "noise_layers", ["number"], [number])

    assert_query "select avg(other) from noise_layers where number = #{number}",
      %{users_count: count1}
    assert_query "select avg(other) from noise_layers",
      %{users_count: count2}
    assert count1 != count2
  end

  test "adding a negative condition adds a noise layer" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [14])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [14])
    :ok = insert_rows(_user_ids = 101..110, "noise_layers", ["number"], [1000])

    assert_query "select avg(number) from noise_layers where number <> 1000",
      %{rows: [%{row: [value1]}]}
    assert_query "select avg(number) from noise_layers",
      %{rows: [%{row: [value2]}]}
    assert value1 != value2
  end

  describe "probing" do
    test "complex negative conditions matching too few users are dropped" do
      :ok = insert_rows(_user_ids = 1..50, "noise_layers", ["number"], [100])
      :ok = insert_rows(_user_ids = 26..75, "noise_layers", ["number"], [50])
      :ok = insert_rows(_user_ids = 76..76, "noise_layers", ["number"], [400])

      assert_query "select count(*) from noise_layers", %{rows: [%{row: [result1]}]}
      assert_query "select count(*) from noise_layers where sqrt(number) <> 20", %{rows: [%{row: [result2]}]}
      assert result1 == result2
    end

    test "complex negative conditions matching enough users are kept" do
      :ok = insert_rows(_user_ids = 1..50, "noise_layers", ["number"], [100])
      :ok = insert_rows(_user_ids = 26..75, "noise_layers", ["number"], [50])

      assert_query "select count(number) from noise_layers where sqrt(number) <> 10", %{rows: [%{row: [48]}]}
    end

    test "complex negative LIKE conditions matching too few users are dropped" do
      :ok = insert_rows(_user_ids = 1..50, "noise_layers", ["string"], ["aa"])
      :ok = insert_rows(_user_ids = 26..75, "noise_layers", ["string"], ["ab"])
      :ok = insert_rows(_user_ids = 76..76, "noise_layers", ["string"], ["bb"])

      assert_query "select count(number) from noise_layers", %{rows: [%{row: [result1]}]}
      assert_query "select count(number) from noise_layers where string NOT LIKE '%bb'", %{rows: [%{row: [result2]}]}
      assert result1 == result2
    end

    test "complex negative LIKE conditions matching enough users are kept" do
      :ok = insert_rows(_user_ids = 1..50, "noise_layers", ["string"], ["aa"])
      :ok = insert_rows(_user_ids = 26..75, "noise_layers", ["string"], ["bb"])

      assert_query "select count(*) from noise_layers where string NOT LIKE '%bb'", %{rows: [%{row: [50]}]}
    end
  end

  test "the reported noise should scale with the layers of noise" do
    :ok = insert_rows(_user_ids = 1..50, "noise_layers", ["number"], [100])

    # The average value is 100, which means we will use a noise of:
    # average * sum_noise_sigma * sqrt(num noise layers) = 100 * 1 * sqrt(2) = 100 * sqrt(2) ~ 141 ---> 140
    assert_query "select sum_noise(number) from noise_layers where number = 100", %{rows: [%{row: [140.0]}]}
  end

  test "[Issue #1538] joins with projected tables are order-independent" do
    :ok = Cloak.Test.DB.create_table("noise_layers_join1", "row_id INTEGER")
    :ok = Cloak.Test.DB.create_table("noise_layers_join2", "row_id INTEGER, one_id INTEGER", [
      projection: %{table: "noise_layers_join1", foreign_key: "one_id", primary_key: "row_id"},
      add_user_id: false,
    ])
    :ok = Cloak.Test.DB.create_table("noise_layers_join3", "one_id INTEGER, two_id INTEGER", [
      projection: %{table: "noise_layers_join1", foreign_key: "one_id", primary_key: "row_id"},
      add_user_id: false,
    ])

    :ok = insert_rows(_user_ids = 1..20, "noise_layers_join1", ["row_id"], [100])
    data = 1..20 |> Enum.to_list() |> Enum.map(fn(_) -> [100, 100] end)
    Cloak.Test.DB.insert_data("noise_layers_join2", ["row_id", "one_id"], data)
    Cloak.Test.DB.insert_data("noise_layers_join3", ["one_id", "two_id"], data)

    assert_query """
      select count(*) from noise_layers_join3 inner join noise_layers_join2
        on noise_layers_join3.user_id = noise_layers_join2.user_id
        and noise_layers_join2.row_id = noise_layers_join3.two_id
        inner join noise_layers_join1 as noise_layers_join1
        on noise_layers_join1.user_id = noise_layers_join2.user_id
        and noise_layers_join1.row_id = noise_layers_join2.one_id
    """, %{rows: [%{row: [count]}]}
    assert_query """
      select count(*) from noise_layers_join1 inner join noise_layers_join2
        on noise_layers_join1.user_id = noise_layers_join2.user_id
        and noise_layers_join1.row_id = noise_layers_join2.one_id
        inner join noise_layers_join3
        on noise_layers_join3.user_id = noise_layers_join2.user_id
        and noise_layers_join2.row_id = noise_layers_join3.two_id
    """, %{rows: [%{row: [^count]}]}
  end
end
