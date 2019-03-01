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

    Application.put_env(
      :cloak,
      :anonymizer,
      anonymizer_config
      |> Keyword.put(:outliers_count, {2, 4, 0.5})
      |> Keyword.put(:low_count_soft_lower_bound, {5, 1})
      |> Keyword.put(:sum_noise_sigma, 1)
      |> Keyword.put(:sum_noise_sigma_scale_params, {1, 0.5})
    )

    on_exit(fn -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)

    :ok
  end

  test "count(*) uses the same noise as count(column)" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["other", "number"], [15, 1])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["other", "number"], [3, 1])
    :ok = insert_rows(_user_ids = 101..111, "noise_layers", ["other", "number"], [0, 1])

    assert_query("select count(*), count(number) from noise_layers where other <> 0", %{
      rows: [%{row: [count, count]}]
    })
  end

  test "an unclear condition produces the same noise as another equivalent unclear condition" do
    for i <- 1..100, do: :ok = insert_rows(_user_ids = i..i, "noise_layers", ["number", "other"], [i, 3])

    assert_query("select avg(number) from noise_layers where other - 1 = 2", %{
      rows: [%{row: [result1]}]
    })

    assert_query("select avg(number) from noise_layers where other + 1 = 4", %{
      rows: [%{row: [^result1]}]
    })
  end

  test "an unclear condition produces the same noise as a clear condition" do
    for i <- 1..100, do: :ok = insert_rows(_user_ids = i..i, "noise_layers", ["number", "other"], [i, 3])

    assert_query("select avg(number) from noise_layers where other = 3", %{
      rows: [%{row: [result1]}]
    })

    assert_query("select avg(number) from noise_layers where other + 1 = 4", %{
      rows: [%{row: [^result1]}]
    })
  end

  test "noise layers on different columns" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [6, 9])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [6, 9])

    assert_query("select avg(number) from noise_layers where number = 6", %{
      rows: [%{row: [value1]}]
    })

    assert_query("select avg(number) from noise_layers where other = 9", %{
      rows: [%{row: [value2]}]
    })

    assert value1 != value2
  end

  test "multiple noise layers on same column are joined" do
    number = 10
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [number])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [number])

    assert_query("select avg(number) from noise_layers where number = #{number}", %{
      rows: [%{row: [value1]}]
    })

    assert_query("select avg(number) from noise_layers where number = #{number} group by number", %{
      rows: [%{row: [value2]}]
    })

    assert value1 == value2
  end

  test "noise layers in subqueries" do
    number = 14
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [number])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [number])

    assert_query("select avg(number) from (select user_id, number from noise_layers where number = #{number}) foo", %{
      rows: [%{row: [value1]}]
    })

    assert_query("select avg(number) from noise_layers", %{rows: [%{row: [value2]}]})
    assert value1 != value2
  end

  test "noise layers in aggregating subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [7, 8])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [7, 14])

    assert_query("select avg(other) from (select user_id, other from noise_layers group by user_id, other) foo", %{
      rows: [%{row: [value1]}]
    })

    assert_query("select avg(other) from noise_layers", %{rows: [%{row: [value2]}]})
    assert value1 != value2
  end

  test "range noise layers" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [6, 11])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [6, 11])

    assert_query("select avg(number) from noise_layers where other between 0 and 100", %{
      rows: [%{row: [value1]}]
    })

    assert_query("select avg(number) from noise_layers", %{rows: [%{row: [value2]}]})
    assert value1 != value2
  end

  test "range noise layers are sensitive to constants" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number", "other"], [6, 11])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [6, 11])

    assert_query("select avg(number) from noise_layers where other between 0 and 100", %{
      rows: [%{row: [value1]}]
    })

    assert_query("select avg(number) from noise_layers where other between 0 and 1000", %{
      rows: [%{row: [value2]}]
    })

    assert value1 != value2
  end

  test "noise layers in hiding the low-count row" do
    other = 33

    for i <- 1..6, do: :ok = insert_rows(_user_ids = [i], "noise_layers", ["number", "other"], [i, other])

    assert_query("select number from noise_layers where other = #{other}", %{rows: rows1})
    assert_query("select number from noise_layers", %{rows: rows2})
    assert rows1 != rows2
  end

  test "adding a negative condition adds a noise layer" do
    :ok = insert_rows(_user_ids = 1..100, "noise_layers", ["number"], [14])
    :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [14])
    :ok = insert_rows(_user_ids = 101..110, "noise_layers", ["number"], [1000])

    assert_query("select avg(number) from noise_layers where number <> 1000", %{
      rows: [%{row: [value1]}]
    })

    assert_query("select avg(number) from noise_layers", %{rows: [%{row: [value2]}]})
    assert value1 != value2
  end

  test "the reported noise should scale with the layers of noise" do
    :ok = insert_rows(_user_ids = 1..50, "noise_layers", ["number"], [100])

    # The average value is 100, which means we will use a noise of:
    # average * sum_noise_sigma * sqrt(num noise layers) = 100 * 1 * sqrt(2) = 100 * sqrt(2) ~ 141 ---> 140
    assert_query("select sum_noise(number) from noise_layers where number = 100", %{
      rows: [%{row: [140.0]}]
    })
  end

  test "same noise is generated if analyst forces floating" do
    :ok = insert_rows(_user_ids = 1..50, "noise_layers", ["number"], [40])

    query1 = "SELECT count(*) FROM noise_layers WHERE number = 40"
    query2 = "SELECT count(*) FROM (SELECT user_id FROM noise_layers GROUP BY user_id HAVING max(number) + 1 = 41) t"

    assert_query(query1, %{rows: [%{row: [count]}]})
    assert_query(query2, %{rows: [%{row: [^count]}]})
  end

  describe "same noise for analyst table and regular subquery" do
    setup do
      for data_source <- Cloak.DataSource.all() do
        Cloak.Test.AnalystTableHelpers.clear_analyst_tables(data_source)
      end

      Cloak.Air.register_air("some air")
      on_exit(fn -> Cloak.Air.unregister_air() end)
    end

    test "condition in query" do
      :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [10])
      :ok = insert_rows(_user_ids = 11..20, "noise_layers", ["number"], [11])

      query = "SELECT count(*) FROM $subquery WHERE column = 100"
      subquery = "SELECT user_id, number * number AS column FROM noise_layers"

      assert_analyst_table_consistent(query, subquery)
    end

    test "condition in analyst table" do
      :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [10])
      :ok = insert_rows(_user_ids = 11..20, "noise_layers", ["number"], [11])

      query = "SELECT count(*) FROM $subquery"
      subquery = "SELECT user_id, number * number AS column FROM noise_layers WHERE number = 10"

      assert_analyst_table_consistent(query, subquery)
    end

    test "aggregated subquery" do
      :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [10])
      :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number"], [-10])
      :ok = insert_rows(_user_ids = 11..20, "noise_layers", ["number"], [11])

      query = "SELECT count(*) FROM $subquery WHERE column = 100"
      subquery = "SELECT user_id, number * number AS column FROM noise_layers GROUP BY 1, 2"

      assert_analyst_table_consistent(query, subquery)
    end

    test "nested analyst tables" do
      :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [10, 1])
      :ok = insert_rows(_user_ids = 1..10, "noise_layers", ["number", "other"], [-10, 2])
      :ok = insert_rows(_user_ids = 11..20, "noise_layers", ["number", "other"], [11, 1])

      for data_source <- Cloak.DataSource.all() do
        {:ok, _} =
          Cloak.Test.AnalystTableHelpers.create_or_update(
            1,
            "bar",
            "SELECT user_id, number FROM noise_layers WHERE sqrt(other) = 1",
            data_source
          )
      end

      IO.inspect("DONE")

      query = "SELECT count(*) FROM $subquery WHERE column = 100"
      subquery = "SELECT user_id, number * number AS column FROM bar GROUP BY 1, 2"

      assert_analyst_table_consistent(query, subquery)
    end

    def assert_analyst_table_consistent(query, subquery) do
      regular_query = query |> String.replace("$subquery", "(#{subquery}) AS foo")
      analyst_query = query |> String.replace("$subquery", "foo")

      for data_source <- Cloak.DataSource.all() do
        {:ok, _} = Cloak.Test.AnalystTableHelpers.create_or_update(1, "foo", subquery, data_source)

        assert_query(regular_query, [analyst_id: 1, data_source: data_source], %{rows: result_regular})
        assert_query(analyst_query, [analyst_id: 1, data_source: data_source], %{rows: ^result_regular})
      end
    end
  end
end
