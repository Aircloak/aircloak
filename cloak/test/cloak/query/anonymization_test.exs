defmodule Cloak.Query.AnonymizationTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("anonymizations", "number REAL, string TEXT")
    :ok = Cloak.Test.DB.create_table("other_table", "dummy BOOLEAN")
  end

  setup do
    Cloak.Test.DB.clear_table("anonymizations")
    Cloak.Test.DB.clear_table("other_table")
    :ok
  end

  describe "count(distinct)" do
    setup do
      :ok = insert_rows(_user_ids = 0..19, "anonymizations", ["number"], [180])
      :ok = insert_rows(_user_ids = 20..29, "anonymizations", ["number"], [170])
      :ok = insert_rows(_user_ids = 20..29, "anonymizations", ["number"], [175])
      :ok = insert_rows(_user_ids = 30..39, "anonymizations", ["number"], [160])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [150])
      :ok = insert_rows(_user_ids = 50..59, "anonymizations", ["number"], [190])

      :ok
    end

    test "counting values represented by many users" do
      assert_query("select count(distinct number) from anonymizations", %{
        columns: ["count"],
        rows: [%{row: [5], occurrences: 1}]
      })
    end

    test "ignoring nils" do
      :ok = insert_rows(_user_ids = 41..49, "anonymizations", ["number"], [nil])

      assert_query("select count(distinct number) from anonymizations", %{
        columns: ["count"],
        rows: [%{row: [5], occurrences: 1}]
      })
    end

    test "hiding users with many distinct values" do
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [151])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [152])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [153])

      assert_query("select count(distinct number) from anonymizations", %{
        columns: ["count"],
        rows: [%{row: [5], occurrences: 1}]
      })
    end

    test "a user with many non-unique values should be treated as one with few distinct values" do
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [151])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [152])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [153])
      for _ <- 1..10, do: :ok = insert_rows(_user_ids = [60], "anonymizations", ["number"], [151])

      assert_query("select count(distinct number) from anonymizations", %{
        columns: ["count"],
        rows: [%{row: [6], occurrences: 1}]
      })
    end
  end

  describe "noisy counts are always positive" do
    setup do
      anonymizer_config = Application.get_env(:cloak, :anonymizer)

      Application.put_env(
        :cloak,
        :anonymizer,
        anonymizer_config
        |> Keyword.put(:outliers_count, {2, 4, 0.5})
        |> Keyword.put(:low_count_soft_lower_bound, {5, 1})
        |> Keyword.put(:sum_noise_sigma, 3)
        |> Keyword.put(:sum_noise_sigma_scale_params, {1, 0.5})
      )

      on_exit(fn -> Application.put_env(:cloak, :anonymizer, anonymizer_config) end)

      :ok
    end

    test "no-uid" do
      for i <- 1..50, do: :ok = insert_rows(_user_ids = (i * 2)..100, "anonymizations", ["number"], [i])

      assert_query("select count(number) from anonymizations where number between 1 and 100 group by number", %{
        rows: rows
      })

      assert Enum.all?(rows, fn %{row: [count]} -> count > 0 end)
    end

    test "uid" do
      for i <- 1..50, do: :ok = insert_rows(_user_ids = (i * 2)..100, "anonymizations", ["number"], [i])

      assert_query(
        "select count(number), median(number) from anonymizations where number between 1 and 100 group by number",
        %{rows: rows}
      )

      assert Enum.all?(rows, fn %{row: [count, _]} -> count > 0 end)
    end
  end

  test "no-uid with nulls" do
    :ok = insert_rows(_user_ids = 0..10, "anonymizations", ["number", "string"], [nil, "aa"])
    :ok = insert_rows(_user_ids = 5..15, "anonymizations", ["number", "string"], [nil, "ab"])
    :ok = insert_rows(_user_ids = 5..15, "anonymizations", ["number", "string"], [nil, "ba"])
    :ok = insert_rows(_user_ids = 15..25, "anonymizations", ["number", "string"], [1, "bb"])

    assert_query("select left(string, 1), round(avg(number)) from anonymizations group by 1 order by 1", %{
      rows: [%{row: ["a", nil]}, %{row: ["b", 1]}]
    })
  end

  test "reported noise scales with number of conditions" do
    :ok = insert_rows(_user_ids = 0..10, "anonymizations", ["number"], [0])

    assert_query(
      "select count_noise(*), count_noise(distinct user_id) from anonymizations",
      %{rows: [%{row: [1.0, 1.0]}]}
    )

    assert_query(
      "select count_noise(*), count_noise(distinct user_id) from anonymizations where number between 1 and 100",
      %{rows: [%{row: [c1, c2]}]}
    )

    assert c1 > 1.0 and c2 > 1.0
  end

  test "stddev with large values" do
    :ok = insert_rows(_user_ids = 1..10, "anonymizations", ["number"], [50])

    assert_query(
      "select stddev(1000000^number) from anonymizations",
      %{rows: [%{row: [nil]}]}
    )
  end

  test "median with large values" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [50])

    assert_query(
      "select median(1000000^number) from anonymizations",
      %{rows: [%{row: [nil]}]}
    )
  end

  test "uid-based sum with large values" do
    :ok = insert_rows(_user_ids = 1..1000, "anonymizations", ["number"], [51])

    assert_query(
      "select sum(1000000^number), median(1) from anonymizations",
      %{rows: [%{row: [nil, 1]}]}
    )
  end
end
