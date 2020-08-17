defmodule Cloak.Query.AnonymizationTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("anonymizations", "number REAL, string TEXT")
    :ok = Cloak.Test.DB.create_table("other_table", "dummy BOOLEAN")
    Cloak.TestShadowCache.safe(default_data_source(), "anonymizations", "number", [1, 2, 3])
  end

  setup do
    Cloak.Test.DB.clear_table("anonymizations")
    Cloak.Test.DB.clear_table("other_table")
    :ok
  end

  defp set_anon_params() do
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

    test "stats: counting values represented by many users" do
      assert_query("select count(distinct number) from anonymizations", %{
        columns: ["count"],
        rows: [%{row: [6], occurrences: 1}]
      })
    end

    test "uid: counting values represented by many users" do
      assert_query("select count(distinct number), stddev(0) from anonymizations", %{
        columns: ["count", _],
        rows: [%{row: [5, _], occurrences: 1}]
      })
    end

    test "stats: ignoring nils" do
      :ok = insert_rows(_user_ids = 41..49, "anonymizations", ["number"], [nil])

      assert_query("select count(distinct number) from anonymizations", %{
        columns: ["count"],
        rows: [%{row: [6], occurrences: 1}]
      })
    end

    test "uid: ignoring nils" do
      :ok = insert_rows(_user_ids = 41..49, "anonymizations", ["number"], [nil])

      assert_query("select count(distinct number), stddev(0) from anonymizations", %{
        columns: ["count", _],
        rows: [%{row: [5, _], occurrences: 1}]
      })
    end

    test "uid: hiding users with many distinct values" do
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [151])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [152])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [153])

      assert_query("select count(distinct number), stddev(0) from anonymizations", %{
        columns: ["count", _],
        rows: [%{row: [5, _], occurrences: 1}]
      })
    end

    test "uid: a user with many non-unique values should be treated as one with few distinct values" do
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [151])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [152])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [153])
      for _ <- 1..10, do: :ok = insert_rows(_user_ids = [60], "anonymizations", ["number"], [151])

      assert_query("select count(distinct number) from anonymizations", %{
        columns: ["count"],
        rows: [%{row: [9], occurrences: 1}]
      })
    end

    test "stats BUG: counting and grouping same column" do
      assert_query("select number, count(distinct round(number)) from anonymizations group by 1 order by 1", %{
        rows: [
          %{row: [160.0, 1]},
          %{row: [170.0, 1]},
          %{row: [175.0, 1]},
          %{row: [180.0, 1]},
          %{row: [190.0, 1]}
        ]
      })
    end
  end

  describe "noisy counts are always positive" do
    setup do
      set_anon_params()
    end

    test "no-uid" do
      for i <- 1..50, do: :ok = insert_rows(_user_ids = (i * 2)..100, "anonymizations", ["number"], [i])

      assert_query("select count(number) from anonymizations where number between 1 and 100 group by number", %{
        rows: rows
      })

      assert Enum.all?(rows, fn %{row: [count]} -> count > 0 end)
    end

    test "no-uid distinct" do
      for i <- 1..50,
          do: :ok = insert_rows(_user_ids = (i * 2)..100, "anonymizations", ["number", "string"], [i, "#{rem(i, 5)}"])

      assert_query("select count(distinct number) from anonymizations group by string", %{
        rows: rows
      })

      assert Enum.all?(rows, fn %{row: [count]} -> count > 0 end)
    end

    test "uid" do
      for i <- 1..50, do: :ok = insert_rows(_user_ids = (i * 2)..100, "anonymizations", ["number"], [i])

      assert_query(
        "select count(number), stddev(number) from anonymizations where number between 1 and 100 group by number",
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
    for i <- 0..10, do: :ok = insert_rows(_user_ids = i..i, "anonymizations", ["number"], [i])

    assert_query(
      "select count_noise(*), count_noise(distinct user_id), count_noise(distinct number) from anonymizations",
      %{rows: [%{row: [1.0, 1.0, distinct_noise]}]}
    )

    assert_in_delta distinct_noise, 1.0, 0.01

    assert_query(
      """
        select count_noise(*), count_noise(distinct user_id), count_noise(distinct number)
        from anonymizations where number <> 0
      """,
      %{rows: [%{row: [c1, c2, c3]}]}
    )

    assert c1 > 1.0 and c2 > 1.0 and c3 > 0.7
  end

  test "stddev with large values" do
    :ok = insert_rows(_user_ids = 1..10, "anonymizations", ["number"], [1.7976931348623157e308])

    assert_query(
      "select stddev(number) from anonymizations",
      %{rows: [%{row: [nil]}]}
    )
  end

  test "uid-based sum with large values" do
    :ok = insert_rows(_user_ids = 1..1000, "anonymizations", ["number"], [1.7976931348623157e308])

    assert_query(
      "select sum(number), stddev(1) from anonymizations",
      %{rows: [%{row: [nil, 0.0]}]}
    )
  end

  test "stats-based sum with large values" do
    :ok = insert_rows(_user_ids = 1..1000, "anonymizations", ["number"], [3.4e38])

    assert_query(
      "select sum(number) from anonymizations",
      %{rows: [%{row: [result]}]}
    )

    assert is_number(result)
  end

  describe "extended grouping" do
    setup do
      :ok = insert_rows(_user_ids = 0..10, "anonymizations", ["number", "string"], [1, "aa"])
      :ok = insert_rows(_user_ids = 5..15, "anonymizations", ["number", "string"], [2, "ab"])
      :ok = insert_rows(_user_ids = 5..15, "anonymizations", ["number", "string"], [3, "ba"])
      :ok = insert_rows(_user_ids = 15..25, "anonymizations", ["number", "string"], [4, "bb"])
    end

    test "uid grouping sets" do
      # using `stddev` forces uid-based anonymization
      assert_query(
        """
          select
            left(string, 1), right(string, 1), count(distinct user_id), stddev(number)
          from anonymizations
          group by grouping sets ((1, 2), 1, 2, ())
        """,
        %{
          rows: [
            %{row: ["a", "a", 11, _]},
            %{row: ["a", "b", 11, _]},
            %{row: ["b", "a", 11, _]},
            %{row: ["b", "b", 11, _]},
            %{row: ["a", nil, 16, _]},
            %{row: ["b", nil, 21, _]},
            %{row: [nil, "a", 16, _]},
            %{row: [nil, "b", 21, _]},
            %{row: [nil, nil, 26, _]}
          ]
        }
      )
    end

    test "uid cube" do
      # using `stddev` forces uid-based anonymization
      assert_query(
        """
          select
            left(string, 1), right(string, 1), count(distinct user_id), stddev(number)
          from anonymizations
          group by cube (1, 2)
        """,
        %{
          rows: [
            %{row: ["a", "a", 11, _]},
            %{row: ["a", "b", 11, _]},
            %{row: ["b", "a", 11, _]},
            %{row: ["b", "b", 11, _]},
            %{row: ["a", nil, 16, _]},
            %{row: ["b", nil, 21, _]},
            %{row: [nil, "a", 16, _]},
            %{row: [nil, "b", 21, _]},
            %{row: [nil, nil, 26, _]}
          ]
        }
      )
    end

    test "no-uid grouping sets" do
      assert_query(
        """
          select
            left(string, 1), right(string, 1), count(distinct user_id)
          from anonymizations
          group by grouping sets ((1, 2), 1, 2, ())
        """,
        %{
          rows: [
            %{row: ["a", "a", 11]},
            %{row: ["a", "b", 11]},
            %{row: ["b", "a", 11]},
            %{row: ["b", "b", 11]},
            %{row: ["a", nil, 16]},
            %{row: ["b", nil, 21]},
            %{row: [nil, "a", 16]},
            %{row: [nil, "b", 21]},
            %{row: [nil, nil, 26]}
          ]
        }
      )
    end

    test "no-uid cube" do
      assert_query(
        """
          select
            left(string, 1), right(string, 1), count(distinct user_id)
          from anonymizations
          group by cube (1, 2)
        """,
        %{
          rows: [
            %{row: ["a", "a", 11]},
            %{row: ["a", "b", 11]},
            %{row: ["b", "a", 11]},
            %{row: ["b", "b", 11]},
            %{row: ["a", nil, 16]},
            %{row: ["b", nil, 21]},
            %{row: [nil, "a", 16]},
            %{row: [nil, "b", 21]},
            %{row: [nil, nil, 26]}
          ]
        }
      )
    end

    test "simple no-uid grouping sets with distinct" do
      assert_query(
        """
          select
            string, count(distinct number)
          from anonymizations
          group by grouping sets (1, ())
        """,
        %{
          rows: [
            %{row: ["aa", 1]},
            %{row: ["ab", 1]},
            %{row: ["ba", 1]},
            %{row: ["bb", 1]},
            %{row: [nil, 4]}
          ]
        }
      )
    end

    test "complex no-uid grouping sets with distinct" do
      assert_query(
        """
          select
            left(string, 1), right(string, 1), count(distinct user_id), count(distinct number)
          from anonymizations
          group by grouping sets ((1, 2), 1, 2, ())
        """,
        %{
          rows: [
            %{row: ["a", "a", 11, 1]},
            %{row: ["a", "b", 11, 1]},
            %{row: ["b", "a", 11, 1]},
            %{row: ["b", "b", 11, 1]},
            %{row: ["a", nil, 16, 2]},
            %{row: ["b", nil, 21, 2]},
            %{row: [nil, "a", 16, 2]},
            %{row: [nil, "b", 21, 2]},
            %{row: [nil, nil, 26, 4]}
          ]
        }
      )
    end
  end

  describe "using grouping sets doesn't change the group noise" do
    setup do
      :ok = insert_rows(_user_ids = 0..10, "anonymizations", ["number", "string"], [1, "aa"])
      :ok = insert_rows(_user_ids = 5..15, "anonymizations", ["number", "string"], [1, "ab"])
      :ok = insert_rows(_user_ids = 5..15, "anonymizations", ["number", "string"], [1, "ba"])
      :ok = insert_rows(_user_ids = 15..25, "anonymizations", ["number", "string"], [1, "bb"])

      set_anon_params()
    end

    test "simple uid anon" do
      assert_query(
        """
          select count(*), stddev(0) from anonymizations
        """,
        %{
          rows: [
            %{row: [count, _]}
          ]
        }
      )

      assert_query(
        """
          select left(string, 1), count(*), stddev(0)
          from anonymizations
          group by grouping sets (1, ())
          order by 1
        """,
        %{
          rows: [
            %{row: ["a", _, _]},
            %{row: ["b", _, _]},
            %{row: [nil, ^count, _]}
          ]
        }
      )
    end

    test "uid anon with filter" do
      assert_query(
        """
          select count(*), stddev(0) from anonymizations where number = 1
        """,
        %{
          rows: [
            %{row: [count, _]}
          ]
        }
      )

      assert_query(
        """
          select left(string, 1), count(*), stddev(0)
          from anonymizations
          where number = 1
          group by grouping sets (1, ())
          order by 1
        """,
        %{
          rows: [
            %{row: ["a", _, _]},
            %{row: ["b", _, _]},
            %{row: [nil, ^count, _]}
          ]
        }
      )
    end

    test "simple stats anon" do
      assert_query(
        """
          select count(*) from anonymizations
        """,
        %{
          rows: [
            %{row: [count]}
          ]
        }
      )

      assert_query(
        """
          select left(string, 1), count(*)
          from anonymizations
          group by grouping sets (1, ())
          order by 1
        """,
        %{
          rows: [
            %{row: ["a", _]},
            %{row: ["b", _]},
            %{row: [nil, ^count]}
          ]
        }
      )
    end

    test "stats anon with filter" do
      assert_query(
        """
          select count(*) from anonymizations where number = 1
        """,
        %{
          rows: [
            %{row: [count]}
          ]
        }
      )

      assert_query(
        """
          select left(string, 1), count(*)
          from anonymizations
          where number = 1
          group by grouping sets (1, ())
          order by 1
        """,
        %{
          rows: [
            %{row: ["a", _]},
            %{row: ["b", _]},
            %{row: [nil, ^count]}
          ]
        }
      )
    end

    test "distinct stats anon" do
      assert_query(
        """
          select count(distinct number) from anonymizations
        """,
        %{
          rows: [
            %{row: [count]}
          ]
        }
      )

      assert_query(
        """
          select left(string, 1), count(distinct number)
          from anonymizations
          group by grouping sets (1, ())
          order by 1
        """,
        %{
          rows: [
            %{row: ["a", _]},
            %{row: ["b", _]},
            %{row: [nil, ^count]}
          ]
        }
      )
    end
  end

  describe "case" do
    setup do
      :ok = insert_rows(_user_ids = 0..9, "anonymizations", ["number"], [1])
      :ok = insert_rows(_user_ids = 10..19, "anonymizations", ["number"], [2])
      :ok = insert_rows(_user_ids = 20..29, "anonymizations", ["number"], [3])
    end

    test "uid bucketed" do
      # using `stddev` forces uid-based anonymization
      assert_query(
        """
          select case when number = 1 then 1 when number = 2 then 2 end, count(*), stddev(0)
          from anonymizations group by 1 order by 1
        """,
        %{
          rows: [
            %{row: [1, 10, _]},
            %{row: [2, 10, _]},
            %{row: [nil, 10, _]}
          ]
        }
      )
    end

    test "stats bucketed" do
      assert_query(
        """
          select case when number = 1 then 1 when number = 2 then 2 end, count(*)
          from anonymizations group by 1 order by 1
        """,
        %{
          rows: [
            %{row: [1, 10]},
            %{row: [2, 10]},
            %{row: [nil, 10]}
          ]
        }
      )
    end

    test "uid aggregated" do
      # using `stddev` forces uid-based anonymization
      assert_query(
        "select sum(case when number = 1 then 1 end), stddev(0) from anonymizations",
        %{rows: [%{row: [10, _]}]}
      )
    end

    test "stats aggregated" do
      assert_query(
        "select sum(case when number = 1 then 1 end) from anonymizations",
        %{rows: [%{row: [10]}]}
      )
    end
  end

  describe "case noise" do
    setup do
      :ok = insert_rows(_user_ids = 1..20, "anonymizations", ["number"], [1])
      :ok = insert_rows(_user_ids = 15..25, "anonymizations", ["number"], [2])

      set_anon_params()
    end

    test "uid" do
      # using `stddev` forces uid-based anonymization

      assert_query(
        "select count(1), stddev(0) from anonymizations where number = 1",
        %{rows: [%{row: [count, _]}]}
      )

      assert_query(
        "select case when number = 1 then 1 end, count(*), stddev(0) from anonymizations group by 1 order by 1",
        %{rows: [%{row: [1, ^count, _]}, %{row: [nil, _, _]}]}
      )

      assert_query(
        "select count(case when number = 1 then 1 end), stddev(0) from anonymizations",
        %{rows: [%{row: [^count, _]}]}
      )

      assert_query(
        """
          select number, count(case when number = 1 then 1 end), stddev(0)
          from anonymizations group by grouping sets ((), number)
        """,
        %{rows: [%{row: [nil, ^count, _]}, %{row: [1.0, ^count, _]}, %{row: [2.0, _, _]}]}
      )
    end

    test "stats" do
      assert_query(
        "select count(case when number = 1 then 1 end) from anonymizations",
        %{rows: [%{row: [count]}]}
      )

      assert_query(
        "select count(1) from anonymizations where number = 1",
        %{rows: [%{row: [^count]}]}
      )

      assert_query(
        "select case when number = 1 then 1 end, count(*) from anonymizations group by 1 order by 1",
        %{rows: [%{row: [1, ^count]}, %{row: [nil, _]}]}
      )

      assert_query(
        """
          select number, count(case when number = 1 then 1 end)
          from anonymizations group by grouping sets ((), number)
        """,
        %{rows: [%{row: [nil, ^count]}, %{row: [1.0, ^count]}, %{row: [2.0, _]}]}
      )
    end
  end
end
