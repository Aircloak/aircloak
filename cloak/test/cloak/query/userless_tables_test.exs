defmodule Cloak.Query.UserlessTableTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("userless", "i INTEGER, name TEXT", user_id: nil, add_user_id: false)
    :ok = insert_rows("userless", ["i", "name"], [1, "car"])
    :ok = insert_rows("userless", ["i", "name"], [2, "food"])
    :ok = insert_rows("userless", ["i", "name"], [2, "drinks"])
    :ok = insert_rows("userless", ["i", "name"], [3, "fun"])

    :ok = Cloak.Test.DB.create_table("userless_join", "i INTEGER")
    :ok = insert_rows(_user_ids = 1..10, "userless_join", ["i"], [1])
    :ok = insert_rows(_user_ids = 11..20, "userless_join", ["i"], [2])
    :ok = insert_rows(_user_ids = 21..30, "userless_join", ["i"], [3])

    :ok
  end

  test "simple select" do
    assert_query("select name from userless order by name", %{
      rows: [%{row: ["car"]}, %{row: ["drinks"]}, %{row: ["food"]}, %{row: ["fun"]}]
    })
  end

  test "select with where filter" do
    assert_query("select name from userless where name in ('fun', 'car') order by name", %{
      rows: [%{row: ["car"]}, %{row: ["fun"]}]
    })
  end

  test "select with having filter" do
    assert_query("select name from userless group by name having name like 'ca_'", %{
      rows: [%{row: ["car"]}]
    })
  end

  test "select from subquery" do
    assert_query("select name from (select name from userless order by name limit 2 offset 1) as t", %{
      rows: [%{row: ["drinks"]}, %{row: ["food"]}]
    })
  end

  test "join between userless tables" do
    assert_query(
      "select t1.name from userless as t1 join userless as t2 on t1.i = t2.i and t1.name <> t2.name order by 1",
      %{rows: [%{row: ["drinks"]}, %{row: ["food"]}]}
    )
  end

  test "error on join between user and userless tables" do
    assert_query(
      "select count(*) from userless as t1 join userless_join as t2 on t1.i = t2.i",
      %{error: "The tables `t1` and `t2` are not joined " <> _}
    )
  end

  test "join between userless tables with inequality filter in the ON clause" do
    assert_query(
      "select count(*) from userless as t1 join userless as t2 on false < true",
      %{rows: [%{row: [16]}]}
    )
  end

  test "join between anonymizing and standard query with constant equality filter in the ON clause" do
    assert_query(
      """
        select * from
          (select count(*) from userless) as t1
        join
          (select count(*) from userless_join) as t2
        on true = true
      """,
      %{rows: [%{row: [4, 30]}]}
    )
  end

  test "standard query with sample users" do
    assert_query(
      "select name from userless sample_users 10%",
      %{error: "The `SAMPLE_USERS` clause is not valid in standard queries."}
    )
  end

  test "[Issue #3070] ordering a userless query by a constant" do
    assert_query(
      """
        SELECT *
        FROM
          (
            SELECT 1 AS x
            FROM (SELECT count(*) FROM userless_join) AS foo
            ORDER BY 1
          ) AS bar
      """,
      %{rows: [_ | _]}
    )
  end

  test "selecting complex expressions with identical names from userless table" do
    assert_query("select count(i), count(*) from userless", %{
      rows: [%{row: [4, 4]}]
    })
  end

  describe "*_noise" do
    for function <- ~w(count_noise avg_noise stddev_noise sum_noise variance_noise) do
      test "using #{function} in standard query" do
        assert_query("select #{unquote(function)}(i) from userless", %{
          rows: [%{row: [0.0]}]
        })
      end
    end
  end

  test "[optimizer bug] subquery with unused column" do
    assert_query("select count(*) from (select i from userless) t", %{
      rows: [%{row: [4]}]
    })
  end

  describe "extended group by clauses" do
    test "empty group by" do
      assert_query("select count(*) from userless group by ()", %{
        rows: [%{row: [4]}]
      })
    end

    test "simple grouping sets" do
      assert_query("select i, count(*) from userless group by grouping sets ((i), ()) order by 1", %{
        rows: [%{row: [1, 1]}, %{row: [2, 2]}, %{row: [3, 1]}, %{row: [nil, 4]}]
      })
    end

    test "complex grouping sets" do
      assert_query(
        "select i, name, count(*) from userless group by grouping sets ((i, name), i, name) order by 1, 2",
        %{
          rows: [
            %{row: [1, "car", 1]},
            %{row: [1, nil, 1]},
            %{row: [2, "drinks", 1]},
            %{row: [2, "food", 1]},
            %{row: [2, nil, 2]},
            %{row: [3, "fun", 1]},
            %{row: [3, nil, 1]},
            %{row: [nil, "car", 1]},
            %{row: [nil, "drinks", 1]},
            %{row: [nil, "food", 1]},
            %{row: [nil, "fun", 1]}
          ]
        }
      )
    end

    test "rollup" do
      assert_query(
        "select i, name, count(*) from userless group by rollup (i, name) order by 1, 2",
        %{
          rows: [
            %{row: [1, "car", 1]},
            %{row: [1, nil, 1]},
            %{row: [2, "drinks", 1]},
            %{row: [2, "food", 1]},
            %{row: [2, nil, 2]},
            %{row: [3, "fun", 1]},
            %{row: [3, nil, 1]},
            %{row: [nil, nil, 4]}
          ]
        }
      )
    end
  end
end
