defmodule Cloak.Query.BasicTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights", "height INTEGER, name TEXT, male BOOLEAN, weight INTEGER")
    :ok = Cloak.Test.DB.create_table("heights_alias", nil, skip_db_create: true, db_name: "heights")

    :ok =
      Cloak.Test.DB.create_table("heights_query", nil,
        skip_db_create: true,
        query: "SELECT DISTINCT user_id, height FROM heights"
      )

    :ok = Cloak.Test.DB.create_table("children", "age INTEGER, name TEXT")
    :ok = Cloak.Test.DB.create_table("weird things", "\"thing as thing\" INTEGER", db_name: "weird-things")
    :ok = Cloak.Test.DB.create_table("dotted.table", "\"dotted.column\" INTEGER", db_name: "dotted-table")
    :ok = Cloak.Test.DB.create_table("dates", "date timestamp, date2 timestamp")

    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("heights")
    Cloak.Test.DB.clear_table("children")
    Cloak.Test.DB.clear_table("weird-things")
    Cloak.Test.DB.clear_table("dotted-table")
    Cloak.Test.DB.clear_table("dates")
    :ok
  end

  test "show tables and views" do
    assert_query("show tables", [views: %{"v1" => "select user_id, height from heights"}], %{
      columns: ["name", "type", "comment"],
      rows: rows
    })

    rows = Enum.map(rows, & &1.row)

    [
      ["children", "personal", ""],
      ["heights", "personal", ""],
      ["weird things", "personal", ""],
      ["dotted.table", "personal", ""],
      ["dates", "personal", ""],
      ["v1", "view", ""]
    ]
    |> Enum.each(&assert(Enum.member?(rows, &1)))
  end

  test "show columns" do
    :ok = Cloak.Test.DB.create_table("basic_isolators", "isolates INTEGER, regular TEXT, pending BOOLEAN")

    for data_source <- Cloak.DataSource.all() do
      Cloak.TestIsolatorsCache.register_isolator(data_source, "basic_isolators", "isolates")
      Cloak.TestIsolatorsCache.register_pending(data_source, "basic_isolators", "pending")
    end

    assert_query("show columns from basic_isolators", %{
      columns: ["name", "data type", "isolator?", "key type", "comment"],
      rows: rows
    })

    assert Enum.sort_by(rows, & &1[:row]) == [
             %{occurrences: 1, row: ["isolates", "integer", "true", nil, ""]},
             %{occurrences: 1, row: ["pending", "boolean", "pending", nil, ""]},
             %{occurrences: 1, row: ["regular", "text", "false", nil, ""]},
             %{occurrences: 1, row: ["user_id", "text", "false", :user_id, ""]}
           ]
  end

  test "show columns using the `public.` prefix for a table" do
    assert_query("show columns from public.heights", %{columns: _, rows: _})
    assert_query(~s/show columns from "public"."heights"/, %{columns: _, rows: _})
    assert_query(~s/show columns from "public.heights"/, %{columns: _, rows: _})
  end

  test "show columns from a view" do
    assert_query("show columns from v1", [views: %{"v1" => "select user_id, height from heights"}], %{
      query_id: "1",
      columns: _,
      rows: rows
    })

    assert Enum.sort_by(rows, & &1[:row]) == [
             %{occurrences: 1, row: ["height", "integer", nil, nil, ""]},
             %{occurrences: 1, row: ["user_id", "text", nil, :user_id, ""]}
           ]
  end

  test "simple select query" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select height from heights", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "select no columns" do
    :ok = insert_rows(_user_ids = 1..10, "heights", [], [])

    assert_query("select from heights", %{
      columns: [],
      rows: [%{row: [], occurrences: 10}]
    })
  end

  test "select no columns with group by" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [100])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["height"], [200])

    assert_query("select from heights group by height", %{
      columns: [],
      rows: [
        %{row: [], occurrences: 1},
        %{row: [], occurrences: 1}
      ]
    })
  end

  test "select using the `public.` prefix for a table" do
    assert_query("select height from public.heights", %{columns: ["height"], rows: _})
    assert_query(~s/select height from "public"."heights"/, %{columns: ["height"], rows: _})
    assert_query(~s/select height from "public.heights"/, %{columns: ["height"], rows: _})
  end

  test "rows with null user_ids are ignored" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = insert_null_uid_row("heights", ["height"], [180])

    assert_query("select height from heights", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "identifiers are case-insensitive" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select HeiGht, Heights.heighT from heIghts", %{
      columns: ["HeiGht", "heighT"],
      rows: [%{row: [180, 180], occurrences: 100}]
    })
  end

  test "select a constant" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])
    assert_query("select 3 from heights", %{columns: [""], rows: [%{occurrences: 10, row: [3]}]})
  end

  test "null in an expression" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])
    assert_query("select height + null from heights", %{rows: [%{occurrences: 10, row: [nil]}]})
  end

  test "select an aliased constant" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])

    assert_query("select 'text' as the_text from heights", %{
      columns: ["the_text"],
      rows: [%{occurrences: 10, row: ["text"]}]
    })
  end

  test "a binary function of two columns" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [2])

    assert_query("select pow(height, height) from heights", %{
      columns: ["pow"],
      rows: [%{occurrences: 10, row: [4.0]}]
    })
  end

  test "order by non-selected field" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query("select height from heights order by name", %{
      query_id: "1",
      columns: ["height"],
      rows: rows
    })

    assert Enum.map(rows, & &1[:row]) == [[170], [160], [180]]
  end

  test "order by grouped but non-selected field" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query("select sum(height) from heights group by name order by name", %{
      query_id: "1",
      columns: ["sum"],
      rows: rows
    })

    assert Enum.map(rows, & &1[:row]) == [[nil], [1600], [1800]]
  end

  test "order by grouped but non-selected aggregate" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query("select name from heights group by name order by sum(height) desc", %{
      query_id: "1",
      columns: ["name"],
      rows: rows
    })

    assert Enum.map(rows, & &1[:row]) == [["adam"], ["mike"], ["john"]]
  end

  test "order by grouped but non-selected aggregate with selected aggregate function" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 31..60, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query("select bucket(height by 10), avg(height) from heights group by 1 order by count(*) desc", %{
      query_id: "1",
      columns: ["bucket", "avg"],
      rows: rows
    })

    assert Enum.map(rows, & &1[:row]) == [[180.0, 180.0], [170.0, 170.0], [160.0, nil]]
  end

  test "order by nulls first" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["height"], [170])

    assert_query("select height from heights order by 1 asc nulls first", %{
      rows: [%{row: [nil]}, %{row: [170]}]
    })
  end

  test "order by nulls directive in subquery" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["height"], [170])

    assert_query("select height from (select height from heights order by 1 asc nulls first limit 10) foo", %{
      rows: [%{row: [nil]}]
    })
  end

  test "order by complex non-grouped column" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "name"], [170, "aaa"])

    assert_query("select 1 from heights group by name order by length(name) - height", %{
      error:
        "Column `height` from table `heights` needs to appear in the `GROUP BY` clause or be used in an aggregate function." <>
          _
    })
  end

  test "order by count distinct uids" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["john"])

    assert_query("select count(distinct user_id) from heights order by 1", %{rows: [%{row: [10]}]})
  end

  test "should return LCF property when sufficient rows are filtered" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..3, "heights", ["height"], [160])
    :ok = insert_rows(_user_ids = 20..23, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..23, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 24..27, "heights", ["height"], [200])
    :ok = insert_rows(_user_ids = 24..27, "heights", ["height"], [150])

    assert_query("select height from heights order by height", %{
      columns: ["height"],
      rows: [
        %{row: [180], occurrences: 20},
        %{row: [:*], occurrences: 24}
      ]
    })
  end

  test "count(*)" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [nil])

    assert_query("select count(*) from heights", %{
      columns: ["count"],
      rows: [%{row: [40], occurrences: 1}]
    })
  end

  test "global aggregators over empty input" do
    assert_query(
      "select 1 + count(*) from heights",
      %{rows: [%{row: [1]}]}
    )
  end

  test "count(column)" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [nil])

    assert_query("select COUNT(height) from heights", %{
      columns: ["count"],
      rows: [%{row: [20], occurrences: 1}]
    })
  end

  test "count(distinct column) for empty sets" do
    assert_query("select count(distinct height) from heights", %{
      columns: ["count"],
      rows: [%{row: [0], occurrences: 1}]
    })
  end

  test "distinct column" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [175])

    assert_query("select distinct height from heights order by height", %{
      columns: ["height"],
      rows: [
        %{row: [170], occurrences: 1},
        %{row: [175], occurrences: 1},
        %{row: [180], occurrences: 1}
      ]
    })
  end

  test "aggregates of an empty table" do
    assert_query("select count(*), count(height), avg(height) from heights", %{
      columns: ["count", "count", "avg"],
      rows: [%{row: [0, 0, nil], occurrences: 1}]
    })
  end

  describe "aggregating positive values" do
    setup do
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
      :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
      :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])
    end

    test "sum" do
      assert_query("select sum(height) from heights", %{
        columns: ["sum"],
        rows: [%{row: [5400], occurrences: 1}]
      })
    end

    test "min" do
      assert_query("select min(height) from heights", %{
        columns: ["min"],
        rows: [%{row: [163], occurrences: 1}]
      })
    end

    test "max" do
      assert_query("select max(height) from heights", %{
        columns: ["max"],
        rows: [%{row: [197], occurrences: 1}]
      })
    end

    test "avg statistics" do
      assert_query("select avg(height) from heights", %{
        columns: ["avg"],
        rows: [%{row: [avg], occurrences: 1}]
      })

      assert_in_delta(avg, 178, 182)
    end

    test "avg uid" do
      assert_query("select avg(height), stddev(height) from heights", %{
        columns: ["avg", _],
        rows: [%{row: [avg, _], occurrences: 1}]
      })

      assert_in_delta(avg, 178, 182)
    end

    test "stddev uid" do
      assert_query("select stddev(height), stddev(height) from heights", %{
        columns: ["stddev", _],
        rows: [%{row: [stddev, _], occurrences: 1}]
      })

      assert_in_delta(stddev, 8.1, 0.1)
    end

    test "stddev statistics" do
      assert_query("select stddev(height) from heights", %{
        columns: ["stddev"],
        rows: [%{row: [stddev], occurrences: 1}]
      })

      assert_in_delta(stddev, 8.1, 0.2)
    end

    test "variance uid" do
      assert_query("select variance(height), stddev(height) from heights", %{
        columns: ["variance", _],
        rows: [%{row: [variance, _], occurrences: 1}]
      })

      # comparing stddev (sqrt(variance)) to match the stddev test above
      assert_in_delta(:math.sqrt(variance), 8.1, 0.1)
    end

    test "variance statistics" do
      assert_query("select variance(height) from heights", %{
        columns: ["variance"],
        rows: [%{row: [variance], occurrences: 1}]
      })

      # comparing stddev (sqrt(variance)) to match the stddev test above
      assert_in_delta(:math.sqrt(variance), 8.1, 0.2)
    end

    test "sum(qualified_column)" do
      assert_query("select sum(heights.height) from heights", %{
        columns: ["sum"],
        rows: [%{row: [5400], occurrences: 1}]
      })
    end

    test "min(qualified_column)" do
      assert_query("select min(heights.height) from heights", %{
        columns: ["min"],
        rows: [%{row: [163], occurrences: 1}]
      })
    end

    test "max(qualified_column)" do
      assert_query("select max(heights.height) from heights", %{
        columns: ["max"],
        rows: [%{row: [197], occurrences: 1}]
      })
    end

    test "avg(qualified_column)" do
      assert_query("select avg(heights.height) from heights", %{
        columns: ["avg"],
        rows: [%{row: [avg], occurrences: 1}]
      })

      assert_in_delta(avg, 178, 182)
    end

    test "stddev(qualified_column)" do
      assert_query("select stddev(heights.height) from heights", %{
        columns: ["stddev"],
        rows: [%{row: [stddev], occurrences: 1}]
      })

      assert_in_delta(stddev, 8.1, 0.2)
    end

    test "variance(qualified_column)" do
      assert_query("select variance(heights.height) from heights", %{
        columns: ["variance"],
        rows: [%{row: [variance], occurrences: 1}]
      })

      # comparing stddev (sqrt(variance)) to match the stddev test above
      assert_in_delta(:math.sqrt(variance), 8.1, 0.2)
    end
  end

  describe "aggregating negative values" do
    setup do
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
      :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [-170])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [-190])
      :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [-183])
    end

    test "sum" do
      assert_query("select sum(height) from heights", %{
        columns: ["sum"],
        rows: [%{row: [-5429], occurrences: 1}]
      })
    end

    test "min" do
      assert_query("select min(height) from heights", %{
        columns: ["min"],
        rows: [%{row: [-196], occurrences: 1}]
      })
    end

    test "max" do
      assert_query("select max(height) from heights", %{
        columns: ["max"],
        rows: [%{row: [-162], occurrences: 1}]
      })
    end

    test "avg" do
      assert_query("select avg(height) from heights", %{
        columns: ["avg"],
        rows: [%{row: [avg], occurrences: 1}]
      })

      assert_in_delta(avg, -180, 5)
    end

    test "stddev" do
      assert_query("select stddev(height) from heights", %{
        columns: ["stddev"],
        rows: [%{row: [stddev], occurrences: 1}]
      })

      assert_in_delta(stddev, 8.29, 0.1)
    end

    test "variance" do
      assert_query("select variance(height) from heights", %{
        columns: ["variance"],
        rows: [%{row: [variance], occurrences: 1}]
      })

      # comparing stddev (sqrt(variance)) to match the stddev test above
      assert_in_delta(:math.sqrt(variance), 8.29, 0.1)
    end
  end

  describe "aggregating negative and positive values" do
    setup do
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
      :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [-175])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [-190])
      :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])
    end

    test "sum" do
      assert_query("select sum(height) from heights", %{
        columns: ["sum"],
        rows: [%{row: [-1750], occurrences: 1}]
      })
    end

    test "min" do
      assert_query("select min(height) from heights", %{
        columns: ["min"],
        rows: [%{row: [-303], occurrences: 1}]
      })
    end

    test "max" do
      assert_query("select max(height) from heights", %{
        columns: ["max"],
        rows: [%{row: [393], occurrences: 1}]
      })
    end

    test "avg" do
      assert_query("select avg(height) from heights", %{
        columns: ["avg"],
        rows: [%{row: [avg], occurrences: 1}]
      })

      assert_in_delta(avg, -60, 2)
    end

    test "stddev" do
      assert_query("select stddev(height) from heights", %{
        columns: ["stddev"],
        rows: [%{row: [stddev], occurrences: 1}]
      })

      assert_in_delta(stddev, 171, 1)
    end

    test "variance" do
      assert_query("select variance(height) from heights", %{
        columns: ["variance"],
        rows: [%{row: [variance], occurrences: 1}]
      })

      # comparing stddev (sqrt(variance)) to match the stddev test above
      assert_in_delta(:math.sqrt(variance), 171, 1)
    end
  end

  test "should return nil when not enough values present for anonymization" do
    :ok = insert_rows(_user_ids = 0..3, "heights", ["height"], [180])

    assert_query("select stddev(height) from heights", %{
      columns: ["stddev"],
      rows: [%{row: [nil], occurrences: 1}]
    })
  end

  test "select the same column multiple times" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select height, height from heights", %{
      columns: ["height", "height"],
      rows: [%{row: [180, 180], occurrences: 100}]
    })
  end

  test "select the same aggregate multiple times" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select count(height), count(*), count(*), count(height) from heights", %{
      columns: ["count", "count", "count", "count"],
      rows: [%{row: [100, 100, 100, 100], occurrences: 1}]
    })
  end

  test "different aggregates on the same column" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select count(height), max(height) from heights", %{
      columns: ["count", "max"],
      rows: [%{row: [100, 180], occurrences: 1}]
    })
  end

  describe "inequalities" do
    setup do
      :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "weight"], [170, 10])
      :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "weight"], [180, 200])
      :ok = insert_rows(_user_ids = 20..39, "heights", ["height", "weight"], [190, 200])
    end

    test "col1 > col2" do
      assert_query("select count(*) from heights where height > weight", %{
        columns: ["count"],
        rows: [%{row: [20], occurrences: 1}]
      })
    end

    test "col1 BETWEEN col2 AND col3" do
      assert_query("select count(*) from heights where height BETWEEN height AND weight", %{
        columns: ["count"],
        rows: [%{row: [40], occurrences: 1}]
      })
    end

    test "should allow ranges in where clause" do
      assert_query("select count(*) from heights where height >= 180 and height < 190", %{
        columns: ["count"],
        rows: [%{row: [20], occurrences: 1}]
      })
    end

    test "should allow between in where clause" do
      assert_query("select count(*) from heights where height between 180 and 190", %{
        columns: ["count"],
        rows: [%{row: [20], occurrences: 1}]
      })
    end

    test "should allow reversed inequalities in where clause" do
      assert_query("select count(*) from heights where 180 <= height and 190 > height", %{
        columns: ["count"],
        rows: [%{row: [20], occurrences: 1}]
      })
    end

    test "should allow reversed inequalities with complex constant expresions in where clause" do
      assert_query("select count(*) from heights where 179 + 1 <= height and abs(190) > height", %{
        columns: ["count"],
        rows: [%{row: [20], occurrences: 1}]
      })
    end

    test "constant expressions in inequalities" do
      assert_query("select count(*) from heights where height >= 90 * 2 and height < 190", %{
        columns: ["count"],
        rows: [%{row: [20], occurrences: 1}]
      })
    end
  end

  test "should allow LIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "bob"])

    assert_query("select count(*) from heights where name LIKE 'b%'", %{
      columns: ["count"],
      rows: [%{row: [20], occurrences: 1}]
    })
  end

  test "should allow NOT LIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "bob"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "alice"])

    assert_query("select count(*) from heights where name NOT LIKE 'b%'", %{
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "should allow NOT ILIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "Bob"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "alice"])

    assert_query("select count(*) from heights where name NOT ILIKE 'b%'", %{
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "should allow ILIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "Bob"])

    assert_query("select count(*) from heights where name ILIKE 'b%'", %{
      columns: ["count"],
      rows: [%{row: [20], occurrences: 1}]
    })
  end

  test "should handle escaped chars in LIKE conditions" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "b_%"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "bob"])

    assert_query("select count(*) from heights where name LIKE 'b~_~%%' ESCAPE '~'", %{
      columns: ["count"],
      rows: [%{row: [20], occurrences: 1}]
    })
  end

  test "should allow IN in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query("select count(*) from heights where height IN (170, 180, 190)", %{
      columns: ["count"],
      rows: [%{row: [60], occurrences: 1}]
    })
  end

  test "should allow NOT IN in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query("select count(*) from heights where height NOT IN (170, 190)", %{
      columns: ["count"],
      rows: [%{row: [20], occurrences: 1}]
    })
  end

  test "should allow <> in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 40..49, "heights", ["height"], [nil])

    assert_query("select count(*) from heights where height <> 180", %{
      columns: ["count"],
      rows: [%{row: [40], occurrences: 1}]
    })
  end

  test "<> conditions count unique users" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["name"], ["Alice"])

    1..10
    |> Enum.each(fn _ -> :ok = insert_rows(_user_ids = [10], "heights", ["name"], ["Bob"]) end)

    assert_query("select count(*) from heights where name <> 'Bob'", %{
      query_id: "1",
      columns: ["count"],
      rows: [%{row: [20], occurrences: 1}]
    })
  end

  test "should allow IS NULL in where clause" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [180])

    assert_query("select count(*) from heights where height IS NULL", %{
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "should allow IS NOT NULL in where clause" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [180])

    assert_query("select count(*) from heights where height IS NOT NULL", %{
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "select and filter booleans" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 180, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["eva", 160, false])

    assert_query("select height, male from heights where male = true", %{
      query_id: "1",
      columns: ["height", "male"],
      rows: [%{row: [180, true], occurrences: 10}]
    })
  end

  test "select with constant filter" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 180, true])

    assert_query("select height, male from heights where true = true", %{
      query_id: "1",
      columns: ["height", "male"],
      rows: [%{row: [180, true], occurrences: 10}]
    })
  end

  test "should order rows when instructed" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])

    assert_query("select height from heights order by height", %{
      query_id: "1",
      columns: ["height"],
      rows: rows
    })

    assert rows == Enum.sort(rows)
  end

  test "should order dates sensibly" do
    dates = [
      {2014, 09, 01},
      {2014, 10, 01},
      {2014, 11, 01},
      {2014, 12, 01},
      {2015, 01, 01},
      {2015, 02, 01},
      {2015, 03, 01},
      {2015, 04, 01},
      {2015, 05, 01},
      {2015, 06, 01},
      {2015, 07, 01},
      {2015, 08, 01},
      {2015, 09, 01},
      {2015, 10, 01},
      {2015, 11, 01},
      {2015, 12, 01}
    ]

    dates =
      for {year, month, day} <- dates do
        date = %NaiveDateTime{
          year: year,
          month: month,
          day: day,
          hour: 0,
          minute: 0,
          second: 0,
          microsecond: {0, 6}
        }

        :ok = insert_rows(_user_ids = 0..100, "dates", ["date"], [date])
        NaiveDateTime.to_iso8601(date)
      end

    assert_query("select date from dates group by date order by date asc", %{
      query_id: "1",
      columns: ["date"],
      rows: rows
    })

    assert for(data <- rows, do: hd(data.row)) == dates

    assert_query("select date from dates group by date order by date desc", %{
      query_id: "1",
      columns: ["date"],
      rows: rows
    })

    assert for(data <- rows, do: hd(data.row)) == Enum.reverse(dates)
  end

  test "intervals in query results" do
    :ok = insert_rows(_user_ids = 0..4, "dates", ["date"], [~N[2017-01-02 00:01:02]])

    assert_query("select date - date, interval 'P1Y2M' from dates", %{
      rows: [%{row: ["P", "P1Y2M"]}]
    })
  end

  test "query with empty interval" do
    :ok = insert_rows(_user_ids = 0..4, "dates", ["date"], [~N[2017-01-02 00:01:02]])

    assert_query("select count(*) from dates where date - date = interval 'P'", %{
      rows: [%{row: [5]}]
    })
  end

  test "query allows mixing aggregated and grouped columns" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query("select count(*), height from heights group by height", %{
      columns: ["count", "height"],
      rows: rows
    })

    assert [%{row: [10, 180], occurrences: 1}, %{row: [20, 160], occurrences: 1}] = Enum.sort_by(rows, & &1[:row])
  end

  test "grouping works when the column is not selected" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query("select count(*) from heights group by height", %{columns: ["count"], rows: rows})

    assert [%{row: [10], occurrences: 1}, %{row: [20], occurrences: 1}] = Enum.sort_by(rows, & &1[:row])
  end

  test "grouping and sorting by a count" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query("select count(*), height from heights group by height order by count(*) asc", %{
      columns: ["count", "height"],
      rows: rows
    })

    assert [
             %{row: [10, 180], occurrences: 1},
             %{row: [20, 160], occurrences: 1},
             %{row: [30, 150], occurrences: 1}
           ] = rows
  end

  test "ordering hidden values" do
    :ok = insert_rows(_user_ids = 0..2, "heights", ["name"], ["Alice"])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["name"], ["Charlie"])
    :ok = insert_rows(_user_ids = 3..6, "heights", ["name"], ["John"])
    :ok = insert_rows(_user_ids = 7..9, "heights", ["name"], ["Bob"])

    assert_query("select count(*), name from heights group by name order by name asc", %{
      columns: ["count", "name"],
      rows: rows
    })

    assert [%{row: [10, "Charlie"], occurrences: 1}, %{row: [11, :*], occurrences: 1}] = rows
  end

  test "grouping and sorting by a grouped field" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query("select count(*), height from heights group by height order by height asc", %{
      columns: ["count", "height"],
      rows: rows
    })

    assert [
             %{row: [30, 150], occurrences: 1},
             %{row: [20, 160], occurrences: 1},
             %{row: [10, 180], occurrences: 1}
           ] = rows
  end

  test "query which returns zero rows" do
    Cloak.Test.DB.clear_table("heights")
    assert_query("select height from heights", %{query_id: "1", columns: ["height"], rows: []})
  end

  describe "alias usage" do
    setup do
      :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
      :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [180])
    end

    test "select with column alias" do
      assert_query("select height as h from heights group by h order by h", %{
        columns: ["h"],
        rows: [%{row: [170], occurrences: 1}, %{row: [180], occurrences: 1}]
      })
    end

    test "select with duplicated alias" do
      assert_query("select count(*) as c, count(height) as c from heights", %{
        columns: ["c", "c"],
        rows: [%{row: [30, 30], occurrences: 1}]
      })
    end

    test "alias usage in where" do
      assert_query("select height as h from heights where h = 170", %{
        columns: ["h"],
        rows: [%{row: [170], occurrences: 10}]
      })
    end

    test "alias usage in where with a function" do
      assert_query("select height as h from heights where abs(h) = 170", %{
        columns: ["h"],
        rows: [%{row: [170], occurrences: 10}]
      })
    end

    test "alias usage in having" do
      assert_query("select height as h from heights group by h having abs(h) = 170", %{
        columns: ["h"],
        rows: [%{row: [170], occurrences: 1}]
      })
    end
  end

  test "select comparing two columns" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select height from heights where height = height", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "function usage in where" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..50, "heights", ["height"], [100])

    assert_query("select height from heights where sqrt(height) * 2 = 20", %{
      columns: ["height"],
      rows: [%{row: [100], occurrences: 50}]
    })
  end

  test "extended trim" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["bob"])

    assert_query("select trim(both 'b' from name) from heights", %{
      columns: ["btrim"],
      rows: [%{row: ["o"], occurrences: 10}]
    })
  end

  test "substring from" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["a name"])

    assert_query("select substring(name from 3) from heights", %{
      columns: [_],
      rows: [%{row: ["name"], occurrences: 10}]
    })
  end

  test "substring from 0" do
    assert_query("select substring(name from 0) from heights", %{
      error: "Expected `positive integer constant`" <> _
    })
  end

  test "substring from ... for ..." do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["a name"])

    assert_query("select substring(name from 3 for 2) from heights", %{
      columns: [_],
      rows: [%{row: ["na"], occurrences: 10}]
    })
  end

  test "substring for" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["a name"])

    assert_query("select substring(name for 4) from heights", %{
      columns: [_],
      rows: [%{row: ["a na"], occurrences: 10}]
    })
  end

  test "concat" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["x"])

    assert_query("select concat(name, 'y', name) from heights", %{
      columns: [_],
      rows: [%{row: ["xyx"], occurrences: 10}]
    })
  end

  test "concat with ||" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["x"])

    assert_query("select name || 'y' || name from heights", %{
      columns: [_],
      rows: [%{row: ["xyx"], occurrences: 10}]
    })
  end

  test "math functions with float constants" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [2])

    assert_query("select pow(height, 3.5) from heights", %{columns: [_], rows: [%{row: [result]}]})

    assert_in_delta result, 11.31, 0.01
  end

  test "table name is different from the database table name" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select height from heights_alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "selecting from two tables which point to the same database table" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "
        select heights.height as h1, heights_alias.height as h2
        from heights, heights_alias
        where heights.user_id=heights_alias.user_id
      ",
      %{columns: ["h1", "h2"], rows: [%{row: [180, 180], occurrences: 100}]}
    )
  end

  test "cast" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])

    assert_query("select cast(height as text) from heights", %{
      columns: ["height"],
      rows: [%{row: ["10"], occurrences: 10}]
    })
  end

  test "quoting table and column names with spaces" do
    assert_query(~s/select "thing as thing" from "weird things"/, %{
      columns: ["thing as thing"],
      rows: []
    })
  end

  test "quoting table and column names with dots" do
    assert_query(~s/select dotted.column from dotted.table/, %{
      columns: ["dotted.column"],
      rows: []
    })
  end

  test "dotted table alias" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(~s/select count("a.b".height) from heights as "a.b"/, %{rows: [%{row: [100]}]})
  end

  test "SQL injection" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name"], [180, "jon"])

    assert_query(~S(select height from heights where name = ''' or ''1''=''1'), %{
      columns: ["height"],
      rows: []
    })
  end

  test "non-alphanumeric characters in string literal" do
    weird_name = "abc ~!@\#{$1%^&1*(){}[]_+-=?/\\<>\b\z\",.:;\n\r\t 123"
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name"], [180, weird_name])

    assert_query("select count(height) from heights where name = '#{weird_name}'", %{
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "proper escaping of quotes" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name"], [180, "O'Brian"])

    assert_query("select count(height) from heights where name = 'O''Brian'", %{
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  describe "limit and offset" do
    setup do
      :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["aaa"])
      :ok = insert_rows(_user_ids = 11..20, "heights", ["name"], ["bbb"])
    end

    test "only limit" do
      assert_query("select name from heights order by name limit 5", %{
        columns: ["name"],
        rows: [%{row: ["aaa"], occurrences: 5}]
      })
    end

    test "only offset" do
      assert_query("select name from heights order by name offset 15", %{
        columns: ["name"],
        rows: [%{row: ["bbb"], occurrences: 5}]
      })
    end

    test "offset and limit" do
      assert_query("select name from heights order by name limit 10 offset 5", %{
        columns: ["name"],
        rows: [%{row: ["aaa"], occurrences: 5}, %{row: ["bbb"], occurrences: 5}]
      })
    end
  end

  describe "grouping with having filters" do
    setup do
      :ok = insert_rows(_user_ids = 30..59, "heights", ["height", "name"], [150, "jon"])
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height", "name"], [180, "dan"])
      :ok = insert_rows(_user_ids = 10..29, "heights", ["height", "name"], [160, "dan"])
    end

    test "count(*)" do
      assert_query("select height, count(*) from heights group by height having count(*) > 25", %{
        columns: ["height", "count"],
        rows: [%{row: [150, 30], occurrences: 1}]
      })
    end

    test "avg <> min" do
      assert_query("select name, count(*) from heights group by name having avg(height) <> min(height)", %{
        columns: ["name", "count"],
        rows: [%{row: ["dan", 30], occurrences: 1}]
      })
    end

    test "not null filter" do
      assert_query("select name from heights group by name having stddev(height) is not null order by 1", %{
        columns: ["name"],
        rows: [%{row: ["dan"], occurrences: 1}, %{row: ["jon"], occurrences: 1}]
      })
    end

    test "BUG: range in having" do
      assert_query("select height, count(*) as c from heights group by height having c between 20 and 100", %{
        columns: ["height", "c"],
        rows: [%{row: [150, 30], occurrences: 1}, %{row: [160, 20], occurrences: 1}]
      })
    end
  end

  test "having without group by" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])

    assert_query("select sum(height) from heights having count(1) > 0", %{
      columns: ["sum"],
      rows: [%{row: [1700], occurrences: 1}]
    })

    assert_query("select sum(height) from heights having count(1) = 0", %{
      columns: ["sum"],
      rows: []
    })
  end

  describe "noise estimates" do
    setup do
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
      :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
      :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])
    end

    test "count" do
      assert_query("select count_noise(*) from heights", %{
        columns: ["count_noise"],
        rows: [%{row: [1.0], occurrences: 1}]
      })
    end

    test "sum" do
      assert_query("select sum_noise(height) from heights", %{
        columns: ["sum_noise"],
        rows: [%{row: [200.0], occurrences: 1}]
      })
    end

    test "avg uid" do
      assert_query("select avg_noise(height), stddev(height) from heights", %{
        rows: [%{row: [0.0, _], occurrences: 1}]
      })
    end

    test "avg statistics" do
      assert_query("select trunc(avg_noise(height), 1) from heights", %{
        rows: [%{row: [6.6], occurrences: 1}]
      })
    end

    test "stddev uid" do
      assert_query("select stddev_noise(height), stddev(height) from heights", %{
        rows: [%{row: [0.0, _], occurrences: 1}]
      })
    end

    test "stddev statistics" do
      assert_query("select stddev_noise(height) from heights", %{
        rows: [%{row: [0.0], occurrences: 1}]
      })
    end

    test "variance uid" do
      assert_query("select variance_noise(height), stddev(height) from heights", %{
        rows: [%{row: [0.0, _], occurrences: 1}]
      })
    end

    test "variance statistics" do
      assert_query("select variance_noise(height) from heights", %{
        rows: [%{row: [0.0], occurrences: 1}]
      })
    end
  end

  test "bucketing values" do
    :ok = insert_rows(_user_ids = 0..5, "heights", ["height"], [175])
    :ok = insert_rows(_user_ids = 6..9, "heights", ["height"], [176])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [190])

    assert_query("select count(*), bucket(height by 9) as foo from heights group by foo", %{
      columns: ["count", "foo"],
      rows: [%{row: [10, 170.0]}, %{row: [10, 190.0]}]
    })
  end

  test "select distinct" do
    :ok = insert_rows(_user_ids = 0..5, "heights", ["height"], [175])
    :ok = insert_rows(_user_ids = 6..10, "heights", ["height"], [176])

    assert_query("select distinct height from heights group by height", %{
      columns: ["height"],
      rows: [%{row: [175]}, %{row: [176]}]
    })
  end

  test "counting distinct uids" do
    :ok = insert_rows(_user_ids = 0..9, "heights", [], [])

    assert_query("select count(distinct user_id) from heights", %{
      columns: ["count"],
      rows: [%{row: [10]}]
    })
  end

  test "parameters binding" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "select height + $1 as height from heights WHERE $3 = $2",
      [
        parameters: [
          %{type: :integer, value: 10},
          %{type: :boolean, value: true},
          %{type: :boolean, value: true}
        ]
      ],
      %{columns: ["height"], rows: [%{row: [190], occurrences: 100}]}
    )
  end

  test "user id case sensitivity and aliasing" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])

    assert_query("select count(*) from (select USER_ID from heights) as t", %{
      rows: [%{row: [10], occurrences: 1}]
    })

    assert_query("select count(T.uId) from (select user_id as uid from heights) as t", %{
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "select from a view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "select height from heights_view",
      [views: %{"heights_view" => "select user_id, height from heights"}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "select from an aliased view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "select height from heights_view view_alias",
      [views: %{"heights_view" => "select user_id, height from heights"}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "view can be used in another view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "select height from v1",
      [
        views: %{
          "v1" => "select user_id, height from v2",
          "v2" => "select user_id, height from heights"
        }
      ],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "qualified select from a view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "select heights_view.height from heights_view",
      [views: %{"heights_view" => "select user_id, height from heights"}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "select from a view that uses distinct" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "select height from heights_view",
      [views: %{"heights_view" => "select distinct user_id, height from heights"}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "select from table defined as view using distinct" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query(
      "select height from heights_query",
      [],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "per-bucket reliability flag" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query("select height from heights group by height order by height asc", %{rows: rows})

    assert [
             %{row: [150], unreliable: false},
             %{row: [160], unreliable: false},
             %{row: [180], unreliable: true}
           ] = rows
  end

  test "per-bucket reliability flag with top-level having" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query("select height from heights group by height having count(*) < 25 order by height asc", %{rows: rows})

    assert [
             %{row: [160], unreliable: false},
             %{row: [180], unreliable: true}
           ] = rows
  end

  test "group by the first position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query("select heights.height, count(*) from heights group by 1", %{
      columns: ["height", "count"],
      rows: [%{row: [170, 10], occurrences: 1}, %{row: [180, 20], occurrences: 1}]
    })
  end

  test "group by the second position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query("select count(*), heights.height from heights group by 2", %{
      columns: ["count", "height"],
      rows: [%{row: [10, 170], occurrences: 1}, %{row: [20, 180], occurrences: 1}]
    })
  end

  test "order by the first position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query("select heights.height from heights order by 1 desc", %{
      rows: [%{row: [180], occurrences: 20}, %{row: [170], occurrences: 10}]
    })
  end

  test "order by the second position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query("select 1, heights.height from heights order by 2 desc", %{
      rows: [%{row: [1, 180], occurrences: 20}, %{row: [1, 170], occurrences: 10}]
    })
  end

  test "select from an aliased table" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select height from heights h", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "select qualified from an aliased table" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query("select h.height from heights h where h.height = 180", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "aggregation of low-count values" do
    :ok = insert_rows(_user_ids = 1..3, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 3..5, "heights", ["height"], [178])
    :ok = insert_rows(_user_ids = 6..7, "heights", ["height"], [155])
    :ok = insert_rows(_user_ids = 8..9, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 10..12, "heights", ["height"], [152])
    :ok = insert_rows(_user_ids = 13..15, "heights", ["height"], [175])
    :ok = insert_rows(_user_ids = 15..17, "heights", ["height"], [153])
    :ok = insert_rows(_user_ids = 17..19, "heights", ["height"], [177])

    assert_query(
      """
        select
          height, count(distinct height), min(height), max(height), round(stddev(height)), round(avg(height))
        from heights group by height
      """,
      %{rows: [%{row: [:*, 8, 154, 176, 13, 167]}]}
    )
  end

  test "aggregation of low-count values with multiple groups" do
    :ok = insert_rows(_user_ids = 1..3, "heights", ["height", "male"], [180, false])
    :ok = insert_rows(_user_ids = 4..5, "heights", ["height", "male"], [190, nil])
    :ok = insert_rows(_user_ids = 3..5, "heights", ["height", "male"], [170, false])
    :ok = insert_rows(_user_ids = 6..7, "heights", ["height", "male"], [150, true])
    :ok = insert_rows(_user_ids = 8..9, "heights", ["height", "male"], [150, false])
    :ok = insert_rows(_user_ids = 10..12, "heights", ["height", "male"], [150, true])
    :ok = insert_rows(_user_ids = 13..15, "heights", ["height", "male"], [170, false])
    :ok = insert_rows(_user_ids = 15..17, "heights", ["height", "male"], [160, false])
    :ok = insert_rows(_user_ids = 17..19, "heights", ["height", "male"], [170, true])

    assert_query("select male, height, count(*) from heights group by 1, 2 order by 1, 2", %{
      rows: [
        %{row: [false, 170, 6]},
        %{row: [false, :*, 9]},
        %{row: [true, 150, 5]},
        %{row: [:*, :*, 5]}
      ]
    })
  end

  test "partial aggregation of low-count buckets" do
    :ok = insert_rows(_user_ids = 1..5, "heights", ["height", "name", "male"], [180, "Ana", false])
    :ok = insert_rows(_user_ids = 5..8, "heights", ["height", "name", "male"], [190, "Ana", nil])
    :ok = insert_rows(_user_ids = 9..10, "heights", ["height", "name", "male"], [170, "Tim", false])
    :ok = insert_rows(_user_ids = 10..13, "heights", ["height", "name", "male"], [150, "Ana", true])
    :ok = insert_rows(_user_ids = 11..14, "heights", ["height", "name", "male"], [180, "Ana", false])
    :ok = insert_rows(_user_ids = 15..17, "heights", ["height", "name", "male"], [150, "Tim", true])
    :ok = insert_rows(_user_ids = 20..21, "heights", ["height", "name", "male"], [180, "Tim", true])
    :ok = insert_rows(_user_ids = 16..19, "heights", ["height", "name", "male"], [170, "Ana", false])
    :ok = insert_rows(_user_ids = 8..10, "heights", ["height", "name", "male"], [150, "Tim", false])
    :ok = insert_rows(_user_ids = 11..15, "heights", ["height", "name", "male"], [190, "Ana", false])

    assert_query(
      "select male, name, height / 10, height, count(*) from heights group by 1, 2, 3, 4 order by 1, 2, 3, 4",
      %{
        rows: [
          %{row: [false, "Ana", 18.0, 180, 9]},
          %{row: [false, "Ana", 19.0, 190, 5]},
          %{row: [true, "Tim", :*, :*, 5]},
          %{row: [:*, :*, :*, :*, 20]}
        ]
      }
    )
  end

  test "stats-based partial aggregation of low-count buckets with grouping sets" do
    :ok = insert_rows(_user_ids = 1..5, "heights", ["height", "name", "male"], [180, "Ana", false])
    :ok = insert_rows(_user_ids = 5..8, "heights", ["height", "name", "male"], [190, "Ana", nil])
    :ok = insert_rows(_user_ids = 9..10, "heights", ["height", "name", "male"], [170, "Tim", false])
    :ok = insert_rows(_user_ids = 10..13, "heights", ["height", "name", "male"], [150, "Ana", true])
    :ok = insert_rows(_user_ids = 11..14, "heights", ["height", "name", "male"], [180, "Ana", false])
    :ok = insert_rows(_user_ids = 15..17, "heights", ["height", "name", "male"], [150, "Tim", true])
    :ok = insert_rows(_user_ids = 20..21, "heights", ["height", "name", "male"], [180, "Tim", true])
    :ok = insert_rows(_user_ids = 16..19, "heights", ["height", "name", "male"], [170, "Ana", false])
    :ok = insert_rows(_user_ids = 8..10, "heights", ["height", "name", "male"], [150, "Tim", false])
    :ok = insert_rows(_user_ids = 11..15, "heights", ["height", "name", "male"], [190, "Ana", false])

    assert_query(
      "select male, name, height / 10, count(*) from heights group by cube (1, 2, 3) order by 1, 2, 3",
      %{
        rows: [
          %{row: [false, "Ana", 18.0, 9]},
          %{row: [false, "Ana", 19.0, 5]},
          %{row: [false, "Ana", nil, 18]},
          %{row: [false, nil, 17.0, 6]},
          %{row: [false, nil, 18.0, 9]},
          %{row: [false, nil, 19.0, 5]},
          %{row: [false, nil, nil, 23]},
          %{row: [true, "Tim", nil, 5]},
          %{row: [true, "Tim", :*, 5]},
          %{row: [true, nil, 15.0, 7]},
          %{row: [true, nil, nil, 9]},
          %{row: [nil, "Ana", 18.0, 9]},
          %{row: [nil, "Ana", 19.0, 9]},
          %{row: [nil, "Ana", nil, 27]},
          %{row: [nil, "Ana", :*, 8]},
          %{row: [nil, "Tim", 15.0, 6]},
          %{row: [nil, "Tim", nil, 10]},
          %{row: [nil, nil, 15.0, 10]},
          %{row: [nil, nil, 17.0, 6]},
          %{row: [nil, nil, 18.0, 11]},
          %{row: [nil, nil, 19.0, 9]},
          %{row: [nil, nil, nil, 36]},
          %{row: [:*, :*, :*, 10]},
          %{row: [:*, :*, :*, 15]},
          %{row: [:*, :*, :*, 20]}
        ]
      }
    )
  end

  test "uid-based partial aggregation of low-count buckets with grouping sets" do
    :ok = insert_rows(_user_ids = 1..5, "heights", ["height", "name", "male"], [180, "Ana", false])
    :ok = insert_rows(_user_ids = 5..8, "heights", ["height", "name", "male"], [190, "Ana", nil])
    :ok = insert_rows(_user_ids = 9..10, "heights", ["height", "name", "male"], [170, "Tim", false])
    :ok = insert_rows(_user_ids = 10..13, "heights", ["height", "name", "male"], [150, "Ana", true])
    :ok = insert_rows(_user_ids = 11..14, "heights", ["height", "name", "male"], [180, "Ana", false])
    :ok = insert_rows(_user_ids = 15..17, "heights", ["height", "name", "male"], [150, "Tim", true])
    :ok = insert_rows(_user_ids = 20..21, "heights", ["height", "name", "male"], [180, "Tim", true])
    :ok = insert_rows(_user_ids = 16..19, "heights", ["height", "name", "male"], [170, "Ana", false])
    :ok = insert_rows(_user_ids = 8..10, "heights", ["height", "name", "male"], [150, "Tim", false])
    :ok = insert_rows(_user_ids = 11..15, "heights", ["height", "name", "male"], [190, "Ana", false])

    assert_query(
      "select male, name, height / 10, count(*), stddev(height) from heights group by cube (1, 2, 3) order by 1, 2, 3",
      %{
        rows: [
          %{row: [false, "Ana", 18.0, 9, _]},
          %{row: [false, "Ana", 19.0, 5, _]},
          %{row: [false, "Ana", nil, 14, _]},
          %{row: [false, nil, 17.0, 6, _]},
          %{row: [false, nil, 18.0, 9, _]},
          %{row: [false, nil, 19.0, 5, _]},
          %{row: [false, nil, nil, 20, _]},
          %{row: [false, :*, :*, 7, _]},
          %{row: [true, "Tim", nil, 5, _]},
          %{row: [true, "Tim", :*, 5, _]},
          %{row: [true, nil, 15.0, 7, _]},
          %{row: [true, nil, nil, 9, _]},
          %{row: [nil, "Ana", 18.0, 9, _]},
          %{row: [nil, "Ana", 19.0, 9, _]},
          %{row: [nil, "Ana", nil, 20, _]},
          %{row: [nil, "Ana", :*, 8, _]},
          %{row: [nil, "Tim", 15.0, 6, _]},
          %{row: [nil, "Tim", nil, 8, _]},
          %{row: [nil, nil, 15.0, 9, _]},
          %{row: [nil, nil, 17.0, 6, _]},
          %{row: [nil, nil, 18.0, 11, _]},
          %{row: [nil, nil, 19.0, 9, _]},
          %{row: [nil, nil, nil, 32, _]},
          %{row: [:*, :*, :*, 8, _]},
          %{row: [:*, :*, :*, 9, _]},
          %{row: [:*, :*, :*, 8, _]}
        ]
      }
    )
  end

  test "grouping_id in subquery" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["name", "height"], ["Alice", 170])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["name", "height"], ["Charlie", 180])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["name", "height"], ["John", 170])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["name", "height"], ["Bob", 180])

    assert_query(
      """
      select round(variance(gid)) from (
        select user_id, name, height, grouping_id(name, height) as gid
        from heights
        group by grouping sets ((1, 2), (1, 3), (1, 2, 3))
      ) as t
      """,
      %{
        rows: [%{row: [1]}]
      }
    )
  end

  test "grouping_id in top query" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["name", "height"], ["Alice", 170])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["name", "height"], ["Charlie", 180])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["name", "height"], ["John", 170])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["name", "height"], ["Bob", 180])

    assert_query(
      "select name, height, grouping_id(name, height) from heights group by grouping sets ((), (1), (2), (1, 2))",
      %{
        rows: [
          %{row: [nil, nil, 3]},
          %{row: ["Alice", nil, 1]},
          %{row: ["Bob", nil, 1]},
          %{row: ["Charlie", nil, 1]},
          %{row: ["John", nil, 1]},
          %{row: [nil, 170, 2]},
          %{row: [nil, 180, 2]},
          %{row: ["Alice", 170, 0]},
          %{row: ["Bob", 180, 0]},
          %{row: ["Charlie", 180, 0]},
          %{row: ["John", 170, 0]}
        ]
      }
    )
  end

  test "grouping_id with bucket column" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["name", "height"], ["Alice", 170])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["name", "height"], ["Charlie", 180])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["name", "height"], ["John", 170])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["name", "height"], ["Bob", 180])

    assert_query(
      """
      select name, bucket(height by 10), grouping_id(name, bucket(height by 10))
      from heights group by grouping sets ((), (1), (2), (1, 2))
      """,
      %{
        rows: [
          %{row: [nil, nil, 3]},
          %{row: ["Alice", nil, 1]},
          %{row: ["Bob", nil, 1]},
          %{row: ["Charlie", nil, 1]},
          %{row: ["John", nil, 1]},
          %{row: [nil, 170.0, 2]},
          %{row: [nil, 180.0, 2]},
          %{row: ["Alice", 170.0, 0]},
          %{row: ["Bob", 180.0, 0]},
          %{row: ["Charlie", 180.0, 0]},
          %{row: ["John", 170.0, 0]}
        ]
      }
    )
  end

  test "grouping sets with expressions over same column" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["name", "height"], ["Alice", 170])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["name", "height"], ["Charlie", 180])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["name", "height"], ["John", 170])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["name", "height"], ["Bob", 180])

    assert_query(
      "select bucket(height by 10), height from heights group by grouping sets ((), (1, 2))",
      %{
        rows: [
          %{row: [nil, nil]},
          %{row: [170.0, 170]},
          %{row: [180.0, 180]}
        ]
      }
    )
  end

  test "distinct in subquery with group by" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "male"], [160, true])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height", "male"], [170, false])

    assert_query(
      "select count(*) from (select distinct user_id, male from heights group by user_id, height, male) alias",
      %{rows: [%{row: [40]}]}
    )
  end

  test "distinct in subquery with group by and where" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "male"], [160, true])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height", "male"], [170, false])

    assert_query(
      "select count(*) from (select distinct user_id, count(*) from heights where not male group by user_id) alias",
      %{rows: [%{row: [20]}]}
    )
  end

  test "Distinct in top-level query" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "male"], [180, true])
    :ok = insert_rows(_user_ids = 21..40, "heights", ["height", "male"], [165, false])

    assert_query(
      "SELECT DISTINCT height, male FROM heights ORDER BY height DESC",
      %{rows: [%{row: [180, true], occurrences: 1}, %{row: [165, false], occurrences: 1}]}
    )
  end

  test "DISTINCT on constants" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "male"], [180, true])

    assert_query("SELECT COUNT(*) FROM (SELECT DISTINCT 1 + 1, height FROM heights) bar", %{
      rows: [%{occurrences: 1, row: [1]}]
    })
  end

  test "[Issue #2860] DISTINCT in subquery with scoped columns" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "male"], [160, true])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height", "male"], [170, false])

    assert_query(
      """
        select count(*) from (
          select distinct user_id, count(*) from heights group by user_id having count(heights.male) = 1
        ) alias
      """,
      %{rows: [%{row: [20]}]}
    )
  end

  test "[Issue #2217] condition on user_id" do
    :ok =
      Cloak.Test.DB.create_table(
        "numeric_user_ids",
        "user_id integer",
        user_id: "user_id",
        add_user_id: false
      )

    :ok = Cloak.Test.DB.insert_data("numeric_user_ids", _columns = ["user_id"], Enum.map(0..9, &[&1]))
    :ok = Cloak.Test.DB.insert_data("numeric_user_ids", _columns = ["user_id"], Enum.map(10..19, &[&1]))

    assert_query("select count(*) from numeric_user_ids where user_id between 0 and 10", %{
      rows: [%{row: [10]}]
    })
  end

  test "unary NOT" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "male"], [160, true])
    :ok = insert_rows(_user_ids = 1..5, "heights", ["height", "male"], [160, false])

    assert_query("SELECT count(*) from heights where NOT (height <> 160 OR male <> true)", %{
      rows: [%{row: [20]}]
    })
  end

  describe "cast boolean" do
    setup do
      :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name", "male"], [1, "true", true])
    end

    test "to text" do
      assert_query(
        "select distinct x from (select cast(male as text) as x from heights) t",
        %{rows: [%{row: ["true"]}]}
      )
    end

    test "to integer" do
      assert_query(
        "select distinct x from (select cast(male as integer) as x from heights) t",
        %{rows: [%{row: [1]}]}
      )
    end

    test "from text" do
      assert_query(
        "select distinct x from (select cast(name as boolean) as x from heights) t",
        %{rows: [%{row: [true]}]}
      )
    end

    test "from integer" do
      assert_query(
        "select distinct x from (select cast(height as boolean) as x from heights) t",
        %{rows: [%{row: [true]}]}
      )
    end

    test "to real" do
      assert_query(
        "select distinct x from (select cast(male as real) as x from heights) t",
        %{rows: [%{row: [1.0]}]}
      )
    end

    test "from real" do
      assert_query(
        "select distinct x from (select cast(height*1.0 as boolean) as x from heights) t",
        %{rows: [%{row: [true]}]}
      )
    end
  end

  test "stddev with nulls" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [nil])

    assert_query("select stddev(height) from heights", %{rows: [%{row: [nil], occurrences: 1}]})
  end

  test "[Issue #3714] subtracting a later date from an earlier one" do
    :ok = insert_rows(_user_ids = 1..10, "dates", ["date", "date2"], [~N[2017-02-02 12:22:33], ~N[2017-02-02 12:55:55]])

    assert_query("SELECT date2 - date, COUNT(*) FROM dates GROUP BY 1", %{rows: rows})
    assert_query("SELECT date - date2, COUNT(*) FROM dates GROUP BY 1", %{rows: ^rows})
  end

  test "[Issue #4121] recursive aggregators in HAVING clause" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [180])

    assert_query("SELECT COUNT(*) AS c FROM heights HAVING MIN(c) = 180", %{
      error: "Expression `min(count(*))` recursively calls multiple aggregators." <> _
    })
  end

  test "[Issue #4133] crash with noise layer from date arithmetic" do
    :ok = insert_rows(_user_ids = 1..10, "dates", ["date"], [~N[2020-02-02 01:00:00]])

    assert_query("SELECT COUNT(*) AS c FROM dates WHERE date = DATE '2020-02-02' + INTERVAL 'PT1H'", %{
      rows: [%{row: [10]}]
    })
  end

  test "[Issue #4180] join protection for queries using `order by`" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [180])

    assert_query(
      """
        SELECT count(t2.user_id) FROM
          (SELECT user_id FROM heights WHERE height = 180 ORDER BY 1) AS t1
        INNER JOIN
          heights AS t2
        ON t1.user_id = t2.user_id
      """,
      %{rows: [%{row: [10]}]}
    )
  end

  test "filtering censored values" do
    :ok = insert_rows(_user_ids = 0..2, "heights", ["name"], ["Alice"])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["name"], ["Charlie"])
    :ok = insert_rows(_user_ids = 3..6, "heights", ["name"], ["John"])
    :ok = insert_rows(_user_ids = 7..9, "heights", ["name"], ["Bob"])

    assert_query("select count(*), name from heights group by name having name = 'Charlie'", %{
      columns: ["count", "name"],
      rows: rows
    })

    assert [%{row: [10, "Charlie"], occurrences: 1}] = rows
  end
end
