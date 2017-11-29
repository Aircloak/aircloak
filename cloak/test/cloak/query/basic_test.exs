defmodule Cloak.Query.BasicTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights", "height INTEGER, name TEXT, male BOOLEAN")
    :ok = Cloak.Test.DB.create_table("heights_alias", nil, db_name: "heights", skip_db_create: true)
    :ok = Cloak.Test.DB.create_table("children", "age INTEGER, name TEXT")
    :ok = Cloak.Test.DB.create_table("weird things", "\"thing as thing\" INTEGER", db_name: "weird")
    :ok = Cloak.Test.DB.create_table("dates", "date timestamp")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("heights")
    Cloak.Test.DB.clear_table("children")
    Cloak.Test.DB.clear_table("weird")
    Cloak.Test.DB.clear_table("dates")
    :ok
  end

  test "show tables" do
    assert_query "show tables", %{columns: ["name"], rows: table_rows}
    tables = Enum.map(table_rows, fn(%{row: [table_name]}) -> table_name end)

    [:children, :heights, :heights_alias, :"weird things", :dates]
    |> Enum.each(&assert(Enum.member?(tables, to_string(&1))))
  end

  test "show tables and views" do
    assert_query "show tables",
      [views: %{"v1" => "select user_id, height from heights"}],
      %{columns: ["name"], rows: table_rows}
    tables = Enum.map(table_rows, fn(%{row: [table_name]}) -> table_name end)

    [:children, :heights, :heights_alias, :"weird things", :dates, :v1]
    |> Enum.each(&assert(Enum.member?(tables, to_string(&1))))
  end

  test "show columns" do
    assert_query "show columns from heights", %{query_id: "1", columns: ["name", "type"], rows: rows}

    assert Enum.sort_by(rows, &(&1[:row])) == [
      %{occurrences: 1, row: ["height", "integer"]},
      %{occurrences: 1, row: ["male", "boolean"]},
      %{occurrences: 1, row: ["name", "text"]},
      %{occurrences: 1, row: ["user_id", "text"]}
    ]
  end

  test "show columns from a view" do
    assert_query "show columns from v1",
      [views: %{"v1" => "select user_id, height from heights"}],
      %{query_id: "1", columns: ["name", "type"], rows: rows}

    assert Enum.sort_by(rows, &(&1[:row])) == [
      %{occurrences: 1, row: ["height", "integer"]},
      %{occurrences: 1, row: ["user_id", "text"]}
    ]
  end

  test "simple select query" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "rows with null user_ids are ignored" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = insert_null_uid_row("heights", ["height"], [180])

    assert_query "select height from heights",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "identifiers are case-insensitive" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select HeiGht, Heights.heighT from heIghts",
      %{columns: ["HeiGht", "heighT"], rows: [%{row: [180, 180], occurrences: 100}]}
  end

  test "select all query" do
    assert_query "select * from heights", %{query_id: "1", columns: ["user_id", "height", "name", "male"], rows: _}
  end

  test "multiple select alls" do
    assert_query "select *, height as h, * from heights", %{query_id: "1", columns: columns, rows: _}
    assert columns == ["user_id", "height", "name", "male", "h", "user_id", "height", "name", "male"]
  end

  test "select all from a table" do
    assert_query "select heights.* from heights",
      %{query_id: "1", columns: ["user_id", "height", "name", "male"], rows: _}
  end

  test "select a constant" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])
    assert_query "select 3 from heights", %{columns: [""], rows: [%{occurrences: 10, row: [3]}]}
  end

  test "select an aliased constant" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])
    assert_query "select 'text' as the_text from heights",
      %{columns: ["the_text"], rows: [%{occurrences: 10, row: ["text"]}]}
  end

  test "a binary function of two columns" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [22])
    assert_query "select div(height, height) from heights",
      %{columns: ["div"], rows: [%{occurrences: 10, row: [1]}]}
  end

  test "select all and order query" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 180, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 180, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query "select * from heights order by name",
      %{query_id: "1", columns: ["user_id", "height", "name", "male"], rows: rows}
    assert Enum.map(rows, &(&1[:row])) == [[:*, 180, "adam", true], [:*, 180, "john", true], [:*, 180, "mike", true]]
  end

  test "order by non-selected field" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query "select height from heights order by name",
      %{query_id: "1", columns: ["height"], rows: rows}
    assert Enum.map(rows, &(&1[:row])) == [[170], [160], [180]]
  end

  test "order by grouped but non-selected field" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query "select sum(height) from heights group by name order by name",
      %{query_id: "1", columns: ["sum"], rows: rows}
    assert Enum.map(rows, &(&1[:row])) == [[1700], [1600], [1800]]
  end

  test "order by grouped but non-selected aggregate" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query "select name from heights group by name order by sum(height) desc",
      %{query_id: "1", columns: ["name"], rows: rows}
    assert Enum.map(rows, &(&1[:row])) == [["mike"], ["adam"], ["john"]]
  end

  test "order by grouped but non-selected aggregate with selected aggregate function" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 160, true])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["name", "height", "male"], ["adam", 170, true])
    :ok = insert_rows(_user_ids = 31..60, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query "select bucket(height by 10), avg(height) from heights group by 1 order by count(*) desc",
      %{query_id: "1", columns: ["bucket", "avg"], rows: rows}
    assert Enum.map(rows, &(&1[:row])) == [[180.0, 180.0], [170.0, 170.0], [160.0, 160.0]]
  end

  test "should return LCF property when sufficient rows are filtered" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..3, "heights", ["height"], [160])
    :ok = insert_rows(_user_ids = 20..23, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..23, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 24..27, "heights", ["height"], [200])
    :ok = insert_rows(_user_ids = 24..27, "heights", ["height"], [150])

    assert_query "select height from heights order by height",
      %{columns: ["height"], rows: [
        %{row: [180], occurrences: 20},
        %{row: [:*], occurrences: 19},
      ]}
  end

  test "count(*)" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [nil])

    assert_query "select count(*) from heights",
      %{columns: ["count"], rows: [%{row: [40], occurrences: 1}]}
  end

  test "count(column)" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [nil])

    assert_query "select COUNT(height) from heights",
      %{columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "count(distinct column) for empty sets" do
    assert_query "select count(distinct height) from heights",
      %{columns: ["count"], rows: [%{row: [0], occurrences: 1}]}
  end

  test "distinct column" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [175])

    assert_query "select distinct height from heights order by height",
      %{
        columns: ["height"],
        rows: [%{row: [170], occurrences: 1}, %{row: [175], occurrences: 1}, %{row: [180], occurrences: 1}]
      }
  end

  test "aggregate on distinct column with too few values" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])

    assert_query "select avg(distinct height) from heights",
      %{columns: ["avg"], rows: [%{row: [nil], occurrences: 1}]}
  end

  describe "aggregate on distinct column" do
    setup do
      :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [170])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [175])
      :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [nil])
      :ok = insert_rows(_user_ids = 40..49, "heights", ["height"], [160])
      :ok = insert_rows(_user_ids = 50..59, "heights", ["height"], [190])
      :ok = insert_rows(_user_ids = 60..69, "heights", ["height"], [165])
      :ok = insert_rows(_user_ids = 70..79, "heights", ["height"], [185])
    end

    test "avg(distinct column)" do
      assert_query "select avg(distinct height) from heights",
        %{columns: ["avg"], rows: [%{row: [162.5], occurrences: 1}]}
    end

    test "avg(distinct fun(column))" do
      assert_query "select avg(distinct abs(height)) from heights",
        %{columns: ["avg"], rows: [%{row: [162.5], occurrences: 1}]}
    end

    test "avg(distinct column - constant)" do
      assert_query "select avg(distinct height - 100) from heights",
        %{columns: ["avg"], rows: [%{row: [62.5], occurrences: 1}]}
    end
  end

  test "aggregates of an empty table" do
    assert_query "select count(*), count(height), avg(height) from heights",
      %{columns: ["count", "count", "avg"], rows: [%{row: [0, 0, nil], occurrences: 1}]}
  end

  describe "aggregating positive values" do
    setup do
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
      :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
      :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])
    end

    test "sum" do
      assert_query "select sum(height) from heights",
        %{columns: ["sum"], rows: [%{row: [5400], occurrences: 1}]}
    end

    test "min" do
      assert_query "select min(height) from heights",
        %{columns: ["min"], rows: [%{row: [170], occurrences: 1}]}
    end

    test "max" do
      assert_query "select max(height) from heights",
        %{columns: ["max"], rows: [%{row: [190], occurrences: 1}]}
    end

    test "avg" do
      assert_query "select avg(height) from heights",
        %{columns: ["avg"], rows: [%{row: [180.0], occurrences: 1}]}
    end

    test "stddev" do
      assert_query "select stddev(height) from heights",
        %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
      assert_in_delta(stddev, 8.1, 0.1)
    end

    test "median" do
      assert_query "select median(height) from heights",
        %{columns: ["median"], rows: [%{row: [180], occurrences: 1}]}
    end

    test "sum(qualified_column)" do
      assert_query "select sum(heights.height) from heights",
        %{columns: ["sum"], rows: [%{row: [5400], occurrences: 1}]}
    end

    test "min(qualified_column)" do
      assert_query "select min(heights.height) from heights",
        %{columns: ["min"], rows: [%{row: [170], occurrences: 1}]}
    end

    test "max(qualified_column)" do
      assert_query "select max(heights.height) from heights",
        %{columns: ["max"], rows: [%{row: [190], occurrences: 1}]}
    end

    test "avg(qualified_column)" do
      assert_query "select avg(heights.height) from heights",
        %{columns: ["avg"], rows: [%{row: [180.0], occurrences: 1}]}
    end

    test "stddev(qualified_column)" do
      assert_query "select stddev(heights.height) from heights",
        %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
      assert_in_delta(stddev, 8.1, 0.1)
    end

    test "median(qualified_column)" do
      assert_query "select median(heights.height) from heights",
        %{columns: ["median"], rows: [%{row: [180], occurrences: 1}]}
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
      assert_query "select sum(height) from heights",
        %{columns: ["sum"], rows: [%{row: [-5430], occurrences: 1}]}
    end

    test "min" do
      assert_query "select min(height) from heights",
        %{columns: ["min"], rows: [%{row: [-190], occurrences: 1}]}
    end

    test "max" do
      assert_query "select max(height) from heights",
        %{columns: ["max"], rows: [%{row: [-170], occurrences: 1}]}
    end

    test "avg" do
      assert_query "select avg(height) from heights",
        %{columns: ["avg"], rows: [%{row: [-181.0], occurrences: 1}]}
    end

    test "stddev" do
      assert_query "select stddev(height) from heights",
        %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
      assert_in_delta(stddev, 8.29, 0.1)
    end

    test "median" do
      assert_query "select median(height) from heights",
        %{columns: ["median"], rows: [%{row: [-183], occurrences: 1}]}
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
      assert_query "select sum(height) from heights",
        %{columns: ["sum"], rows: [%{row: [-1850], occurrences: 1}]}
    end

    test "min" do
      assert_query "select min(height) from heights",
        %{columns: ["min"], rows: [%{row: [-190], occurrences: 1}]}
    end

    test "max" do
      assert_query "select max(height) from heights",
        %{columns: ["max"], rows: [%{row: [180], occurrences: 1}]}
    end

    test "avg" do
      assert_query "select avg(height) from heights",
        %{columns: ["avg"], rows: [%{row: [-61.666666666666664], occurrences: 1}]}
    end

    test "stddev" do
      assert_query "select stddev(height) from heights",
        %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
      assert_in_delta(stddev, 170.99, 0.1)
    end

    test "median" do
      assert_query "select median(height) from heights",
        %{columns: ["median"], rows: [%{row: [-175], occurrences: 1}]}
    end
  end

  test "should return nil when not enough values present for anonymization" do
    :ok = insert_rows(_user_ids = 0..8, "heights", ["height"], [180])

    assert_query "select median(height) from heights",
      %{columns: ["median"], rows: [%{row: [nil], occurrences: 1}]}
  end

  test "select the same column multiple times" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query "select height, height from heights",
      %{columns: ["height", "height"], rows: [%{row: [180, 180], occurrences: 100}]}
  end

  test "select the same aggregate multiple times" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query "select count(height), count(*), count(*), count(height) from heights",
      %{
        columns: ["count", "count", "count", "count"],
        rows: [%{row: [100, 100, 100, 100], occurrences: 1}]
      }
  end

  test "different aggregates on the same column" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query "select count(height), max(height) from heights",
      %{columns: ["count", "max"], rows: [%{row: [100, 180], occurrences: 1}]}
  end

  test "should allow ranges in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height >= 180 and height < 190",
      %{query_id: "1", columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow between in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height between 180 and 190",
      %{query_id: "1", columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow reversed inequalities in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where 180 <= height and 190 > height",
      %{query_id: "1", columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow LIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "bob"])

    assert_query "select count(*) from heights where name LIKE 'b%'",
      %{columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow NOT LIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "bob"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "alice"])

    assert_query "select count(*) from heights where name NOT LIKE 'b%'",
      %{columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should allow NOT ILIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "Bob"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "alice"])

    assert_query "select count(*) from heights where name NOT ILIKE 'b%'",
      %{columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should allow ILIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "Bob"])

    assert_query "select count(*) from heights where name ILIKE 'b%'",
      %{columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should handle escaped chars in LIKE conditions" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "b_%"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "bob"])

    assert_query "select count(*) from heights where name LIKE 'b~_~%%' ESCAPE '~'",
      %{columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow IN in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height IN (170, 180, 190)",
      %{columns: ["count"], rows: [%{row: [60], occurrences: 1}]}
  end

  test "should allow NOT IN in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height NOT IN (170, 190)",
      %{columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow <> in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 40..49, "heights", ["height"], [nil])

    assert_query "select count(*) from heights where height <> 180",
      %{columns: ["count"], rows: [%{row: [40], occurrences: 1}]}
  end

  test "<> conditions count unique users" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["name"], ["Alice"])
    1..10 |> Enum.each(fn _ -> :ok = insert_rows(_user_ids = [10], "heights", ["name"], ["Bob"]) end)

    assert_query "select count(*) from heights where name <> 'Bob'",
      %{query_id: "1", columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow IS NULL in where clause" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [180])

    assert_query "select count(*) from heights where height IS NULL",
      %{columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should allow IS NOT NULL in where clause" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [180])

    assert_query "select count(*) from heights where height IS NOT NULL",
      %{columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "select and filter booleans" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 180, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["eva", 160, false])

    assert_query "select height, male from heights where male = true",
      %{query_id: "1", columns: ["height", "male"], rows: [%{row: [180, true], occurrences: 10}]}
  end

  test "select with constant filter" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 180, true])

    assert_query "select height, male from heights where true = true",
      %{query_id: "1", columns: ["height", "male"], rows: [%{row: [180, true], occurrences: 10}]}
  end

  test "should order rows when instructed" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])

    assert_query "select height from heights order by height",
      %{query_id: "1", columns: ["height"], rows: rows}
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
      {2015, 12, 01},
    ]
    dates = for {year, month, day} <- dates do
      date = %NaiveDateTime{
        year: year, month: month, day: day,
        hour: 0, minute: 0, second: 0, microsecond: {0, 6}
      }
      :ok = insert_rows(_user_ids = 0..100, "dates", ["date"], [date])
      NaiveDateTime.to_iso8601(date)
    end

    assert_query "select date from dates group by date order by date asc",
      %{query_id: "1", columns: ["date"], rows: rows}
    assert (for data <- rows, do: hd(data.row)) == dates

    assert_query "select date from dates group by date order by date desc",
      %{query_id: "1", columns: ["date"], rows: rows}
    assert (for data <- rows, do: hd(data.row)) == Enum.reverse(dates)
  end

  test "intervals in query results" do
    :ok = insert_rows(_user_ids = 0..4, "dates", ["date"], [~N[2017-01-02 00:01:02]])
    assert_query "select date - date, interval 'P1Y2M' from dates",
      %{rows: [%{row: ["P", "P1Y2M"]}]}
  end

  test "query allows mixing aggregated and grouped columns" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height",
      %{columns: ["count", "height"], rows: rows}
    assert [%{row: [10, 180], occurrences: 1}, %{row: [20, 160], occurrences: 1}] = Enum.sort_by(rows, &(&1[:row]))
  end

  test "grouping works when the column is not selected" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*) from heights group by height", %{columns: ["count"], rows: rows}
    assert [%{row: [10], occurrences: 1}, %{row: [20], occurrences: 1}] = Enum.sort_by(rows, &(&1[:row]))
  end

  test "grouping and sorting by a count" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height order by count(*) asc",
      %{columns: ["count", "height"], rows: rows}
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

    assert_query "select count(*), name from heights group by name order by name asc",
      %{columns: ["count", "name"], rows: rows}
    assert [%{row: [10, "Charlie"], occurrences: 1}, %{row: [10, :*], occurrences: 1}] = rows
  end

  test "grouping and sorting by a grouped field" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height order by height asc",
      %{columns: ["count", "height"], rows: rows}
    assert [
      %{row: [30, 150], occurrences: 1},
      %{row: [20, 160], occurrences: 1},
      %{row: [10, 180], occurrences: 1}
    ] = rows
  end

  test "query which returns zero rows" do
    Cloak.Test.DB.clear_table("heights")
    assert_query "select height from heights", %{query_id: "1", columns: ["height"], rows: []}
  end

  describe "alias usage" do
    setup do
      :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
      :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [180])
    end

    test "select with column alias" do
      assert_query "select height as h from heights group by h order by h",
        %{columns: ["h"], rows: [%{row: [170], occurrences: 1}, %{row: [180], occurrences: 1}]}
    end

    test "select with duplicated alias" do
      assert_query "select count(*) as c, count(height) as c from heights",
        %{columns: ["c", "c"], rows: [%{row: [30, 30], occurrences: 1}]}
    end

    test "alias usage in where" do
      assert_query "select height as h from heights where h = 170",
        %{columns: ["h"], rows: [%{row: [170], occurrences: 10}]}
    end

    test "alias usage in where with a function" do
      assert_query "select height as h from heights where abs(h) = 170",
        %{columns: ["h"], rows: [%{row: [170], occurrences: 10}]}
    end

    test "alias usage in having" do
      assert_query "select height as h from heights group by h having abs(h) = 170",
        %{columns: ["h"], rows: [%{row: [170], occurrences: 1}]}
    end
  end

  test "select comparing two columns" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights where height = height",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "function usage in where" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..50, "heights", ["height"], [100])
    assert_query "select height from heights where sqrt(height) * 2 = 20",
      %{columns: ["height"], rows: [%{row: [100], occurrences: 50}]}
  end

  test "extended trim" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["bob"])
    assert_query "select trim(both 'b' from name) from heights",
      %{columns: ["btrim"], rows: [%{row: ["o"], occurrences: 10}]}
  end

  test "substring from" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["a name"])
    assert_query "select substring(name from 3) from heights",
      %{columns: [_], rows: [%{row: ["name"], occurrences: 10}]}
  end

  test "substring from 0" do
    assert_query "select substring(name from 0) from heights",
      %{error: "Expected `positive integer constant` at line 1, column 28."}
  end

  test "substring from ... for ..." do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["a name"])
    assert_query "select substring(name from 3 for 2) from heights",
      %{columns: [_], rows: [%{row: ["na"], occurrences: 10}]}
  end

  test "substring for" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["a name"])
    assert_query "select substring(name for 4) from heights",
      %{columns: [_], rows: [%{row: ["a na"], occurrences: 10}]}
  end

  test "concat" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["x"])
    assert_query "select concat(name, 'y', name) from heights",
      %{columns: [_], rows: [%{row: ["xyx"], occurrences: 10}]}
  end

  test "concat with ||" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["x"])
    assert_query "select name || 'y' || name from heights",
      %{columns: [_], rows: [%{row: ["xyx"], occurrences: 10}]}
  end

  test "math functions with float constants" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [2])
    assert_query "select pow(height, 3.5) from heights", %{columns: [_], rows: [%{row: [result]}]}
    assert_in_delta result, 11.31, 0.01
  end

  test "table name is different from the database table name" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights_alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
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
    assert_query "select cast(height as text) from heights",
      %{columns: ["cast"], rows: [%{row: ["10"], occurrences: 10}]}
  end

  test "optimization of a redundant cast doesn't affect the column name" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])
    assert_query "select cast(height as integer) from heights",
      %{columns: ["cast"], rows: [%{row: [10], occurrences: 10}]}
  end

  test "quoting table and column names" do
    assert_query "select \"thing as thing\" from \"weird things\"",
      %{columns: ["thing as thing"], rows: []}
  end

  test "SQL injection" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name"], [180, "jon"])
    assert_query ~S(select height from heights where name = ''' or ''1''=''1'),
      %{columns: ["height"], rows: []}
  end

  test "non-alphanumeric characters in string literal" do
    weird_name = "abc ~!@\#{$1%^&1*(){}[]_+-=?/\\<>\b\z\",.:;\n\r\t 123"
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name"], [180, weird_name])
    assert_query "select count(height) from heights where name = '#{weird_name}'",
      %{columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "proper escaping of quotes" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name"], [180, "O'Brian"])
    assert_query "select count(height) from heights where name = 'O''Brian'",
      %{columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  describe "limit and offset" do
    setup do
      :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["aaa"])
      :ok = insert_rows(_user_ids = 11..20, "heights", ["name"], ["bbb"])
    end

    test "only limit" do
      assert_query "select name from heights order by name limit 5",
        %{columns: ["name"], rows: [%{row: ["aaa"], occurrences: 5}]}
    end

    test "only offset" do
      assert_query "select name from heights order by name offset 15",
        %{columns: ["name"], rows: [%{row: ["bbb"], occurrences: 5}]}
    end

    test "offset and limit" do
      assert_query "select name from heights order by name limit 10 offset 5",
        %{columns: ["name"], rows: [%{row: ["aaa"], occurrences: 5}, %{row: ["bbb"], occurrences: 5}]}
    end
  end

  describe "grouping with having filters" do
    setup do
      :ok = insert_rows(_user_ids = 30..59, "heights", ["height", "name"], [150, "jon"])
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height", "name"], [180, "dan"])
      :ok = insert_rows(_user_ids = 10..29, "heights", ["height", "name"], [160, "dan"])
    end

    test "count(*)" do
      assert_query "select height, count(*) from heights group by height having count(*) > 25",
        %{columns: ["height", "count"], rows: [%{row: [150, 30], occurrences: 1}]}
    end

    test "avg <> min" do
      assert_query "select name, count(*) from heights group by name having avg(height) <> min(height)",
        %{columns: ["name", "count"], rows: [%{row: ["dan", 30], occurrences: 1}]}
    end
  end

  test "having without group by" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])

    assert_query "select sum(height) from heights having count(1) > 0",
      %{columns: ["sum"], rows: [%{row: [1700], occurrences: 1}]}
    assert_query "select sum(height) from heights having count(1) = 0",
      %{columns: ["sum"], rows: []}
  end

  describe "noise estimates" do
    setup do
      :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
      :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
      :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
      :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])
    end

    test "count" do
      assert_query "select count_noise(*) from heights",
        %{columns: ["count_noise"], rows: [%{row: [0.0], occurrences: 1}]}
    end

    test "sum" do
      assert_query "select sum_noise(height) from heights",
        %{columns: ["sum_noise"], rows: [%{row: [0.0], occurrences: 1}]}
    end

    test "avg" do
      assert_query "select avg_noise(height) from heights",
        %{columns: ["avg_noise"], rows: [%{row: [0.0], occurrences: 1}]}
    end

    test "stddev" do
      assert_query "select stddev_noise(height) from heights",
        %{columns: ["stddev_noise"], rows: [%{row: [0.0], occurrences: 1}]}
    end
  end

  test "bucketing values" do
    :ok = insert_rows(_user_ids = 0..5, "heights", ["height"], [175])
    :ok = insert_rows(_user_ids = 6..9, "heights", ["height"], [176])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [190])

    assert_query "select count(*), bucket(height by 9) as foo from heights group by foo",
      %{columns: ["count", "foo"], rows: [%{row: [10, 170.0]}, %{row: [10, 190.0]}]}
  end

  test "select distinct" do
    :ok = insert_rows(_user_ids = 0..5, "heights", ["height"], [175])
    :ok = insert_rows(_user_ids = 6..10, "heights", ["height"], [176])

    assert_query "select distinct height from heights group by height order by height",
      %{columns: ["height"], rows: [%{row: [175]}, %{row: [176]}]}
  end

  test "counting distinct uids" do
    :ok = insert_rows(_user_ids = 0..9, "heights", [], [])

    assert_query "select count(distinct user_id) from heights",
      %{columns: ["count"], rows: [%{row: [10]}]}
  end

  test "parameters binding" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height + $1 as height from heights WHERE $3 = $2",
      [parameters: [%{type: :integer, value: 10}, %{type: :boolean, value: true}, %{type: :boolean, value: true}]],
      %{columns: ["height"], rows: [%{row: [190], occurrences: 100}]}
  end

  test "user id case sensitivity and aliasing" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])

    assert_query "select count(*) from (select USER_ID from heights) as t",
      %{rows: [%{row: [10], occurrences: 1}]}
    assert_query "select count(T.uId) from (select user_id as uid from heights) as t",
      %{rows: [%{row: [10], occurrences: 1}]}
  end

  test "select from a view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights_view",
      [views: %{"heights_view" => "select user_id, height from heights"}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "select from an aliased view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights_view view_alias",
      [views: %{"heights_view" => "select user_id, height from heights"}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "view can be used in another view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from v1",
      [views: %{
        "v1" => "select user_id, height from v2",
        "v2" => "select user_id, height from heights"
      }],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "qualified select from a view" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select heights_view.height from heights_view",
      [views: %{"heights_view" => "select user_id, height from heights"}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "per-bucket noisy users count" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select height from heights group by height order by height asc", %{rows: rows}
    assert [
      %{row: [150], users_count: 30},
      %{row: [160], users_count: 20},
      %{row: [180], users_count: 10}
    ] = rows
  end

  test "group by the first position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query "select heights.height, count(*) from heights group by 1",
      %{
        columns: ["height", "count"],
        rows: [%{row: [170, 10], occurrences: 1}, %{row: [180, 20], occurrences: 1}]
      }
  end

  test "group by the second position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query "select count(*), heights.height from heights group by 2",
      %{
        columns: ["count", "height"],
        rows: [%{row: [10, 170], occurrences: 1}, %{row: [20, 180], occurrences: 1}]
      }
  end

  test "order by the first position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query "select heights.height from heights order by 1 desc",
      %{rows: [%{row: [180], occurrences: 20}, %{row: [170], occurrences: 10}]}
  end

  test "order by the second position in the select list" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height"], [180])

    assert_query "select 1, heights.height from heights order by 2 desc",
      %{rows: [%{row: [1, 180], occurrences: 20}, %{row: [1, 170], occurrences: 10}]}
  end

  test "select from an aliased table" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights h",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "select qualified from an aliased table" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select h.height from heights h where h.height = 180",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "select all from an aliased table" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select * from heights h",
      %{
        columns: ["user_id", "height", "name", "male"],
        rows: [%{occurrences: 100, row: [:*, 180, nil, nil]}]
      }
  end

  test "sample from table" do
    :ok = insert_rows(_user_ids = 1..1000, "heights", ["height"], [180])
    assert_query "select count(height) from heights sample_users 2%", %{rows: [%{row: [25]}]}
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

    assert_query """
      select
        height, count(distinct height), min(height), max(height), median(height), round(avg(height))
      from heights group by height
    """, %{rows: [%{row: [:*, 8, 154, 176, 166, 167]}]}
  end

  test "distinct in subquery with group by" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height", "male"], [160, true])
    :ok = insert_rows(_user_ids = 11..30, "heights", ["height", "male"], [170, false])
    assert_query(
      "select count(*) from (select distinct user_id, male from heights group by user_id, height, male) alias",
      %{rows: [%{row: [40]}]}
    )
  end
end
