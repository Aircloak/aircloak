defmodule Cloak.Query.BasicTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers
  alias Cloak.Aql.Column

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
    :ok
  end

  test "show tables" do
    assert_query "show tables", %{columns: ["name"], rows: table_rows}
    tables = Enum.map(table_rows, fn(%{row: [table_name]}) -> table_name end)

    [:children, :heights, :heights_alias, :"weird things", :dates]
    |> Enum.each(&assert(Enum.member?(tables, &1)))
  end

  test "show columns" do
    assert_query "show columns from heights", %{query_id: "1", columns: ["name", "type"], rows: rows}
    assert Enum.sort_by(rows, &(&1[:row])) == [
      %{occurrences: 1, row: ["height", :integer]},
      %{occurrences: 1, row: ["male", :boolean]},
      %{occurrences: 1, row: ["name", :text]},
      %{occurrences: 1, row: ["user_id", :text]}
    ]
  end

  test "simple select query" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "select all query" do
    assert_query "select * from heights",
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

  test "a binary function of a column and a constant" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [22])
    assert_query "select div(height, 3) from heights",
      %{columns: ["div"], rows: [%{occurrences: 10, row: [7]}]}
  end

  test "select all and order query" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "male"], ["john", 180, true])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height", "male"], ["adam", 180, true])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height", "male"], ["mike", 180, true])

    assert_query "select * from heights order by name",
      %{query_id: "1", columns: ["user_id", "height", "name", "male"], rows: rows}
    assert Enum.map(rows, &(&1[:row])) == [[:*, :*, :*, :*]]
  end

  test "should return LCF property when sufficient rows are filtered" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..1, "heights", ["height"], [160])
    :ok = insert_rows(_user_ids = 20..24, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..24, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 25..29, "heights", ["height"], [200])
    :ok = insert_rows(_user_ids = 25..29, "heights", ["height"], [150])

    assert_query "select height from heights order by height",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 20}, %{row: [:*], occurrences: 22}]}
  end

  test "should produce counts" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [nil])

    assert_query "select count(*) from heights",
      %{columns: ["count"], rows: [%{row: [40], occurrences: 1}]}

    assert_query "select COUNT(height) from heights",
      %{columns: ["count"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "count(distinct column)" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [175])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [160])
    :ok = insert_rows(_user_ids = 40..40, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 40..40, "heights", ["height"], [151])
    :ok = insert_rows(_user_ids = 40..40, "heights", ["height"], [152])
    :ok = insert_rows(_user_ids = 40..40, "heights", ["height"], [153])
    :ok = insert_rows(_user_ids = 41..49, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 50..59, "heights", ["height"], [190])

    assert_query "select count(distinct height) from heights",
      %{columns: ["count"], rows: [%{row: [5], occurrences: 1}]}
  end

  test "count(distinct column) for empty sets" do
    assert_query "select count(distinct height) from heights",
      %{columns: ["count"], rows: [%{row: [0], occurrences: 1}]}
  end

  test "aggregates of an empty table" do
    assert_query "select count(*), count(height), avg(height) from heights",
      %{columns: ["count", "count", "avg"],
      rows: [%{row: [0, 0, nil], occurrences: 1}]}
  end

  test "should be able to aggregate positive values" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])

    assert_query "select sum(height) from heights",
      %{columns: ["sum"], rows: [%{row: [5400], occurrences: 1}]}

    assert_query "select min(height) from heights",
      %{columns: ["min"], rows: [%{row: [170], occurrences: 1}]}

    assert_query "select max(height) from heights",
      %{columns: ["max"], rows: [%{row: [190], occurrences: 1}]}

    assert_query "select avg(height) from heights",
      %{columns: ["avg"], rows: [%{row: [180.0], occurrences: 1}]}

    assert_query "select stddev(height) from heights",
      %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
    assert_in_delta(stddev, 8.1, 0.1)

    assert_query "select median(height) from heights",
      %{columns: ["median"], rows: [%{row: [179], occurrences: 1}]}
  end

  test "should be able to aggregate qualified columns values" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])

    assert_query "select sum(heights.height) from heights",
      %{columns: ["sum"], rows: [%{row: [5400], occurrences: 1}]}

    assert_query "select min(heights.height) from heights",
      %{columns: ["min"], rows: [%{row: [170], occurrences: 1}]}

    assert_query "select max(heights.height) from heights",
      %{columns: ["max"], rows: [%{row: [190], occurrences: 1}]}

    assert_query "select avg(heights.height) from heights",
      %{columns: ["avg"], rows: [%{row: [180.0], occurrences: 1}]}

    assert_query "select stddev(heights.height) from heights",
      %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
    assert_in_delta(stddev, 8.1, 0.1)

    assert_query "select median(heights.height) from heights",
      %{columns: ["median"], rows: [%{row: [179], occurrences: 1}]}
  end

  test "should be able to aggregate negative values" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [-170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [-190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [-183])

    assert_query "select sum(height) from heights",
      %{columns: ["sum"], rows: [%{row: [-5430], occurrences: 1}]}

    assert_query "select min(height) from heights",
      %{columns: ["min"], rows: [%{row: [-190], occurrences: 1}]}

    assert_query "select max(height) from heights",
      %{columns: ["max"], rows: [%{row: [-170], occurrences: 1}]}

    assert_query "select avg(height) from heights",
      %{columns: ["avg"], rows: [%{row: [-181.0], occurrences: 1}]}

    assert_query "select stddev(height) from heights",
      %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
    assert_in_delta(stddev, 8.29, 0.1)

    assert_query "select median(height) from heights",
      %{columns: ["median"], rows: [%{row: [-184], occurrences: 1}]}
  end

  test "should be able to aggregate negative and positive values" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [-175])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [-190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])

    assert_query "select sum(height) from heights",
      %{columns: ["sum"], rows: [%{row: [-1850], occurrences: 1}]}

    assert_query "select min(height) from heights",
      %{columns: ["min"], rows: [%{row: [-190], occurrences: 1}]}

    assert_query "select max(height) from heights",
      %{columns: ["max"], rows: [%{row: [180], occurrences: 1}]}

    assert_query "select avg(height) from heights",
      %{columns: ["avg"], rows: [%{row: [-61.666666666666664], occurrences: 1}]}

    assert_query "select stddev(height) from heights",
      %{columns: ["stddev"], rows: [%{row: [stddev], occurrences: 1}]}
    assert_in_delta(stddev, 170.99, 0.1)

    assert_query "select median(height) from heights",
      %{columns: ["median"], rows: [%{row: [-176], occurrences: 1}]}
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

  test "should allow ranges for where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height > 170 and height < 190",
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

  test "should allow IN in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height IN (170, 180, 190)",
      %{columns: ["count"], rows: [%{row: [60], occurrences: 1}]}
  end

  test "should drop IN clauses that could expose individuals" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..21, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 22..23, "heights", ["height"], [191])
    :ok = insert_rows(_user_ids = 24..25, "heights", ["height"], [192])
    :ok = insert_rows(_user_ids = 26..27, "heights", ["height"], [193])

    assert_query "select count(*) from heights where height IN (170, 180, 190, 191, 192, 193)",
      %{columns: ["count"], rows: [%{row: [40], occurrences: 1}]}
  end

  test "should not drop NOT IN clauses for which there are not enough users" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..21, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 22..23, "heights", ["height"], [191])
    :ok = insert_rows(_user_ids = 24..25, "heights", ["height"], [192])
    :ok = insert_rows(_user_ids = 26..27, "heights", ["height"], [193])

    assert_query "select count(*) from heights where height NOT IN (170, 180, 190, 191, 192, 193)",
      %{columns: ["count"], rows: [%{row: [8], occurrences: 1}]}
  end

  test "negative filters dominate positive ones" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..21, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 22..23, "heights", ["height"], [191])
    :ok = insert_rows(_user_ids = 24..25, "heights", ["height"], [192])
    :ok = insert_rows(_user_ids = 26..27, "heights", ["height"], [193])

    assert_query """
      SELECT count(*) FROM heights
      WHERE
        height IN (170, 180, 190, 191, 192, 193) AND
        height NOT IN (170, 180, 190, 191, 192, 193)
    """, %{columns: ["count"], rows: [%{row: [8], occurrences: 1}]}
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

    assert_query "select count(*) from heights where height <> 180",
      %{columns: ["count"], rows: [%{row: [40], occurrences: 1}]}
  end

  test "should drop <> conditions if they would expose small groups" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["name"], ["Alice"])
    :ok = insert_rows(_user_ids = 10..11, "heights", ["name"], ["Bob"])

    assert_query "select count(*) from heights where name <> 'Bob'",
      %{columns: ["count"], rows: [%{row: [12], occurrences: 1}]}
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
      date
    end

    assert_query "select date from dates group by date order by date asc",
      %{query_id: "1", columns: ["date"], rows: rows}
    assert (for data <- rows, do: hd(data.row)) == dates

    assert_query "select date from dates group by date order by date desc",
      %{query_id: "1", columns: ["date"], rows: rows}
    assert (for data <- rows, do: hd(data.row)) == Enum.reverse(dates)
  end

  test "query allows mixing aggregated and grouped columns" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height",
      %{columns: ["count", "height"], rows: rows}
    assert Enum.sort_by(rows, &(&1[:row])) ==
      [%{row: [10, 180], occurrences: 1}, %{row: [20, 160], occurrences: 1}]
  end

  test "grouping works when the column is not selected" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*) from heights group by height", %{columns: ["count"], rows: rows}
    assert Enum.sort_by(rows, &(&1[:row])) == [%{row: [10], occurrences: 1}, %{row: [20], occurrences: 1}]
  end

  test "grouping and sorting by a count" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height order by count(*) asc",
      %{columns: ["count", "height"], rows: rows}
    assert rows == [
      %{row: [10, 180], occurrences: 1},
      %{row: [20, 160], occurrences: 1},
      %{row: [30, 150], occurrences: 1}
    ]
  end

  test "ordering hidden values" do
    :ok = insert_rows(_user_ids = 0..2, "heights", ["name"], ["Alice"])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["name"], ["Charlie"])
    :ok = insert_rows(_user_ids = 3..6, "heights", ["name"], ["John"])
    :ok = insert_rows(_user_ids = 7..9, "heights", ["name"], ["Bob"])

    assert_query "select count(*), name from heights group by name order by name asc",
      %{columns: ["count", "name"], rows: rows}
    assert rows == [%{row: [10, "Charlie"], occurrences: 1}, %{row: [10, :*], occurrences: 1}]
  end

  test "grouping and sorting by a grouped field" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height order by height asc",
      %{columns: ["count", "height"], rows: rows}
    assert rows == [
      %{row: [30, 150], occurrences: 1},
      %{row: [20, 160], occurrences: 1},
      %{row: [10, 180], occurrences: 1}
    ]
  end

  test "query which returns zero rows" do
    Cloak.Test.DB.clear_table("heights")
    assert_query "select height from heights", %{query_id: "1", columns: ["height"], rows: []}
  end

  test "select with column alias" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [180])

    assert_query "select height as h from heights group by h order by h",
      %{columns: ["h"], rows: [%{row: [170], occurrences: 1}, %{row: [180], occurrences: 1}]}
    assert_query "select count(*) as c, count(height) as c from heights",
      %{columns: ["c", "c"], rows: [%{row: [30, 30], occurrences: 1}]}
  end

  test "select comparing two columns" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights where height = height",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
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

  test "same database columns are selected only once in implicit self-join" do
    {:ok, query} = Cloak.Aql.Query.make(
      Cloak.DataSource.fetch!(hd(Cloak.DataSource.ids())),
      "
        select heights.height as h1, heights_alias.height as h2
        from heights, heights_alias
        where heights.user_id=heights_alias.user_id
      "
    )
    assert [%Column{name: "user_id"}, %Column{name: "height"}] = query.db_columns
  end

  test "cast" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [10])
    assert_query "select cast(height as text) from heights",
      %{columns: ["cast"], rows: [%{row: ["10"], occurrences: 10}]}
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

  test "limit and offset" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name"], ["aaa"])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name"], ["bbb"])

    assert_query "select name from heights order by name limit 5",
      %{columns: ["name"], rows: [%{row: ["aaa"], occurrences: 5}]}
    assert_query "select name from heights order by name offset 15",
      %{columns: ["name"], rows: [%{row: ["bbb"], occurrences: 5}]}
    assert_query "select name from heights order by name limit 10 offset 5",
      %{columns: ["name"], rows: [%{row: ["aaa"], occurrences: 5}, %{row: ["bbb"], occurrences: 5}]}
  end

  test "should be able to provide noise estimates for count, sum, avg and stddev aggregators" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])

    assert_query "select count_noise(*) from heights",
      %{columns: ["count_noise"], rows: [%{row: [0], occurrences: 1}]}

    assert_query "select sum_noise(height) from heights",
      %{columns: ["sum_noise"], rows: [%{row: [0], occurrences: 1}]}

    assert_query "select avg_noise(height) from heights",
      %{columns: ["avg_noise"], rows: [%{row: [0.0], occurrences: 1}]}

    assert_query "select stddev_noise(height) from heights",
      %{columns: ["stddev_noise"], rows: [%{row: [0.0], occurrences: 1}]}
  end
end
