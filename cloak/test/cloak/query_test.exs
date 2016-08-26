defmodule Cloak.QueryTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers
  alias Cloak.Aql.Column

  setup_all do
    Cloak.Test.DB.setup()
    Cloak.Test.DB.create_test_schema()
    :ok = Cloak.Test.DB.create_table("heights", "height INTEGER, name TEXT, male BOOLEAN")
    :ok = Cloak.Test.DB.create_table("datetimes", "datetime TIMESTAMP, date_only DATE, time_only TIME")
    :ok = Cloak.Test.DB.create_table("floats", "float REAL")
    :ok = Cloak.Test.DB.create_table("heights_alias", nil, db_name: "heights", skip_db_create: true)
    :ok = Cloak.Test.DB.create_table("purchases", "price INTEGER, name TEXT, datetime TIMESTAMP")
    :ok = Cloak.Test.DB.create_table("children", "age INTEGER, name TEXT")
    :ok = Cloak.Test.DB.create_table("weird things", "\"thing as thing\" INTEGER", db_name: "weird")
    :ok
  end

  describe "basic querying" do
    setup [:clear_heights]

    test "show tables" do
      assert_query "show tables", %{columns: ["name"], rows: [
        %{occurrences: 1, row: [:children]},
        %{occurrences: 1, row: [:datetimes]},
        %{occurrences: 1, row: [:floats]},
        %{occurrences: 1, row: [:heights]},
        %{occurrences: 1, row: [:heights_alias]},
        %{occurrences: 1, row: [:purchases]},
        %{occurrences: 1, row: [:"weird things"]},
      ]}
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
        Cloak.DataSource.fetch!(:local),
        "
          select heights.height as h1, heights_alias.height as h2
          from heights, heights_alias
          where heights.user_id=heights_alias.user_id
        "
      )
      assert [%Column{name: "user_id"}, %Column{name: "height"}] = query.db_columns
    end
  end

  describe "error handling" do
    test "query reports an error on invalid where clause identifier" do
      assert_query "select height from heights where nonexistant > 10", %{error: error}
      assert ~s/Column `nonexistant` doesn't exist in table `heights`./ == error
    end

    test "query reports an error on invalid order by field" do
      assert_query "select height from heights order by name", %{error: error}
      assert ~s/Non-selected column `name` from table `heights` specified in `order by` clause./ == error
    end

    test "query reports an error on unknown function" do
      assert_query "select invalid_function(height) from heights", %{error: error}
      assert ~s/Unknown function `invalid_function`./ == error
    end

    test "reports an error on wrong cast" do
      assert_query "select * from datetimes where datetime > 0", %{error: error}
      assert ~s/Cannot cast `0` to timestamp./ == error
    end

    test "reports an error on ambigous usage of an alias occurring multiple times" do
      assert_query "select count(*) as x, count(height) as x from heights order by x", %{error: error}
      assert ~s/Usage of `x` is ambiguous./ == error
    end

    test "query reports an error on invalid statement" do
      assert_query "invalid statement", %{error: "Expected `select or show` at line 1, column 1."}
    end

    test "query reports an error on invalid column" do
      assert_query "select invalid_column from heights", %{error: error}
      assert ~s/Column `invalid_column` doesn't exist in table `heights`./ == error
    end

    test "query reports an error on invalid table" do
      assert_query "select column from invalid_table", %{error: error}
      assert ~s/Table `invalid_table` doesn't exist./ == error
    end

    test "query reports an error when mixing aggregated and normal columns" do
      assert_query "select count(*), height from heights", %{error: error}
      assert error =~ ~r/`height` from table `heights` needs to appear in the `group by` clause/
    end

    test "query reports an error when grouping by nonexistent columns" do
      assert_query "select count(*) from heights group by nothing", %{error: error}
      assert error =~ ~r/Column `nothing` doesn't exist in table `heights`./
    end

    test "query reports an error when not grouping by some selected columns" do
      assert_query "select name, height from heights group by height", %{error: error}
      assert error =~ ~r/`name` from table `heights` needs to appear in the `group by` clause/
    end

    test "query reports an error on runner crash" do
      ExUnit.CaptureLog.capture_log(fn ->
        assert_query :invalid_query_type, %{error: "Cloak error"}
      end)
    end

    test "warns when uid column is selected" do
      assert_info "select user_id from heights", "`user_id` from table `heights`"
      assert_info "select user_id, height from heights", "`user_id` from table `heights`"
      assert_info "select * from heights", "`user_id` from table `heights`"

      assert_query "select * from heights, purchases where heights.user_id = purchases.user_id", %{info: [info1, info2]}
      assert info1 =~ "`user_id` from table `heights`"
      assert info2 =~ "`user_id` from table `purchases`"
    end

    test "substring with neither for nor from" do
      assert_query "select substring(name) from heights", %{error: error}
      assert error == "Function `substring` requires arguments of type (`text`, `integer`, [`integer`]),"
        <> " but got (`text`)"
    end
  end

  describe "JOINs" do
    setup [:clear_heights, :clear_children, :clear_purchases]

    test "selecting from multiple tables" do
      :ok = insert_rows(_user_ids = 0..100, "heights", ["height"], [180])
      :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

      assert_query "select max(height), max(price) FROM heights, purchases WHERE heights.user_id = purchases.user_id",
        %{columns: ["max", "max"], rows: rows}
      assert rows == [%{row: [180, 200], occurrences: 1}]
    end

    test "selecting using INNER JOIN" do
      :ok = insert_rows(_user_ids = 0..100, "heights", ["height"], [180])
      :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

      assert_query """
        SELECT max(height), max(price)
        FROM heights INNER JOIN purchases ON heights.user_id = purchases.user_id
      """,
        %{columns: ["max", "max"], rows: rows}
      assert rows == [%{row: [180, 200], occurrences: 1}]
    end

    for negative_condition <- ["<>", "NOT LIKE", "NOT ILIKE"] do
      test "#{negative_condition} not supported in a join" do
        assert_query """
          SELECT max(height), max(price)
          FROM heights JOIN purchases ON heights.user_id = purchases.user_id AND
          heights.user_id #{unquote(negative_condition)} ''
        """,
          %{error: "#{unquote(negative_condition)} not supported in joins."}
      end
    end

    test "selecting using complex JOIN" do
      :ok = insert_rows(_user_ids = 0..100, "heights", ["height"], [180])
      :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])
      :ok = insert_rows(_user_ids = 0..100, "children", ["age"], [20])

      assert_query """
        SELECT max(height), max(price), max(age)
        FROM
          heights INNER JOIN purchases ON heights.user_id = purchases.user_id,
          children
        WHERE children.user_id = purchases.user_id
      """, %{columns: ["max", "max", "max"], rows: rows}
      assert rows == [%{row: [180, 200, 20], occurrences: 1}]
    end

    test "selecting using LEFT OUTER JOIN" do
      :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
      :ok = insert_rows(_user_ids = 1..50, "children", ["age"], [20])

      assert_query """
        SELECT count(*)
        FROM heights LEFT OUTER JOIN children ON heights.user_id = children.user_id
      """, %{columns: ["count"], rows: rows}
      assert rows == [%{row: [100], occurrences: 1}]
    end

    test "selecting using RIGHT OUTER JOIN" do
      :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
      :ok = insert_rows(_user_ids = 1..50, "children", ["age"], [20])

      assert_query """
        SELECT count(*)
        FROM heights RIGHT OUTER JOIN children ON heights.user_id = children.user_id
      """, %{columns: ["count"], rows: rows}
      assert rows == [%{row: [50], occurrences: 1}]
    end

    test "selecting using FULL OUTER JOIN" do
      :ok = insert_rows(_user_ids = 1..50, "heights", ["height"], [180])
      :ok = insert_rows(_user_ids = 101..150, "children", ["age"], [20])

      assert_query """
        SELECT count(*)
        FROM heights FULL OUTER JOIN children ON heights.user_id = children.user_id
      """, %{columns: ["count"], rows: rows}
      assert rows == [%{row: [100], occurrences: 1}]
    end
  end

  describe "date/time handling" do
    setup [:clear_datetimes]

    test "select date parts" do
      time = %Postgrex.Timestamp{year: 2015, month: 1, day: 2}
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [time])

      assert_query "select year(datetime), month(datetime), day(datetime) from datetimes group by datetime",
        %{columns: ["year", "month", "day"], rows: [%{occurrences: 1, row: [2015, 1, 2]}]}
    end

    test "select date parts of the LCF bucket" do
      time = %Postgrex.Timestamp{year: 2015, month: 1}
      for number <- 1..10 do
        :ok = insert_rows(_user_ids = number..number, "datetimes", ["datetime"], [%{time | day: number}])
      end

      assert_query "select day(datetime) from datetimes group by datetime",
        %{columns: ["day"], rows: [%{occurrences: 1, row: [:*]}]}
    end

    test "anonymization over date parts" do
      time1 = %Postgrex.Timestamp{year: 2015, month: 1, day: 2}
      time2 = %Postgrex.Timestamp{year: 2015, month: 1, day: 3}
      time3 = %Postgrex.Timestamp{year: 2016, month: 1, day: 3}
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [time1])
      :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [time2])
      :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [time3])

      assert_query "select year(datetime) from datetimes",
        %{columns: ["year"], rows: [%{occurrences: 20, row: [2015]}, %{occurrences: 10, row: [2016]}]}
    end

    test "grouping by a date part" do
      time1 = %Postgrex.Timestamp{year: 2015, month: 1, day: 2}
      time2 = %Postgrex.Timestamp{year: 2015, month: 1, day: 3}
      time3 = %Postgrex.Timestamp{year: 2016, month: 1, day: 3}
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [time1])
      :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [time2])
      :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [time3])

      assert_query "select count(*), year(datetime) from datetimes group by year(datetime) order by count(*)",
        %{columns: ["count", "year"], rows: [
          %{occurrences: 1, row: [10, 2016]},
          %{occurrences: 1, row: [20, 2015]},
        ]}
    end

    test "grouping by an aliased date part" do
      time1 = %Postgrex.Timestamp{year: 2015, month: 1, day: 2}
      time2 = %Postgrex.Timestamp{year: 2015, month: 1, day: 3}
      time3 = %Postgrex.Timestamp{year: 2016, month: 1, day: 3}
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [time1])
      :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [time2])
      :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [time3])

      assert_query "select count(*), year(datetime) as the_year from datetimes group by the_year order by count(*)",
        %{columns: ["count", "the_year"], rows: [
          %{occurrences: 1, row: [10, 2016]},
          %{occurrences: 1, row: [20, 2015]},
        ]}
    end

    test "comparing a timestamp" do
      early = %Postgrex.Timestamp{year: 2015}
      late = %Postgrex.Timestamp{year: 2017}
      :ok = insert_rows(_user_ids = 0..9, "datetimes", ["datetime"], [early])
      :ok = insert_rows(_user_ids = 10..19, "datetimes", ["datetime"], [late])

      assert_query "select count(*) from datetimes where datetime > '2016-01-01'",
        %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
    end

    test "comparing a timestamp with <>" do
      early = %Postgrex.Timestamp{year: 2015, month: 1, day: 1}
      late = %Postgrex.Timestamp{year: 2017, month: 1, day: 1}
      :ok = insert_rows(_user_ids = 0..9, "datetimes", ["datetime"], [early])
      :ok = insert_rows(_user_ids = 10..19, "datetimes", ["datetime"], [late])

      assert_query "select count(*) from datetimes where datetime <> '2015-01-01'",
        %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
    end

    test "comparing a time" do
      early = %Postgrex.Time{hour: 1}
      late = %Postgrex.Time{hour: 10}
      :ok = insert_rows(_user_ids = 0..9, "datetimes", ["time_only"], [early])
      :ok = insert_rows(_user_ids = 10..19, "datetimes", ["time_only"], [late])

      assert_query "select count(*) from datetimes where time_only > '05:00:00'",
        %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
    end

    test "comparing a date" do
      early = %Postgrex.Date{year: 2015, month: 1, day: 1}
      late = %Postgrex.Date{year: 2017, month: 1, day: 1}
      :ok = insert_rows(_user_ids = 0..9, "datetimes", ["date_only"], [early])
      :ok = insert_rows(_user_ids = 10..19, "datetimes", ["date_only"], [late])

      assert_query "select count(*) from datetimes where date_only > '2016-01-01'",
        %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
    end

    test "selecting time" do
      time = %Postgrex.Time{hour: 1, min: 2, sec: 3}
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["time_only"], [time])

      assert_query "select time_only from datetimes",
        %{rows: [%{row: [~T[01:02:03.000000]]}]}
    end

    test "selecting date" do
      time = %Postgrex.Date{year: 1, month: 2, day: 3}
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["date_only"], [time])

      assert_query "select date_only from datetimes",
        %{rows: [%{row: [~D[0001-02-03]]}]}
    end

    test "selecting datetime" do
      time = %Postgrex.Timestamp{year: 2015, month: 1, day: 2, hour: 3, min: 4, sec: 5}
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [time])

      assert_query "select datetime from datetimes", %{rows: [%{row: [~N[2015-01-02 03:04:05.000000]]}]}
    end
  end

  describe "float handling" do
    setup [:clear_floats]

    test "unary trunc" do
      :ok = insert_rows(_user_ids = 1..10, "floats", ["float"], [12.234])
      assert_query "select trunc(float) from floats",
        %{columns: ["trunc"], rows: [%{occurrences: 10, row: [12]}]}
    end

    test "binary trunc" do
      :ok = insert_rows(_user_ids = 1..10, "floats", ["float"], [12.234])
      assert_query "select trunc(float, 2) from floats",
        %{columns: ["trunc"], rows: [%{occurrences: 10, row: [12.23]}]}
    end

    test "binary trunc in a grouped query" do
      :ok = insert_rows(_user_ids = 1..10, "floats", ["float"], [12.234])
      assert_query "select trunc(float, 2) from floats group by float",
        %{columns: ["trunc"], rows: [%{occurrences: 1, row: [12.23]}]}
    end

    test "arithmetic expressions" do
      :ok = insert_rows(_user_ids = 1..10, "floats", ["float"], [4])
      assert_query "select 2 ^ 3 * (3 + 4 - 1) / 5 from floats",
        %{columns: [_], rows: [%{row: [9.6]}]}
    end

    test "function on an aggregated value" do
      :ok = insert_rows(_user_ids = 1..10, "floats", ["float"], [4])
      :ok = insert_rows(_user_ids = 11..100, "floats", ["float"], [9])
      assert_query "select round(avg(float)) from floats",
        %{columns: ["round"], rows: [%{row: [9], occurrences: 1}]}
    end

    test "nested function call" do
      :ok = insert_rows(_user_ids = 1..10, "floats", ["float"], [-4.2])
      assert_query "select sqrt(abs(round(float))) from floats",
        %{columns: ["sqrt"], rows: [%{row: [value], occurrences: 10}]}
      assert_in_delta value, 2, 0.1
    end

    test "aggregating a function" do
      :ok = insert_rows(_user_ids = 1..10, "floats", ["float"], [4])
      :ok = insert_rows(_user_ids = 11..20, "floats", ["float"], [9])
      assert_query "select avg(sqrt(float)) from floats",
        %{columns: ["avg"], rows: [%{row: [value], occurrences: 1}]}
      assert_in_delta value, 2.5, 0.01
    end
  end

  describe "quoting" do
    test "quoting table and column names" do
      assert_query "select \"thing as thing\" from \"weird things\"",
        %{columns: ["thing as thing"], rows: []}
    end
  end

  describe "miscellaneous" do
    setup [:clear_heights]

    test "SQL injection" do
      :ok = insert_rows(_user_ids = 1..10, "heights", ["height", "name"], [10, "jon"])
      assert_query ~S(select height from heights where name = '\' or \'1\'=\'1'),
        %{columns: ["height"], rows: []}
      assert_query ~S(select height from heights where name = ''' or ''1''=''1'),
        %{columns: ["height"], rows: []}
    end
  end
end
