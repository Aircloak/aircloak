defmodule Cloak.QueryTest do
  use ExUnit.Case, async: false

  alias Cloak.Query

  defmacrop assert_query(query, expected_response) do
    quote do
      :ok = start_query(unquote(query))
      assert_receive {:reply, unquote(expected_response)}
    end
  end

  setup_all do
    :db_test.setup()
    :db_test.create_test_schema()
    :db_test.create_table("heights", "height INTEGER, name TEXT, time TIMESTAMP")
    :ok
  end

  setup do
    :db_test.clear_table("heights")
    :ok
  end

  test "show tables" do
    assert_query "show tables", %{columns: ["name"], rows: [%{occurrences: 1, row: [:heights]}]}
  end

  test "show columns" do
    assert_query "show columns from heights", %{query_id: "1", columns: ["name", "type"], rows: rows}
    assert Enum.sort_by(rows, &(&1[:row])) == [
      %{occurrences: 1, row: ["height", :integer]},
      %{occurrences: 1, row: ["name", :text]},
      %{occurrences: 1, row: ["time", :timestamp]}
    ]
  end

  test "simple select query" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from heights", %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "select all query" do
    assert_query "select * from heights", %{query_id: "1", columns: ["height", "name", "time"], rows: _}
  end

  test "select all and order query" do
    time = %Postgrex.Timestamp{year: 2015, month: 1, day: 1}
    :ok = insert_rows(_user_ids = 1..10, "heights", ["name", "height", "time"], ["john", 180, time])
    :ok = insert_rows(_user_ids = 11..20, "heights", ["name", "height"], ["adam", 180])
    :ok = insert_rows(_user_ids = 21..30, "heights", ["name", "height"], ["mike", 180])

    assert_query "select * from heights order by name",
      %{query_id: "1", columns: ["height", "name", "time"], rows: rows}
    assert Enum.map(rows, &(&1[:row])) == [
      [180, "adam", nil],
      [180, "john", %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}],
      [180, "mike", nil]
    ]
  end

  test "should return LCF property when sufficient rows are filtered" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    for id <- 5..10 do
      range = (id * 4)..(id * 4 + 2)
      assert :ok = insert_rows(_user_ids = range, "heights", ["height"], [100 + id])
    end

    assert_query "select height from heights", %{columns: ["height"], rows: rows}
    assert Enum.sort_by(rows, &(&1[:row])) == [
      %{row: [180], occurrences: 20},
      %{row: [:*], occurrences: 18}
    ]
  end

  test "should produce counts" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [nil])

    assert_query "select count(*) from heights",
      %{columns: ["count(*)"], rows: [%{row: [40], occurrences: 1}]}

    assert_query "select COUNT(height) from heights",
      %{columns: ["count(height)"], rows: [%{row: [20], occurrences: 1}]}
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

    assert_query "select count(distinct height) from heights",
      %{columns: ["count(distinct height)"], rows: [%{row: [5], occurrences: 1}]}
  end

  test "count(distinct column) for empty sets" do
    assert_query "select count(distinct height) from heights",
      %{columns: ["count(distinct height)"], rows: [%{row: [0], occurrences: 1}]}
  end

  test "aggregates of an empty table" do
    assert_query "select count(*), count(height), avg(height) from heights",
      %{columns: ["count(*)", "count(height)", "avg(height)"], rows: [%{row: [0, 0, nil], occurrences: 1}]}
  end

  test "should be able to aggregate positive values" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])

    assert_query "select sum(height) from heights",
      %{columns: ["sum(height)"], rows: [%{row: [5400], occurrences: 1}]}

    assert_query "select min(height) from heights",
      %{columns: ["min(height)"], rows: [%{row: [170], occurrences: 1}]}

    assert_query "select max(height) from heights",
      %{columns: ["max(height)"], rows: [%{row: [190], occurrences: 1}]}

    assert_query "select avg(height) from heights",
      %{columns: ["avg(height)"], rows: [%{row: [180.0], occurrences: 1}]}

    assert_query "select stddev(height) from heights",
      %{columns: ["stddev(height)"], rows: [%{row: [stddev], occurrences: 1}]}
    assert_in_delta(stddev, 8.1, 0.1)

    assert_query "select median(height) from heights",
      %{columns: ["median(height)"], rows: [%{row: [180], occurrences: 1}]}
  end

  test "should be able to aggregate negative values" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [-170])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [-190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [-183])

    assert_query "select sum(height) from heights",
      %{columns: ["sum(height)"], rows: [%{row: [-5430], occurrences: 1}]}

    assert_query "select min(height) from heights",
      %{columns: ["min(height)"], rows: [%{row: [-190], occurrences: 1}]}

    assert_query "select max(height) from heights",
      %{columns: ["max(height)"], rows: [%{row: [-170], occurrences: 1}]}

    assert_query "select avg(height) from heights",
      %{columns: ["avg(height)"], rows: [%{row: [-181.0], occurrences: 1}]}

    assert_query "select stddev(height) from heights",
      %{columns: ["stddev(height)"], rows: [%{row: [stddev], occurrences: 1}]}
    assert_in_delta(stddev, 8.29, 0.1)

    assert_query "select median(height) from heights",
      %{columns: ["median(height)"], rows: [%{row: [-183], occurrences: 1}]}
  end

  test "should be able to aggregate negative and positive values" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["height"], [-175])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height"], [-190])
    :ok = insert_rows(_user_ids = 30..39, "heights", ["height"], [180])

    assert_query "select sum(height) from heights",
      %{columns: ["sum(height)"], rows: [%{row: [-1850], occurrences: 1}]}

    assert_query "select min(height) from heights",
      %{columns: ["min(height)"], rows: [%{row: [-190], occurrences: 1}]}

    assert_query "select max(height) from heights",
      %{columns: ["max(height)"], rows: [%{row: [180], occurrences: 1}]}

    assert_query "select avg(height) from heights",
      %{columns: ["avg(height)"], rows: [%{row: [-61.666666666666664], occurrences: 1}]}

    assert_query "select stddev(height) from heights",
      %{columns: ["stddev(height)"], rows: [%{row: [stddev], occurrences: 1}]}
    assert_in_delta(stddev, 170.99, 0.1)

    assert_query "select median(height) from heights",
      %{columns: ["median(height)"], rows: [%{row: [-175], occurrences: 1}]}
  end

  test "should return nil when not enough values present for anonymization" do
    :ok = insert_rows(_user_ids = 0..8, "heights", ["height"], [180])

    assert_query "select median(height) from heights",
      %{columns: ["median(height)"], rows: [%{row: [nil], occurrences: 1}]}
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
        columns: ["count(height)", "count(*)", "count(*)", "count(height)"],
        rows: [%{row: [100, 100, 100, 100], occurrences: 1}]
      }
  end

  test "different aggregates on the same column" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])

    assert_query "select count(height), max(height) from heights",
      %{columns: ["count(height)", "max(height)"], rows: [%{row: [100, 180], occurrences: 1}]}
  end

  test "should allow ranges for where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height > 170 and height < 190",
      %{query_id: "1", columns: ["count(*)"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "comparing a timestamp" do
    early = %Postgrex.Timestamp{year: 2015}
    late = %Postgrex.Timestamp{year: 2017}
    :ok = insert_rows(_user_ids = 0..9, "heights", ["time"], [early])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["time"], [late])

    assert_query "select count(*) from heights where time > '2016-01-01'",
      %{query_id: "1", columns: ["count(*)"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "comparing a timestamp with <>" do
    early = %Postgrex.Timestamp{year: 2015, month: 1, day: 1}
    late = %Postgrex.Timestamp{year: 2017, month: 1, day: 1}
    :ok = insert_rows(_user_ids = 0..9, "heights", ["time"], [early])
    :ok = insert_rows(_user_ids = 10..19, "heights", ["time"], [late])

    assert_query "select count(*) from heights where time <> '2015-01-01'",
      %{query_id: "1", columns: ["count(*)"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should allow LIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "bob"])

    assert_query "select count(*) from heights where name LIKE 'b%'",
      %{columns: ["count(*)"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow NOT LIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "bob"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "alice"])

    assert_query "select count(*) from heights where name NOT LIKE 'b%'",
      %{columns: ["count(*)"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should allow NOT ILIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "Bob"])
    :ok = insert_rows(_user_ids = 20..29, "heights", ["height", "name"], [170, "alice"])

    assert_query "select count(*) from heights where name NOT ILIKE 'b%'",
      %{columns: ["count(*)"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should allow ILIKE in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height", "name"], [170, "Bob"])

    assert_query "select count(*) from heights where name ILIKE 'b%'",
      %{columns: ["count(*)"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow IN in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height IN (170, 180, 190)",
      %{columns: ["count(*)"], rows: [%{row: [60], occurrences: 1}]}
  end

  test "should allow <> in where clause" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 20..39, "heights", ["height"], [190])

    assert_query "select count(*) from heights where height <> 180",
      %{columns: ["count(*)"], rows: [%{row: [40], occurrences: 1}]}
  end

  test "should drop <> conditions if they would expose small groups" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["name"], ["Alice"])
    :ok = insert_rows(_user_ids = 10..11, "heights", ["name"], ["Bob"])

    assert_query "select count(*) from heights where name <> 'Bob'",
      %{columns: ["count(*)"], rows: [%{row: [12], occurrences: 1}]}
  end

  test "<> conditions count unique users" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["name"], ["Alice"])
    1..10 |> Enum.each(fn _ -> :ok = insert_rows(_user_ids = [10], "heights", ["name"], ["Bob"]) end)

    assert_query "select count(*) from heights where name <> 'Bob'",
      %{query_id: "1", columns: ["count(*)"], rows: [%{row: [20], occurrences: 1}]}
  end

  test "should allow IS NULL in where clause" do
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [180])

    assert_query "select count(*) from heights where height IS NULL",
      %{columns: ["count(*)"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should allow IS NOT NULL in where clause" do
    :ok = insert_rows(_user_ids = 1..20, "heights", ["height"], [nil])
    :ok = insert_rows(_user_ids = 1..10, "heights", ["height"], [180])

    assert_query "select count(*) from heights where height IS NOT NULL",
      %{columns: ["count(*)"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "should order rows when instructed" do
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [190])
    :ok = insert_rows(_user_ids = 0..19, "heights", ["height"], [170])

    assert_query "select height from heights order by height",
      %{query_id: "1", columns: ["height"], rows: rows}
    assert rows == Enum.sort(rows)
  end

  test "query reports an error on invalid statement" do
    assert_query "invalid statement", %{error: "Expected `select or show` at line 1, column 1."}
  end

  test "query reports an error on invalid column" do
    assert_query "select invalid_column from heights", %{error: error}
    assert ~s/Column `invalid_column` doesn't exist./ == error
  end

  test "query reports an error on invalid table" do
    assert_query "select column from invalid_table", %{error: error}
    assert ~s/Table `invalid_table` doesn't exist./ == error
  end

  test "query reports an error when mixing aggregated and normal columns" do
    assert_query "select count(*), height from heights", %{error: error}
    assert error =~ ~r/`height` needs to appear in the `group by` clause/
  end

  test "query reports an error when grouping by nonexistent columns" do
    assert_query "select count(*) from heights group by nothing", %{error: error}
    assert error =~ ~r/Column `nothing` doesn't exist./
  end

  test "query reports an error when not grouping by some selected columns" do
    assert_query "select name, height from heights group by height", %{error: error}
    assert error =~ ~r/`name` needs to appear in the `group by` clause/
  end

  test "query allows mixing aggregated and grouped columns" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height",
      %{columns: ["count(*)", "height"], rows: rows}
    assert Enum.sort_by(rows, &(&1[:row])) ==
      [%{row: [10, 180], occurrences: 1}, %{row: [20, 160], occurrences: 1}]
  end

  test "grouping works when the column is not selected" do
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*) from heights group by height", %{columns: ["count(*)"], rows: rows}
    assert Enum.sort_by(rows, &(&1[:row])) == [%{row: [10], occurrences: 1}, %{row: [20], occurrences: 1}]
  end

  test "grouping and sorting by a count" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height order by count(*) asc",
      %{columns: ["count(*)", "height"], rows: rows}
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
      %{columns: ["count(*)", "name"], rows: rows}
    assert rows == [%{row: [10, "Charlie"], occurrences: 1}, %{row: [10, :*], occurrences: 1}]
  end

  test "grouping and sorting by a grouped field" do
    :ok = insert_rows(_user_ids = 30..59, "heights", ["height"], [150])
    :ok = insert_rows(_user_ids = 0..9, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 10..29, "heights", ["height"], [160])

    assert_query "select count(*), height from heights group by height order by height asc",
      %{columns: ["count(*)", "height"], rows: rows}
    assert rows == [
      %{row: [30, 150], occurrences: 1},
      %{row: [20, 160], occurrences: 1},
      %{row: [10, 180], occurrences: 1}
    ]
  end

  test "query reports an error on invalid where clause identifier" do
    assert_query "select height from heights where nonexistant > 10", %{error: error}
    assert ~s/Column `nonexistant` doesn't exist./ == error
  end

  test "query reports an error on invalid order by field" do
    assert_query "select height from heights order by age", %{error: error}
    assert ~s/Non-selected field `age` specified in `order by` clause./ == error
  end

  test "query reports an error on unknown function" do
    assert_query "select invalid_function(height) from heights", %{error: error}
    assert ~s/Unknown function `invalid_function`./ == error
  end

  test "reports an error on wrong cast" do
    assert_query "select * from heights where time > 0", %{error: error}
    assert ~s/Cannot cast `0` to timestamp./ == error
  end

  test "query reports an error on runner crash" do
    ExUnit.CaptureLog.capture_log(fn ->
      assert_query :invalid_query_type, %{error: "Cloak error"}
    end)
  end

  test "query which returns zero rows" do
    :db_test.clear_table("heights")
    assert_query "select height from heights", result
    assert %{query_id: "1", columns: ["height"], rows: rows} = result
    assert [%{occurrences: 0, row: [nil]}] == rows
  end

  defp start_query(statement) do
    %Query{id: "1", statement: statement, data_source: Cloak.DataSource.fetch!(:local)}
    |> Query.start({:process, self()})
  end

  defp insert_rows(user_id_range, table, columns, values) do
    :db_test.add_users_data(
      Enum.map(user_id_range,
        &{"user#{&1}", [
          {table, [
            columns: columns,
            data: [values]
          ]}
        ]}
      )
    )
  end
end
