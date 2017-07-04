defmodule Cloak.Query.DatetimeTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("datetimes", "datetime TIMESTAMP, date_only DATE, time_only TIME")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("datetimes")
    :ok
  end

  test "select date parts" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])

    assert_query "select year(datetime), month(datetime), day(datetime) from datetimes group by datetime",
      %{columns: ["year", "month", "day"], rows: [%{occurrences: 1, row: [2015, 1, 2]}]}
  end

  test "select date parts of the LCF bucket" do
    for number <- 1..10 do
      time = NaiveDateTime.from_erl!({{2015, 1, number}, {0, 0, 0}})
      :ok = insert_rows(_user_ids = number..number, "datetimes", ["datetime"], [time])
    end

    assert_query "select day(datetime) from datetimes group by datetime",
      %{columns: ["day"], rows: [%{occurrences: 1, row: [:*]}]}
  end

  test "anonymization over date parts" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])
    :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [~N[2015-01-03 00:00:00]])
    :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [~N[2016-01-03 00:00:00]])

    assert_query "select year(datetime) from datetimes",
      %{columns: ["year"], rows: [%{occurrences: 20, row: [2015]}, %{occurrences: 10, row: [2016]}]}
  end

  test "grouping by a date part" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])
    :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [~N[2015-01-03 00:00:00]])
    :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [~N[2016-01-03 00:00:00]])

    assert_query "select count(*), year(datetime) from datetimes group by year(datetime) order by count(*)",
      %{columns: ["count", "year"], rows: [
        %{occurrences: 1, row: [10, 2016]},
        %{occurrences: 1, row: [20, 2015]},
      ]}
  end

  test "grouping by an aliased date part" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])
    :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [~N[2015-01-03 00:00:00]])
    :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [~N[2016-01-03 00:00:00]])

    assert_query "select count(*), year(datetime) as the_year from datetimes group by the_year order by count(*)",
      %{columns: ["count", "the_year"], rows: [
        %{occurrences: 1, row: [10, 2016]},
        %{occurrences: 1, row: [20, 2015]},
      ]}
  end

  test "comparing a timestamp" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["datetime"], [~N[2015-01-01 00:00:00]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["datetime"], [~N[2017-01-01 00:00:00]])

    assert_query "select count(*) from datetimes where datetime >= '2016-06-01' and datetime < '2017-06-01'",
      %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "comparing a timestamp with <>" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["datetime"], [~N[2015-01-01 00:00:00]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["datetime"], [~N[2017-01-01 00:00:00]])

    assert_query "select count(*) from datetimes where datetime <> '2015-01-01'",
      %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "comparing a time" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["time_only"], [~T[01:00:00]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["time_only"], [~T[10:00:00]])

    assert_query "select count(*) from datetimes where time_only > '09:00:00' and time_only < '11:00:00'",
      %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "comparing a date" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["date_only"], [~D[2015-01-01]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["date_only"], [~D[2017-01-01]])

    assert_query "select count(*) from datetimes where date_only > '2016-01-01' and date_only < '2018-01-01'",
      %{query_id: "1", columns: ["count"], rows: [%{row: [10], occurrences: 1}]}
  end

  test "selecting time" do
    time = ~T[01:02:03.000000]
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["time_only"], [time])

    assert_query "select time_only from datetimes", %{rows: [%{row: [time_string]}]}
    assert time_string == Time.to_iso8601(time)
  end

  test "selecting date" do
    date = ~D[0001-02-03]
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["date_only"], [date])

    assert_query "select date_only from datetimes", %{rows: [%{row: [date_string]}]}
    assert date_string == Date.to_iso8601(date)
  end

  test "selecting datetime" do
    date_time = ~N[2015-01-02 03:04:05.000000]
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [date_time])

    assert_query "select datetime from datetimes", %{rows: [%{row: [date_time_string]}]}
    assert date_time_string == NaiveDateTime.to_iso8601(date_time)
  end
end
