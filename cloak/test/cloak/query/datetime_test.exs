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
