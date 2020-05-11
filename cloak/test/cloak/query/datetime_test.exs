defmodule Cloak.Query.DatetimeTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok =
      Cloak.Test.DB.create_table(
        "datetimes",
        "datetime TIMESTAMP, date_only DATE, time_only TIME"
      )

    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("datetimes")
    :ok
  end

  test "select date parts" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])

    assert_query("select year(datetime), month(datetime), day(datetime) from datetimes group by datetime", %{
      columns: ["year", "month", "day"],
      rows: [%{occurrences: 1, row: [2015, 1, 2]}]
    })
  end

  test "select date parts of the LCF bucket" do
    for number <- 1..10 do
      time = NaiveDateTime.from_erl!({{2015, 1, number}, {0, 0, 0}})
      :ok = insert_rows(_user_ids = number..number, "datetimes", ["datetime"], [time])
    end

    assert_query("select day(datetime) from datetimes group by datetime", %{
      columns: ["day"],
      rows: [%{occurrences: 1, row: [:*]}]
    })
  end

  test "anonymization over date parts" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])
    :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [~N[2015-01-03 00:00:00]])
    :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [~N[2016-01-03 00:00:00]])

    assert_query("select year(datetime) from datetimes", %{
      columns: ["year"],
      rows: [%{occurrences: 20, row: [2015]}, %{occurrences: 10, row: [2016]}]
    })
  end

  test "grouping by a date part" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])
    :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [~N[2015-01-03 00:00:00]])
    :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [~N[2016-01-03 00:00:00]])

    assert_query("select count(*), year(datetime) from datetimes group by year(datetime) order by count(*)", %{
      columns: ["count", "year"],
      rows: [
        %{occurrences: 1, row: [10, 2016]},
        %{occurrences: 1, row: [20, 2015]}
      ]
    })
  end

  test "grouping by an aliased date part" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 00:00:00]])
    :ok = insert_rows(_user_ids = 11..20, "datetimes", ["datetime"], [~N[2015-01-03 00:00:00]])
    :ok = insert_rows(_user_ids = 21..30, "datetimes", ["datetime"], [~N[2016-01-03 00:00:00]])

    assert_query("select count(*), year(datetime) as the_year from datetimes group by the_year order by count(*)", %{
      columns: ["count", "the_year"],
      rows: [
        %{occurrences: 1, row: [10, 2016]},
        %{occurrences: 1, row: [20, 2015]}
      ]
    })
  end

  test "comparing a timestamp" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["datetime"], [~N[2015-01-01 00:00:00]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["datetime"], [~N[2017-01-01 00:00:00]])

    assert_query("select count(*) from datetimes where datetime >= '2016-06-01' and datetime < '2017-06-01'", %{
      query_id: "1",
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "comparing a timestamp with <>" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["datetime"], [~N[2015-01-01 00:00:00]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["datetime"], [~N[2017-01-01 00:00:00]])

    assert_query("select count(*) from datetimes where datetime <> '2015-01-01'", %{
      query_id: "1",
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "comparing a time" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["time_only"], [~T[01:00:00]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["time_only"], [~T[10:00:00]])

    assert_query("select count(*) from datetimes where time_only > '09:00:00' and time_only < '11:00:00'", %{
      query_id: "1",
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "comparing a date" do
    :ok = insert_rows(_user_ids = 0..9, "datetimes", ["date_only"], [~D[2015-01-01]])
    :ok = insert_rows(_user_ids = 10..19, "datetimes", ["date_only"], [~D[2017-01-01]])

    assert_query("select count(*) from datetimes where date_only > '2016-01-01' and date_only < '2018-01-01'", %{
      query_id: "1",
      columns: ["count"],
      rows: [%{row: [10], occurrences: 1}]
    })
  end

  test "selecting time" do
    time = ~T[01:02:03.000000]
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["time_only"], [time])

    assert_query("select time_only from datetimes", %{rows: [%{row: [time_string]}]})
    assert time_string == Time.to_iso8601(time)
  end

  test "selecting date" do
    date = ~D[0001-02-03]
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["date_only"], [date])

    assert_query("select date_only from datetimes", %{rows: [%{row: [date_string]}]})
    assert date_string == Date.to_iso8601(date)
  end

  test "selecting datetime" do
    date_time = ~N[2015-01-02 03:04:05.000000]
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [date_time])

    assert_query("select datetime from datetimes", %{rows: [%{row: [date_time_string]}]})
    assert date_time_string == NaiveDateTime.to_iso8601(date_time)
  end

  test "using date/time literals" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", [], [])
    date = Date.to_iso8601(~D[2018-01-03])

    assert_query("select hour(time '12:02:01'), date '2018-01-03' from datetimes", %{
      rows: [%{row: [12, ^date]}]
    })
  end

  test "using interval literals" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", [], [])

    assert_query("select interval 'P1Y' from datetimes", %{
      rows: [%{row: ["P1Y"]}]
    })
  end

  test "subtracting interval from datetime" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 03:04:05.000000]])

    assert_query("select datetime - interval 'P1Y' from datetimes", %{rows: [%{row: [date_time_string]}]})
    assert date_time_string == NaiveDateTime.to_iso8601(~N[2014-01-02 03:04:05.000000])
  end

  test "subtracting dates" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["date_only"], [~D[2015-01-02]])

    assert_query("select date_only - date '2014-01-02' from datetimes", %{rows: [%{row: ["P1Y"]}]})
  end

  test "subtracting datetimes" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 12:00:00]])

    assert_query("select datetime - datetime '2014-01-02 12:00:00' from datetimes", %{rows: [%{row: ["P1Y"]}]})
  end

  test "subtracting times" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["time_only"], [~T[01:01:00]])

    assert_query("select time_only - time '00:01:00' from datetimes", %{rows: [%{row: ["PT1H"]}]})
  end

  test "casting date to datetime" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["date_only"], [~D[0001-02-03]])

    assert_query("select cast(date_only as datetime) from datetimes", %{
      rows: [%{row: ["0001-02-03T00:00:00.000000"]}]
    })

    assert_query("select dt from (select cast(date_only as datetime) as dt from datetimes) t", %{
      rows: [%{row: ["0001-02-03T00:00:00.000000"]}]
    })
  end

  test "casting datetime to date" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[0001-02-03 01:00:00]])

    assert_query("select cast(datetime as date) from datetimes", %{rows: [%{row: ["0001-02-03"]}]})

    assert_query("select d from (select cast(datetime as date) as d from datetimes) t", %{
      rows: [%{row: ["0001-02-03"]}]
    })
  end

  test "automatic conversion of text to datetime" do
    :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2017-02-03 01:00:00]])

    assert_query(
      """
        select count(*) from datetimes as t1 join datetimes as t2
        on t1.user_id = t2.user_id and t1.datetime = '2017-02-03 01:00:00'
      """,
      %{rows: [%{row: [10]}]}
    )
  end

  describe "aggregators over date/time" do
    setup do
      :ok = insert_rows(_user_ids = 1..10, "datetimes", ["datetime"], [~N[2015-01-02 10:00:05.000000]])

      :ok =
        insert_rows(_user_ids = 11..30, "datetimes", ["datetime"], [
          ~N[2017-01-02 12:00:05.000000]
        ])

      :ok =
        insert_rows(_user_ids = 31..40, "datetimes", ["datetime"], [
          ~N[2018-03-04 15:00:05.000000]
        ])
    end

    test "min/max over datetime" do
      assert_query("select min(datetime), max(datetime) from datetimes", %{
        rows: [%{row: ["2015-01-02T10:00:05.000000", "2018-03-04T15:00:05.000000"]}]
      })
    end

    test "min/max over date" do
      assert_query("select min(cast(datetime as date)), max(cast(datetime as date)) from datetimes", %{
        rows: [%{row: ["2015-01-02", "2018-03-04"]}]
      })
    end

    test "min/max over time" do
      assert_query("select min(cast(datetime as time)), max(cast(datetime as time)) from datetimes", %{
        rows: [%{row: ["10:00:05.000000", "15:00:05.000000"]}]
      })
    end
  end

  describe "current date/time functions" do
    test "current_date" do
      today = Date.utc_today() |> to_string()

      assert_query(
        "select count(*), current_date() from datetimes",
        %{rows: [%{row: [0, ^today]}]}
      )
    end

    test "current_datetime" do
      today = Date.utc_today() |> to_string()

      assert_query(
        "select count(*), cast(now() as date) from datetimes",
        %{rows: [%{row: [0, ^today]}]}
      )
    end

    test "current_time" do
      hour = Time.utc_now().hour

      assert_query(
        "select count(*), hour(current_time()) from datetimes",
        %{rows: [%{row: [0, ^hour]}]}
      )
    end
  end
end
