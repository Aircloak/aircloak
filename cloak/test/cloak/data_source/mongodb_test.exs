defmodule Cloak.DataSource.MongoDBTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.{Table, MongoDB}

  import Cloak.Test.MongoHelpers

  @moduletag :exclude_in_dev
  @moduletag :mongodb
  @user_table "user_test"
  @userless_table "userless_test"

  setup_all do
    parameters = [hostname: "localhost", database: "cloaktest"]
    {:ok, conn} = Mongo.start_link(parameters)
    Mongo.delete_many(conn, @user_table, %{})
    Mongo.delete_many(conn, @userless_table, %{})
    date = DateTime.from_unix!(1_437_940_203)

    for i <- 1..10 do
      value = %{
        name: "user#{i}",
        age: 30,
        male: true,
        date: date,
        bills: [%{issuer: "vendor", ids: [1, 2]}]
      }

      Mongo.insert_one!(conn, @user_table, value)
    end

    Mongo.insert_one!(conn, @user_table, %{name: nil, male: nil, mixed: true, bills: nil})
    Mongo.insert_one!(conn, @user_table, %{mixed: "dummy", date: date})
    Mongo.insert_one!(conn, @user_table, %{mixed: [1, 2, 3], date: date})
    Mongo.insert_one!(conn, @user_table, %{mixed: %{a: 1, b: 2}, date: date})

    for _i <- 11..15 do
      Mongo.insert_one!(conn, @user_table, %{debt: -10.55})
    end

    Mongo.insert_one!(conn, @userless_table, %{val: 1})
    Mongo.insert_one!(conn, @userless_table, %{val: 2})
    Mongo.insert_one!(conn, @userless_table, %{val: 3})
    Mongo.insert_one!(conn, @userless_table, %{val: nil})

    tables =
      [
        Table.new(@user_table, "_id", db_name: @user_table),
        Table.new(@userless_table, nil, db_name: @userless_table)
      ]
      |> Enum.flat_map(&MongoDB.load_tables(conn, &1))
      |> Enum.map(&{&1.name, &1})
      |> Enum.into(%{})

    GenServer.stop(conn, :normal, :timer.seconds(5))

    data_source = %{
      name: "mongo_db_standard",
      concurrency: 0,
      lcf_buckets_aggregation_limit: nil,
      driver: MongoDB,
      parameters: parameters,
      tables: tables
    }

    {:ok, data_source: data_source}
  end

  test "connection error", %{data_source: data_source} do
    assert_raise(
      Cloak.Query.ExecutionError,
      ~r/Failed to establish a connection to the database/,
      fn -> Cloak.DataSource.Connection.execute!(put_in(data_source.parameters[:hostname], "invalid_host"), & &1) end
    )
  end

  test "schema mapping", context do
    %{@user_table => root, (@user_table <> "_bills") => bills, (@user_table <> "_bills_ids") => ids} =
      context.data_source.tables

    assert root.columns == [
             Table.column("_id", :text),
             Table.column("age", :real),
             Table.column("bills#", :integer),
             Table.column("date", :datetime),
             Table.column("debt", :real),
             Table.column("male", :boolean),
             Table.column("mixed", :unknown),
             Table.column("name", :text)
           ]

    assert bills.columns == [
             Table.column("_id", :text),
             Table.column("age", :real),
             Table.column("date", :datetime),
             Table.column("debt", :real),
             Table.column("male", :boolean),
             Table.column("mixed", :unknown),
             Table.column("name", :text),
             Table.column("bills.ids#", :integer),
             Table.column("bills.issuer", :text)
           ]

    assert ids.columns == [
             Table.column("_id", :text),
             Table.column("age", :real),
             Table.column("date", :datetime),
             Table.column("debt", :real),
             Table.column("male", :boolean),
             Table.column("mixed", :unknown),
             Table.column("name", :text),
             Table.column("bills.issuer", :text),
             Table.column("bills.ids", :real)
           ]
  end

  test "basic queries on root table", context do
    assert_query(context, "SELECT COUNT(name) FROM #{@user_table}", %{
      rows: [%{occurrences: 1, row: [10]}]
    })

    assert_query(context, "SELECT round(AVG(age)) FROM #{@user_table}", %{
      rows: [%{occurrences: 1, row: [30]}]
    })

    assert_query(context, "SELECT DISTINCT male FROM #{@user_table} WHERE male IS NOT NULL", %{
      rows: [%{occurrences: 1, row: [true]}]
    })
  end

  test(
    "aliased query",
    context,
    do:
      assert_query(context, "SELECT COUNT(a.name) FROM #{@user_table} a", %{
        rows: [%{occurrences: 1, row: [10]}]
      })
  )

  test "conditions on root table", context do
    assert_query(context, "SELECT COUNT(name) FROM #{@user_table} WHERE male = false", %{
      rows: [%{occurrences: 1, row: [0]}]
    })

    assert_query(context, "SELECT COUNT(name) FROM #{@user_table} WHERE false <> male", %{
      rows: [%{occurrences: 1, row: [10]}]
    })

    assert_query(context, "SELECT COUNT(name) FROM #{@user_table} WHERE 30 = age", %{
      rows: [%{occurrences: 1, row: [10]}]
    })
  end

  test "virtual tables", context do
    assert_query(context, "SELECT COUNT(name) FROM #{@user_table}_bills", %{
      rows: [%{occurrences: 1, row: [10]}]
    })

    assert_query(context, "SELECT COUNT(name) FROM #{@user_table}_bills_ids", %{
      rows: [%{occurrences: 1, row: [20]}]
    })
  end

  test "virtual columns", context do
    assert_query(context, "SELECT AVG(bills#) FROM #{@user_table} WHERE bills# = 1", %{
      rows: [%{occurrences: 1, row: [1.0]}]
    })

    assert_query(context, "SELECT AVG(bills.ids#) FROM #{@user_table}_bills", %{
      rows: [%{occurrences: 1, row: [2.0]}]
    })
  end

  test "NULL values in virtual columns", context do
    assert_query(context, "SELECT COUNT(bills#), COUNT(*) FROM #{@user_table}", %{
      rows: [%{occurrences: 1, row: [10, 19]}]
    })
  end

  test "complex queries on virtual columns and tables", context do
    assert_query(
      context,
      "SELECT COUNT(*) FROM #{@user_table}_bills WHERE bills.issuer = 'vendor' AND male = TRUE AND abs(bills.ids#) + 1 = 3",
      %{rows: [%{occurrences: 1, row: [10]}]}
    )

    assert_query(
      context,
      "SELECT bills.issuer, COUNT(*) FROM #{@user_table}_bills_ids WHERE bills.ids = 1 GROUP BY bills.issuer",
      %{rows: [%{occurrences: 1, row: ["vendor", 10]}]}
    )

    assert_query(
      context,
      """
        SELECT AVG(bills#) FROM
          (SELECT _id, bills# FROM #{@user_table} WHERE abs(bills#) = 1 AND bills# = 1) AS t
        WHERE abs(bills#) = 1 AND bills# = 1
      """,
      %{rows: [%{occurrences: 1, row: [1.0]}]}
    )
  end

  test "basic sub-queries", context do
    assert_query(context, "SELECT round(AVG(age)) FROM (SELECT _id, age FROM #{@user_table}) AS t", %{
      rows: [%{occurrences: 1, row: [30]}]
    })

    assert_query(context, "SELECT COUNT(name) FROM (SELECT _id, name FROM #{@user_table}_bills) AS t", %{
      rows: [%{occurrences: 1, row: [10]}]
    })
  end

  test "sub-queries with limit/offset", context do
    assert_query(
      context,
      """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@user_table}_bills_ids ORDER BY _id LIMIT 10) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )

    assert_query(
      context,
      """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@user_table}_bills_ids ORDER BY _id LIMIT 10 OFFSET 10) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end

  test "sub-queries with non-selected order by", context do
    assert_query(
      context,
      """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@user_table}_bills_ids ORDER BY age LIMIT 10) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end

  test "simple order by in standard query", context do
    assert_query(
      context,
      """
        SELECT val FROM #{@userless_table} ORDER BY 1 DESC
      """,
      %{
        rows: [%{row: [3]}, %{row: [2]}, %{row: [1]}, %{row: [nil]}]
      }
    )
  end

  test "complex order by in standard query", context do
    assert_query(
      context,
      """
        SELECT val + 1 AS x FROM #{@userless_table} ORDER BY 1 DESC
      """,
      %{
        rows: [%{row: [4]}, %{row: [3]}, %{row: [2]}, %{row: [nil]}]
      }
    )
  end

  test "explicit nulls directive gets emulated", context do
    assert_query(
      context,
      """
        SELECT name FROM (SELECT _id, name FROM #{@user_table} ORDER BY 2 ASC NULLS LAST LIMIT 10) t
      """,
      %{rows: [%{occurrences: 10, row: [:*]}]}
    )

    assert_query(
      context,
      """
        SELECT val FROM #{@userless_table} ORDER BY 1 DESC NULLS FIRST
      """,
      %{
        rows: [
          %{occurrences: 1, row: [nil]},
          %{occurrences: 1, row: [3]},
          %{occurrences: 1, row: [2]},
          %{occurrences: 1, row: [1]}
        ]
      }
    )
  end

  test "functions in sub-queries", context do
    assert_query(context, "SELECT round(AVG(age)) FROM (SELECT _id, age * 2 + 1 AS age FROM #{@user_table}) AS t", %{
      rows: [%{occurrences: 1, row: [61]}]
    })

    assert_query(context, "SELECT name FROM (SELECT _id, left(name, 4) AS name FROM #{@user_table}) AS t", %{
      rows: [%{occurrences: 9, row: [nil]}, %{occurrences: 10, row: ["user"]}]
    })
  end

  test "aggregation in sub-queries", context do
    assert_query(
      context,
      """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, COUNT(*) AS value FROM #{@user_table} GROUP BY _id) AS t
      """,
      %{rows: [%{occurrences: 1, row: [19, 19]}]}
    )

    assert_query(
      context,
      """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, COUNT(age) AS value FROM #{@user_table} GROUP BY _id) AS t
      """,
      %{rows: [%{occurrences: 1, row: [19, 10]}]}
    )

    assert_query(
      context,
      """
        SELECT COUNT(*), round(SUM(value)) FROM (SELECT _id, AVG(age) AS value FROM #{@user_table} GROUP BY _id) AS t
      """,
      %{rows: [%{occurrences: 1, row: [19, 298]}]}
    )

    assert_query(
      context,
      """
        SELECT COUNT(*), SUM(value) FROM
        (SELECT _id, COUNT(DISTINCT bills.issuer) AS value FROM #{@user_table}_bills_ids GROUP BY _id) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10, 10]}]}
    )

    assert_query(
      context,
      """
        SELECT COUNT(*), SUM(value) FROM
        (SELECT _id, SUM(DISTINCT bills.ids) AS value FROM #{@user_table}_bills_ids GROUP BY _id) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10, 30.0]}]}
    )
  end

  test "distinct in sub-queries", context do
    assert_query(
      context,
      """
        SELECT COUNT(*) FROM (SELECT DISTINCT _id, length(bills.issuer) AS value FROM #{@user_table}_bills_ids) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end

  test "error on invalid conditions", context do
    assert_query(context, "SELECT COUNT(name) FROM #{@user_table} WHERE TRUE IS NOT NULL", %{
      error: "Condition on MongoDB data source expects a column as subject."
    })
  end

  test "datetime support", context do
    assert_query(context, "SELECT DISTINCT date FROM #{@user_table} WHERE date IS NOT NULL", %{
      rows: [%{occurrences: 1, row: ["2015-07-26T19:50:03.000000"]}]
    })

    assert_query(
      context,
      """
        SELECT max(year) FROM (SELECT _id, year(date) FROM #{@user_table} WHERE date = '2015-07-26 19:50:03') AS t
      """,
      %{rows: [%{occurrences: 1, row: [2015]}]}
    )
  end

  test "datetime - quarter support", context do
    assert_query(context, "SELECT quarter(date) FROM #{@user_table} WHERE date IS NOT NULL GROUP BY 1", %{
      rows: [%{occurrences: 1, row: [3]}]
    })
  end

  test "unsupported functions in sub-queries are emulated", context do
    assert_query(context, "SELECT v FROM (SELECT _id, btrim(bills.issuer) AS v FROM #{@user_table}_bills) AS t", %{
      rows: [%{occurrences: 10, row: ["vendor"]}]
    })
  end

  test "dotted names", context do
    assert_query(
      context,
      """
        SELECT count(bills.issuer) FROM (SELECT _id, bills.issuer FROM #{@user_table}_bills_ids) AS t
      """,
      %{rows: [%{occurrences: 1, row: [20]}]}
    )
  end

  test "functions in conditions on root table", context do
    assert_query(context, "SELECT COUNT(name) FROM #{@user_table} WHERE 2 * abs(age) = 60", %{
      rows: [%{occurrences: 1, row: [10]}]
    })
  end

  test "functions in conditions on virtual table and column", context do
    assert_query(context, "SELECT COUNT(name) FROM #{@user_table}_bills WHERE pow(bills.ids#, 0) = 1", %{
      rows: [%{occurrences: 1, row: [10]}]
    })
  end

  test "functions in conditions in subqueries", context do
    assert_query(
      context,
      """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@user_table}_bills WHERE abs(age / 3) = 10) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end

  test "outer functions in having clauses in subqueries", context do
    assert_query(
      context,
      """
        SELECT COUNT(*) FROM (SELECT _id, COUNT(name) FROM #{@user_table}_bills
        GROUP BY _id HAVING abs(COUNT(DISTINCT name)) - 1 = 0) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end

  test "having clauses over grouped columns in subqueries", context do
    assert_query(
      context,
      """
        SELECT COUNT(*) FROM (SELECT _id, name FROM #{@user_table}_bills GROUP BY _id, name HAVING length(name) = 2) AS t
      """,
      %{rows: [%{occurrences: 1, row: [0]}]}
    )
  end

  test "inner functions in having clauses in subqueries", context do
    assert_query(
      context,
      """
        SELECT COUNT(*) FROM (SELECT _id, COUNT(name) FROM #{@user_table}_bills
        GROUP BY _id HAVING COUNT(abs(age)) = 1) AS t
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end

  test "string length", context do
    assert_query(context, "SELECT v FROM (SELECT _id, length(name) AS v FROM #{@user_table}) AS t", %{
      rows: [%{occurrences: 9, row: [5]}, %{occurrences: 9, row: [nil]}]
    })
  end

  test "select constant", context do
    assert_query(context, "SELECT v FROM (SELECT _id, 1 + 1 AS v FROM #{@user_table}) AS t", %{
      rows: [%{occurrences: 19, row: [2]}]
    })
  end

  test "sub-queries with complex order by", context do
    assert_query(
      context,
      """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@user_table}_bills_ids ORDER BY left(name, 2)) AS t
      """,
      %{rows: [%{occurrences: 1, row: [20]}]}
    )
  end

  test "substring", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, substring(name FROM 2 FOR 3) AS v FROM #{@user_table}) AS t WHERE v IS NOT NULL
      """,
      %{rows: [%{occurrences: 10, row: ["ser"]}]}
    )
  end

  test "left", context do
    assert_query(
      context,
      """
        SELECT x FROM (SELECT _id, left(name, 2) AS x FROM #{@user_table}) AS t WHERE x IS NOT NULL
      """,
      %{rows: [%{occurrences: 10, row: ["us"]}]}
    )
  end

  test "right", context do
    assert_query(
      context,
      """
        SELECT x FROM (SELECT _id, right("bills.issuer", 2) AS x FROM #{@user_table}_bills) AS t WHERE x IS NOT NULL
      """,
      %{rows: [%{occurrences: 10, row: ["or"]}]}
    )
  end

  test "cast numbers", context do
    assert_query(
      context,
      """
        SELECT distinct v1, v2, v3 FROM (
          SELECT _id,
            CAST(debt AS integer) AS v1,
            CAST(debt AS real) AS v2,
            CAST(debt AS text) AS v3
           FROM #{@user_table}
        ) AS t ORDER BY 1
      """,
      %{
        rows: [
          %{occurrences: 1, row: [-11, -10.55, "-10.55"]},
          %{occurrences: 1, row: [nil, nil, nil]}
        ]
      }
    )
  end

  test "cast datetime", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, CAST(date AS text) AS v FROM #{@user_table}) AS t WHERE v IS NOT NULL
      """,
      %{rows: [%{occurrences: 13, row: ["2015-07-26 19:50:03.000000"]}]}
    )
  end

  test "cast boolean to text", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, CAST(male AS text) AS v FROM #{@user_table}) AS t ORDER BY 1
      """,
      %{rows: [%{occurrences: 10, row: ["true"]}, %{occurrences: 9, row: [nil]}]}
    )
  end

  test "cast boolean to integer", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, CAST(male AS integer) AS v FROM #{@user_table}) AS t ORDER BY 1
      """,
      %{rows: [%{occurrences: 10, row: [1]}, %{occurrences: 9, row: [nil]}]}
    )
  end

  test "cast boolean to real", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, CAST(male AS real) AS v FROM #{@user_table}) AS t ORDER BY 1
      """,
      %{rows: [%{occurrences: 10, row: [1.0]}, %{occurrences: 9, row: [nil]}]}
    )
  end

  test "cast boolean to text to boolean", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, CAST(CAST(male AS text) AS boolean) AS v FROM #{@user_table}) AS t ORDER BY 1
      """,
      %{rows: [%{occurrences: 10, row: [true]}, %{occurrences: 9, row: [nil]}]}
    )
  end

  test "cast integer to boolean", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, CAST(age AS boolean) AS v FROM #{@user_table}) AS t ORDER BY 1
      """,
      %{rows: [%{occurrences: 10, row: [true]}, %{occurrences: 9, row: [nil]}]}
    )
  end

  test "cast real to boolean", context do
    assert_query(
      context,
      """
        SELECT v FROM (SELECT _id, CAST(age AS boolean) AS v FROM #{@user_table}) AS t ORDER BY 1
      """,
      %{rows: [%{occurrences: 10, row: [true]}, %{occurrences: 9, row: [nil]}]}
    )
  end

  test "round", context do
    assert_query(
      context,
      """
        SELECT v1, v2 FROM (
          SELECT _id, round(debt) AS v1, round(debt, 1) AS v2 FROM #{@user_table}
        ) AS t ORDER BY 1
      """,
      %{rows: [%{occurrences: 5, row: [-11, -10.6]}, %{occurrences: 14, row: [nil, nil]}]}
    )
  end

  test "(i)like conditions", context do
    assert_query(
      context,
      """
        SELECT left(name, 4) FROM #{@user_table} WHERE name LIKE 'user_' AND name NOT ILIKE 'USer__'
      """,
      %{rows: [%{occurrences: 9, row: ["user"]}]}
    )
  end

  test "simple standard query", context do
    assert_query(context, "SELECT COUNT(*) FROM #{@userless_table}", %{rows: [%{occurrences: 1, row: [4]}]})
  end

  test "simple standard query with filter", context do
    assert_query(context, "SELECT val FROM #{@userless_table} WHERE val = 0", %{
      rows: []
    })
  end

  test "divide by 0", context do
    assert_query(context, "SELECT 1/(val-val) AS v FROM #{@userless_table}", %{
      rows: [%{row: [nil]}, %{row: [nil]}, %{row: [nil]}, %{row: [nil]}]
    })
  end

  test "mod by 0", context do
    assert_query(context, "SELECT 1 % round(val-val) AS v FROM #{@userless_table}", %{
      rows: [%{row: [nil]}, %{row: [nil]}, %{row: [nil]}, %{row: [nil]}]
    })
  end

  test "sqrt with negative input", context do
    assert_query(context, "SELECT sqrt(-val) AS v FROM #{@userless_table}", %{
      rows: [%{row: [nil]}, %{row: [nil]}, %{row: [nil]}, %{row: [nil]}]
    })
  end

  test "pow with negative input", context do
    assert_query(context, "SELECT pow(-val, -0.2) AS v FROM #{@userless_table}", %{
      rows: [%{row: [nil]}, %{row: [nil]}, %{row: [nil]}, %{row: [nil]}]
    })
  end

  test "standard query with grouping and order by (1)", context do
    assert_query(
      context,
      """
        SELECT x FROM (
          SELECT CAST(val - 1 AS boolean) AS x FROM #{@userless_table}
        ) AS t GROUP BY 1 ORDER BY 1
      """,
      %{
        rows: [%{occurrences: 1, row: [nil]}, %{occurrences: 1, row: [false]}, %{occurrences: 1, row: [true]}]
      }
    )
  end

  test "standard query with grouping and order by (2)", context do
    assert_query(
      context,
      """
        SELECT val FROM #{@userless_table} GROUP BY val ORDER BY val + 1
      """,
      %{
        rows: [
          %{occurrences: 1, row: [nil]},
          %{occurrences: 1, row: [1]},
          %{occurrences: 1, row: [2]},
          %{occurrences: 1, row: [3]}
        ]
      }
    )
  end

  test "standard query with empty group", context do
    assert_query(
      context,
      """
        SELECT SUM(val) FROM #{@userless_table} GROUP BY ()
      """,
      %{rows: [%{occurrences: 1, row: [6]}]}
    )
  end

  test "standard query that doesn't reference any columns", context do
    assert_query(
      context,
      """
        SELECT 0 + 1 AS x, 'a', TRUE FROM #{@userless_table}
      """,
      %{
        rows: [
          %{occurrences: 1, row: [1, "a", true]},
          %{occurrences: 1, row: [1, "a", true]},
          %{occurrences: 1, row: [1, "a", true]},
          %{occurrences: 1, row: [1, "a", true]}
        ]
      }
    )
  end

  test "standard query with grouping sets", context do
    assert_query(
      context,
      """
        SELECT val, COUNT(*) FROM #{@userless_table} GROUP BY GROUPING SETS (val, ()) ORDER BY val
      """,
      %{
        rows: [
          %{occurrences: 1, row: [1, 1]},
          %{occurrences: 1, row: [2, 1]},
          %{occurrences: 1, row: [3, 1]},
          %{occurrences: 1, row: [nil, 1]},
          %{occurrences: 1, row: [nil, 4]}
        ]
      }
    )
  end

  test "standard query with distinct", context do
    assert_query(context, "SELECT DISTINCT CAST(val - 1 AS boolean) AS x FROM #{@userless_table}", %{
      rows: [%{occurrences: 1, row: [nil]}, %{occurrences: 1, row: [true]}, %{occurrences: 1, row: [false]}]
    })
  end
end
