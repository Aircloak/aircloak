defmodule Cloak.Query.ErrorTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok =
      Cloak.Test.DB.create_table(
        "test_errors",
        "height INTEGER, name TEXT, male BOOLEAN, datetime TIMESTAMP, date_only DATE, time_only TIME"
      )

    :ok = Cloak.Test.DB.create_table("test_errors2", "height INTEGER")
  end

  test "parse errors include a location indicator" do
    assert_query("select select", %{error: error})

    assert String.contains?(
             error,
             """
             \t1:    select select
             \t             ^
             """
             |> String.trim()
           )
  end

  test "compiler errors include a location indicator" do
    assert_query("select nonexistent from test_errors", %{error: error})

    assert String.contains?(error, """
           \t1:    select nonexistent from test_errors
           \t             ^
           """)
  end

  test "query reports an error on invalid where clause identifier" do
    assert_query("select height from test_errors where nonexistant > 10", %{error: error})
    assert error =~ ~r/Column `nonexistant` doesn't exist in table `test_errors`./
  end

  test "query reports an error on unknown function" do
    assert_query("select invalid_function(height) from test_errors", %{error: error})
    assert error =~ ~r/Unknown function `invalid_function`./
  end

  test "reports an error on wrong cast" do
    assert_query("select * from test_errors where datetime > 0", %{error: error})
    assert error =~ ~r/Arguments of type \(`datetime`, `integer`\) are incorrect for `>`./
  end

  test "reports an error on ambiguous usage of an alias occurring multiple times" do
    assert_query("select count(*) as x, count(height) as x from test_errors order by x", %{
      error: error
    })

    assert error =~ ~r/Usage of `x` is ambiguous./
  end

  test "reports an error on collision between alias and column in order by" do
    assert_query("select count(height) as height from test_errors order by height", %{
      error: error
    })

    assert error =~ ~r/Usage of `height` is ambiguous./
  end

  test "reports an error on collision between alias and column in where" do
    assert_query("select abs(height) as height from test_errors where height = 20", %{
      error: error
    })

    assert error =~ ~s/Usage of `height` is ambiguous./
  end

  test "reports an error on collision between alias and column in group by" do
    assert_query("select abs(height) as height from test_errors group by height", %{error: error})
    assert error =~ ~r/Usage of `height` is ambiguous./
  end

  test "query reports an error on invalid statement" do
    assert_query("invalid statement", %{error: "Expected `select, explain, or show`" <> _})
  end

  test "query reports an error on invalid column" do
    assert_query("select invalid_column from test_errors", %{error: error})
    assert error =~ ~r/Column `invalid_column` doesn't exist in table `test_errors`./
  end

  test "query reports an error on invalid table" do
    assert_query("select column from invalid_table", %{error: error})
    assert "Table `invalid_table` doesn't exist." <> _ = error
  end

  test "query reports an error when mixing aggregated and normal columns" do
    assert_query("select count(*), height from test_errors", %{error: error})
    assert error =~ ~r/`height` needs to appear in the `GROUP BY` clause/
  end

  test "query reports an error when grouping by nonexistent columns" do
    assert_query("select count(*) from test_errors group by nothing", %{error: error})
    assert error =~ ~r/Column `nothing` doesn't exist in table `test_errors`./
  end

  test "query reports an error when not grouping by some selected columns" do
    assert_query("select name, height from test_errors group by height", %{error: error})
    assert error =~ ~r/`name` needs to appear in the `GROUP BY` clause/
  end

  test "query reports an error when calling `grouping_id` without any arguments" do
    assert_query("select grouping_id() from test_errors group by height", %{error: error})
    assert error =~ ~r/Function `grouping_id` requires arguments of type .*, but got ()/
  end

  test "query reports an error when calling `grouping_id` with invalid arguments" do
    assert_query("select grouping_id(abs(height)) from test_errors group by height", %{error: error})

    assert error =~
             Regex.compile!(
               "Arguments to function `grouping_id` have to be from the list of " <>
                 "expressions used in the `GROUP BY` clause"
             )
  end

  test "query reports an error when using `grouping_id` in an invalid clause" do
    assert_query("select count(*) from test_errors where grouping_id(height) = 0 group by height", %{error: error})
    assert error =~ ~r/Function `grouping_id` can not be used in the `WHERE` or `GROUP BY` clauses/
  end

  test "query reports an error when using duplicated grouping sets (1)" do
    assert_query("select name, height from test_errors group by grouping sets ((1, 2), (1, 2))", %{error: error})
    assert error =~ ~r/Duplicated grouping sets used in the `GROUP BY` clause/
  end

  test "query reports an error when using duplicated grouping sets (2)" do
    assert_query("select height, height from test_errors group by grouping sets (1, 2)", %{error: error})
    assert error =~ ~r/Duplicated grouping sets used in the `GROUP BY` clause/
  end

  test "query reports an error when grouping set in restricted query doesn't contain user id" do
    assert_query(
      """
      select count(*) from (
        select user_id, name from test_errors group by grouping sets ((1), (), (1, 2))
      ) t
      """,
      %{error: error}
    )

    assert error =~ ~r/Grouping set in restricted query doesn't contain a user id/
  end

  test "query reports an error on runner crash" do
    ExUnit.CaptureLog.capture_log(fn ->
      assert_query(:invalid_query_type, %{error: "Unknown cloak error."})
    end)
  end

  test "substring with neither for nor from" do
    assert_query("select substring(name) from test_errors", %{error: error})
    assert error =~ ~r/Expected `substring arguments`/
  end

  test "substring with invalid from" do
    assert_query("select substring(name FROM 0) from test_errors", %{error: error})
    assert error =~ ~r/Expected `positive integer constant`/
    assert_query("select substring(name ,    0) from test_errors", %{error: error})
    assert error =~ ~r/Expected `positive integer constant`/
  end

  test "substring with invalid for" do
    assert_query("select substring(name FOR -1) from test_errors", %{error: error})
    assert error =~ ~r/Expected `positive integer constant`/
    assert_query("select substring(name ,   -1) from test_errors", %{error: error})
    assert error =~ ~r/Expected `positive integer constant`/
  end

  test "substring with invalid argument" do
    assert_query("select substring(height FROM 3 FOR 2) from test_errors", %{error: error})
    assert error =~ ~r/Function `substring` requires arguments of type (.*), but got (.*)./
  end

  test "query reports error on invalid limit parameters" do
    assert_query("select name from test_errors limit -1", %{error: error})
    assert error =~ ~r/Expected `positive integer constant`.*/
  end

  test "query reports error on invalid offset parameters" do
    assert_query("select name from test_errors offset -1", %{error: error})
    assert error =~ ~r/Expected `non-negative integer constant`.*/
  end

  test "query reports error on invalid having clause" do
    assert_query("select name from test_errors group by name having height >= 100", %{
      error: error
    })

    assert error =~ ~r/Expression `height` has to appear in the `GROUP BY` clause or be used in an aggregate function./
  end

  test "query reports error on invalid where clause" do
    assert_query("select name from test_errors where max(height) >= 100", %{error: error})
    assert error =~ ~r/Expression `max\(height\)` is not valid in the `WHERE` clause./
  end

  test "query reports error on cast in a where clause" do
    assert_query("select name from test_errors where cast(name as integer) >= 100", %{
      error: error
    })

    assert error =~ ~r/Expression `cast\(name as integer\)` must be limited to a finite range./
    assert error =~ ~r/line 1, column 36/
  end

  test "query reports error on invalid group by position" do
    assert_query("select name from test_errors group by 0", %{
      error: "`GROUP BY` position `0` is out of the range of selected columns." <> _
    })

    assert_query("select name from test_errors group by 2", %{
      error: "`GROUP BY` position `2` is out of the range of selected columns." <> _
    })
  end

  test "non-integer constants are not allowed in group by" do
    assert_query("select name from test_errors group by 1.0", %{
      error: "Non-integer constant is not allowed in `GROUP BY`." <> _
    })
  end

  test "query reports error on invalid order by position" do
    assert_query("select name from test_errors order by 0", %{
      error: "`ORDER BY` position `0` is out of the range of selected columns." <> _
    })

    assert_query("select name from test_errors order by 2", %{
      error: "`ORDER BY` position `2` is out of the range of selected columns." <> _
    })
  end

  test "non-integer constants are not allowed in order by" do
    assert_query("select name from test_errors order by 1.0", %{
      error: "Non-integer constant is not allowed in `ORDER BY`." <> _
    })
  end

  describe "IN" do
    test "non-constant in IN" do
      assert_query("select count(*) from test_errors where height IN (1, height / 2)", %{
        error: "Only constants are allowed on the right-hand side of the `IN` operator." <> _
      })
    end

    test "mistyped IN" do
      assert_query(
        """
          select count(*) from test_errors where height
          in (1, '2')
        """,
        %{error: error}
      )

      assert String.contains?(error, """
             \t1:      select count(*) from test_errors where height
             \t2:      in (1, '2')
             \t               ^
             """)

      assert error =~ ~r/Expected a constant of type `integer`, got `text`/
    end
  end

  test "multiple aggregtors in the same expression are not allowed" do
    assert_query("select max(count(name)) from test_errors", %{
      error: "Expression `max(count(name))` recursively calls multiple aggregators." <> _
    })
  end

  test "[Issue #2559] Inspecting an interval in an error" do
    assert_query("SELECT count(*) FROM test_errors WHERE interval 'PT1S' like ''", %{
      error:
        "Function `like` requires arguments of type (`text`, `like_pattern`), " <>
          "but got (`interval`, `like_pattern`)" <> _
    })
  end

  test "complex expression missing aggregate" do
    assert_query("SELECT count(*), minute(cast('19:27:01', time)) + height FROM test_errors", %{
      error: "Column `height` needs to appear in the `GROUP BY` clause" <> _
    })
  end

  test "division by a constant 0" do
    assert_query("SELECT count(*) FROM test_errors WHERE height / (2 - 2) = 0", %{error: "Division by zero" <> _})
  end

  test "flipped arguments to date_trunc" do
    assert_query("SELECT count(*) FROM test_errors WHERE date_trunc(datetime, 'month')", %{
      error: "Function `date_trunc` requires arguments" <> _
    })
  end

  test "error message for condition pushed into subquery" do
    assert_query(
      """
      SELECT COUNT(*) FROM (SELECT user_id, height FROM test_errors) x
      WHERE height BETWEEN 0 AND 0
      """,
      %{error: error}
    )

    assert String.contains?(error, """
           \t2:    WHERE height BETWEEN 0 AND 0
           \t            ^
           """)
  end

  test "error message for DISTINCT with GROUP BY and ORDER BY" do
    assert_query(
      """
      SELECT COUNT(*) FROM (
        SELECT DISTINCT user_id FROM test_errors GROUP BY height
        ORDER BY user_id
      ) x
      """,
      %{error: error}
    )

    assert String.contains?(error, """
           \t3:      ORDER BY user_id
           \t                 ^
           """)

    assert String.contains?(error, "usage of `DISTINCT`, `GROUP BY`, and `ORDER BY` in the same query is not supported")
  end

  test "error message for failed constant simplification" do
    assert_query(
      """
      SELECT COUNT(*) FROM test_errors
      WHERE height BETWEEN 1 AND sqrt(-1)
      """,
      %{error: error}
    )

    assert String.contains?(error, "Failed to evaluate expression `sqrt(-1)`")

    assert String.contains?(error, """
           \t2:    WHERE height BETWEEN 1 AND sqrt(-1)
           \t                                 ^
           """)
  end

  test "select * error location" do
    assert_query("SELECT * FROM test_errors GROUP BY 1", %{error: error})

    assert String.contains?(error, """
           \t1:    SELECT * FROM test_errors GROUP BY 1
           \t             ^
           """)
  end

  test "selecting all from a non-selected table" do
    assert_query("SELECT bar.* FROM test_errors", %{error: error})

    assert String.contains?(
             error,
             "Select clause `bar.*` cannot be resolved because the table does not exist in the `FROM` list."
           )

    assert String.contains?(error, """
           \t1:    SELECT bar.* FROM test_errors
           \t             ^
           """)
  end
end
