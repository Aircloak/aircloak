defmodule Cloak.Sql.Compiler.Test do
  use ExUnit.Case, async: true

  import Lens.Macros

  alias Cloak.Sql.{Expression, Compiler, Parser, Query}

  defmacrop column(table_name, column_name) do
    quote do
      %{name: unquote(column_name), table: %{db_name: unquote(table_name)}}
    end
  end

  test "adds an empty group by" do
    assert %{group_by: []} = compile!("select * from table", data_source())
  end

  test "adds a non-nil condition on user_id for top query" do
    query = compile!("select * from (select uid, column from table) as t", data_source())
    assert {:not, {:is, %{name: "uid"}, :null}} = query.where
    {:subquery, %{ast: subquery}} = query.from
    assert nil == subquery.where
  end

  for first <- [:>, :>=], second <- [:<, :<=] do
    test "rejects inequalities on strings with #{first} and #{second}" do
      {:error, error} = compile("select * from table where string #{unquote(first)} " <>
        "'CEO' and string #{unquote(second)} 'CEP'", data_source())
      assert error == "Inequalities on string values are currently not supported."
    end
  end

  test "rejects mistyped where conditions" do
    {:error, error} = compile("select * from table where numeric = column", data_source())
    assert error == "Column `numeric` from table `table` of type `integer` and column `column` from table `table` "
      <> "of type `datetime` cannot be compared."
  end

  test "rejects mistyped like conditions" do
    {:error, error} = compile("select * from table where numeric like 'something'", data_source())
    assert error == "Column `numeric` from table `table` of type `integer` cannot be used in a LIKE expression."
  end

  test "rejects usage of * in function requiring argument of known type" do
    {:error, error} = compile("select length(*) from table", data_source())
    assert error =~ ~r/unspecified type/
  end

  test "casts datetime where conditions" do
    result = compile!("select * from table where column > '2015-01-01' and column < '2016-01-01'", data_source())

    assert {:and, {:comparison, column("table", "column"), :>=, value}, _rhs} = result.where
    assert value == Expression.constant(:datetime, ~N[2015-01-01 00:00:00.000000])
  end

  test "allows comparing datetime columns to other datetime columns" do
    assert {:ok, _} = compile("select * from table where column = column", data_source())
  end

  test "casts time where conditions" do
    assert %{where: {:and, range, _}} =
      compile!("select * from table where column >= '01:00:00' and column < '02:00:00'", time_data_source())
    assert {:and, {:comparison, column("table", "column"), :>=, value}, _} = range
    assert value == Expression.constant(:time, ~T[01:00:00.000000])
  end

  test "casts date where conditions" do
    assert %{where: {:and, range, _}} =
      compile!("select * from table where column >= '2015-01-01' and column < '2016-01-01'", date_data_source())
    assert {:and, {:comparison, column("table", "column"), :>=, value}, _} = range
    assert value == Expression.constant(:date, ~D[2015-01-01])
  end

  test "casts datetime in `in` conditions" do
    result = compile!("select * from table where column in ('2015-01-01', '2015-01-02')", data_source())

    assert {:and, {:not, {:is, column("table", "uid"), :null}}, {:in, column("table", "column"), times}} = result.where
    assert times |> Enum.map(&(&1.value)) |> Enum.sort() ==
      [~N[2015-01-01 00:00:00.000000], ~N[2015-01-02 00:00:00.000000]]
  end

  test "casts datetime in negated conditions" do
    result = compile!("select * from table where column <> '2015-01-01'", data_source())

    assert {:and, {:not, {:is, column("table", "uid"), :null}},
      {:comparison, column("table", "column"), :<>, value}} = result.where
    assert value == Expression.constant(:datetime, ~N[2015-01-01 00:00:00.000000])
  end

  test "reports malformed datetimes" do
    assert {:error, "Cannot cast `something stupid` to datetime."} =
      compile("select * from table where column > 'something stupid'", data_source())
  end

  for function <- ~w(sum) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "rejecting #{function} on non-numerical columns" do
      assert {:error, error} = compile("select #{unquote(function)}(string) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer`) or (`real`), but got (`text`)."
    end
  end

  for function <- ~w(min max median) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
      assert {:ok, _} = compile("select #{unquote(function)}(distinct numeric) from table", data_source())
    end

    test "allowing #{function} on text columns in subqueries" do
      assert {:ok, _} = compile("""
          select * from (select uid, #{unquote(function)}(string) from table group by uid) t
        """, data_source())
      assert {:ok, _} = compile("""
          select * from (select uid, #{unquote(function)}(distinct string) from table group by uid) t
        """, data_source())
    end

    test "allowing #{function} on datetime columns in subqueries" do
      assert {:ok, _} = compile("""
          select * from (select uid, #{unquote(function)}(column) from table group by uid) t
        """, data_source())
      assert {:ok, _} = compile("""
          select * from (select uid, #{unquote(function)}(distinct column) from table group by uid) t
        """, data_source())
    end

    test "rejecting #{function} on text columns in top query" do
      assert {:error, _} = compile("select #{unquote(function)}(string) from table", data_source())
      assert {:error, _} = compile("select #{unquote(function)}(distinct string) from table", data_source())
    end

    test "rejecting #{function} on datetime columns in top query" do
      assert {:error, _} = compile("select #{unquote(function)}(column) from table", data_source())
      assert {:error, _} = compile("select #{unquote(function)}(distinct column) from table", data_source())
    end
  end

  for function <- ~w(avg stddev sqrt) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "rejecting #{function} on non-numerical columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer` | `real`), but got (`datetime`)."
    end
  end

  test "allowing abs on numeric columns" do
    assert {:ok, _} = compile("select abs(numeric) from table", data_source())
  end

  test "rejecting abs on non-numerical columns" do
    assert {:error, error} = compile("select abs(column) from table", data_source())
    assert error == "Function `abs` requires arguments of type (`integer`) or (`real`), but got (`datetime`)."
  end

  for function <- ~w(count) do
    test "allowing #{function} on non-numerical columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table", data_source())
    end
  end

  for function <- ~w(count avg min max sum stddev median) do
    test "rejecting #{function} in group by" do
      query = "select #{unquote(function)}(numeric) from table group by #{unquote(function)}(numeric)"
      assert {:error, error} = compile(query, data_source())
      assert error == "Aggregate function `#{unquote(function)}` can not be used in the `GROUP BY` clause."
    end
  end

  for function <- ~w(year quarter month day hour minute second weekday) do
    test "allowing #{function} on datetime columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table", data_source())
    end

    test "allowing #{function} in group by" do
      assert {:ok, _} = compile(
        "select #{unquote(function)}(column) from table group by #{unquote(function)}(column)",
        data_source()
      )
    end

    test "allowing #{function} in select when the argument is grouped" do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table group by column", data_source())
    end
  end

  for function <- ~w(hour minute second) do
    test "rejecting #{function} on non-datetime columns" do
      assert {:error, error} = compile("select #{unquote(function)}(numeric) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`datetime` | `time`), but got (`integer`)."
    end
  end

  for function <- ~w(year quarter month day weekday) do
    test "rejecting #{function} on non-datetime columns" do
      assert {:error, error} = compile("select #{unquote(function)}(numeric) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`datetime` | `date`), but got (`integer`)."
    end
  end

  for function <- ~w(floor ceil ceiling) do
    test "allowing #{function} on real columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(float) from table", data_source())
    end

    test "rejecting #{function} on non-numeric columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer` | `real`), but got (`datetime`)."
    end
  end

  for function <- ~w(trunc round) do
    test "allowing #{function} on real columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(float) from table", data_source())
    end

    test "rejecting #{function} on non-numeric columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error == "Function `#{unquote(function)}` requires arguments of type"
       <> " (`integer` | `real`) or (`integer` | `real`, `integer`), but got (`datetime`)."
    end
  end

  test "multiarg function argument verification" do
    assert {:error, error} = compile("select div(numeric, column) from table", data_source())
    assert error ==
      "Function `div` requires arguments of type (`integer`, `integer`), but got (`integer`, `datetime`)."
  end

  test "rejecting a function with too many arguments" do
    assert {:error, error} = compile("select avg(numeric, column) from table", data_source())
    assert error ==
      "Function `avg` requires arguments of type (`integer` | `real`), but got (`integer`, `datetime`)."
  end

  test "rejecting a function with too few arguments" do
    assert {:error, error} = compile("select div(numeric) from table", data_source())
    assert error == "Function `div` requires arguments of type (`integer`, `integer`), but got (`integer`)."
  end

  test "rejecting a column in select when its function is grouped" do
    assert {:error, error} = compile("select column from table group by day(column)", data_source())
    assert error ==
      "Column `column` from table `table` needs to appear in the `GROUP BY` clause" <>
      " or be used in an aggregate function."
  end

  test "rejecting a function in select when another function is grouped" do
    assert {:error, error} = compile("select div(numeric, numeric) from table group by abs(numeric)", data_source())
    assert error == "Column `numeric` from table `table` needs to appear in the `GROUP BY` clause or be used in an " <>
      "aggregate function."
  end

  test "rejecting concat on non-strings" do
    assert {:error, error} = compile("select concat(numeric) from table", data_source())
    assert error == "Function `concat` requires arguments of type ([`text`]+), but got (`integer`)."
  end

  test "rejecting ill-typed nested function calls" do
    assert {:error, error} = compile("select concat(avg(numeric)) from table", data_source())
    assert error == "Function `concat` requires arguments of type ([`text`]+), but got (`real`)."
  end

  test "typechecking nested function calls recursively" do
    assert {:error, error} = compile("select sqrt(abs(avg(column))) from table", data_source())
    assert error == "Function `avg` requires arguments of type (`integer` | `real`), but got (`datetime`)."
  end

  test "accepting constants as aggregated", do:
    assert {:ok, _} = compile("select count(*), 1, abs(1) from table", data_source())

  test "accepting constants as aggregated in queries with group by", do:
    assert {:ok, _} = compile("select 1, abs(1) from table group by numeric", data_source())

  test "accepting proper joins" do
    assert {:ok, _} = compile("SELECT t1.c1 from t1, t2 WHERE t1.uid = t2.uid", data_source())
    assert {:ok, _} = compile("SELECT t1.c1 from t1, t2 WHERE t2.uid = t1.uid", data_source())

    assert {:ok, _} = compile(
      "SELECT t1.c1 from t1, t2, t3 WHERE t1.uid = t2.uid AND t2.uid = t3.uid",
      data_source()
    )

    assert {:ok, _} = compile(
      "SELECT t1.c1 from t1, t2, t3 WHERE t3.uid = t1.uid AND t3.uid = t2.uid",
      data_source()
    )

    assert {:ok, _} = compile(
      "SELECT t1.c1 from t1, t2, t3, t4 WHERE t3.uid = t1.uid AND t3.uid = t2.uid AND t1.uid = t4.uid",
      data_source()
    )
  end

  test "rejecting missing column" do
    assert {:error, "Column `a` doesn't exist in table `table`."} =
      compile("SELECT a FROM table", data_source())
  end

  test "rejecting missing column on join" do
    assert {:error, "Column `a` doesn't exist in any of the selected tables."} =
      compile("SELECT a FROM t1, t2", data_source())
  end

  test "rejecting missing qualified column" do
    assert {:error, "Column `a` doesn't exist in table `table`."} =
      compile("SELECT table.a FROM table", data_source())
  end

  test "rejecting qualified SELECT from not selected table" do
    assert {:error, "Missing FROM clause entry for table `other_table`."} =
      compile("SELECT other_table.other_column FROM table", data_source())
  end

  test "rejecting qualified SELECT from not selected table when join" do
    assert {:error, "Missing FROM clause entry for table `other_table`."} =
      compile("SELECT other_table.other_column FROM t1, t2", data_source())
  end

  test "rejecting qualified ORDER BY from not selected table" do
    assert {:error, "Missing FROM clause entry for table `other_table`."} =
      compile("SELECT column FROM table ORDER BY other_table.other_column", data_source())
  end

  test "rejecting qualified GROUP BY from not selected table" do
    assert {:error, "Missing FROM clause entry for table `other_table`."} =
      compile("SELECT column FROM table GROUP BY other_table.other_column", data_source())
  end

  test "rejecting qualified WHERE from not selected table" do
    assert {:error, "Missing FROM clause entry for table `other_table`."} =
      compile("SELECT column FROM table WHERE other_table.other_column <> ''", data_source())
  end

  test "rejecting joins with only one side of a range" do
    assert {:error, "Column `numeric` from table `table` must be limited to a finite range."} =
      compile("SELECT * FROM table JOIN other_table ON table.uid = other_table.uid AND numeric > 3", data_source())
  end

  test "aligning ranges in joins" do
    query1 = compile!("SELECT * FROM table JOIN other_table ON table.uid = other_table.uid AND numeric > 3" <>
      " AND numeric < 9", data_source())
    query2 = compile!("SELECT * FROM table JOIN other_table ON table.uid = other_table.uid AND numeric >= 0" <>
      " AND numeric < 10", data_source())
    assert query1.info ==
      ["The range for column `numeric` from table `table` has been adjusted to 0.0 <= `numeric` < 10.0."]
    {:join, %{conditions: conditions1}} = query1.from
    {:join, %{conditions: conditions2}} = query2.from
    assert conditions_list(conditions1) == conditions_list(conditions2)
  end

  test "rejecting improper joins" do
    assert {:error, error} = compile("SELECT t1.c1 from t1, t2", data_source())
    assert error =~ ~r/Missing where comparison.*`t1` and `t2`/

    assert {:error, error} = compile("SELECT t1.c1 from t1, t2, t3 WHERE t1.uid = t2.uid", data_source())
    assert error =~ ~r/Missing where comparison.*`t1` and `t3`/

    assert {:error, error} = compile(
      "SELECT t1.c1 from t1, t2, t3, t4 WHERE t1.uid = t2.uid AND t3.uid = t4.uid",
      data_source()
    )
    assert error =~ ~r/Missing where comparison.*`t1` and `t3`/
  end

  test "rejecting a join when cast changes the uid type" do
    assert {:error, error} = compile("SELECT t1.c1 from t1, t2 WHERE cast(t1.uid as text) = t2.uid", data_source())
    assert error =~ ~r/Missing where comparison.*`t1` and `t2`/
  end

  Enum.each(["count", "min", "max", "median", "stddev"], fn(function) ->
    test "allows qualified identifiers in function calls (function #{function})" do
      assert %{columns: [%Expression{function: unquote(function), function_args: [column("table", "numeric")]}]} =
        compile!("select #{unquote(function)}(table.numeric) from table", data_source())
    end
  end)

  test "qualifies all identifiers" do
    result = compile!("""
        SELECT column, count(column)
        FROM table
        WHERE numeric >= 1 and numeric < 9 and column <> '2015-01-02'
        GROUP BY column
        ORDER BY count(column) DESC, count(table.column) DESC
      """,
      data_source())
    assert [column("table", "column"), %Expression{function: "count", function_args: [column("table", "column")]}] =
      result.columns
    conditions = conditions_list(result.where)
    assert Enum.any?(conditions, &match?({:comparison, column("table", "numeric"), :>=, _}, &1))
    assert Enum.any?(conditions, &match?({:comparison, column("table", "numeric"), :<, _}, &1))
    assert Enum.any?(conditions, &match?({:not, {:is, column("table", "uid"), :null}}, &1))
    assert Enum.any?(conditions, &match?({:comparison, column("table", "column"), :<>, _}, &1))
    assert [column("table", "column")] = result.group_by
    assert [{expr_1, :desc}, {expr_2, :desc}] = result.order_by
    assert %Expression{function: "count"} = expr_1
    assert %Expression{function: "count"} = expr_2
  end

  test "complains when tables don't exist" do
    assert {:error, "Table `t_doesnt_exist` doesn't exist."} =
      compile("SELECT c1 FROM t1, t_doesnt_exist", data_source())
  end

  test "expands all columns for all tables when joining" do
    result = compile!("SELECT * FROM t1, t2 JOIN t3 on t2.uid = t3.uid WHERE t1.uid = t2.uid", data_source())
    assert [
      %{value: :*},
      column("t1", "c1"),
      column("t1", "c2"),
      %{value: :*},
      column("t2", "c1"),
      column("t2", "c3"),
      %{value: :*},
      column("t3", "c1")
    ] = result.columns
  end

  test "complains when an unqualified identifier cannot be pinned down" do
    assert {:error, "Column `c1` is ambiguous."} =
      compile("SELECT c1 FROM t1, t2", data_source())
  end

  test "allows for where, order by and group by when performing cross join" do
    result = compile!("""
        SELECT t1.c1
        FROM t1, t2
        WHERE c2 > 10 AND c2 < 20
        AND t1.uid = t2.uid
        GROUP BY t1.c1, c3
        ORDER BY t1.c1 DESC
      """,
      data_source())
    conditions = conditions_list(result.where)
    assert [column("t1", "c1")] = result.columns
    assert Enum.any?(conditions, &match?({:comparison, column("t1", "c2"), :>=, _}, &1))
    assert Enum.any?(conditions, &match?({:comparison, column("t1", "c2"), :<, _}, &1))
    assert Enum.any?(conditions, &match?({:comparison, column("t1", "uid"), :=, column("t2", "uid")}, &1))
    assert [column("t1", "c1"), column("t2", "c3")] = result.group_by
    assert [{column("t1", "c1"), :desc}] = result.order_by
  end

  test "complains when conditions not on columns of JOINed tables" do
    assert {:error, "Column `c3` of table `t2` is used out of scope."} = compile("""
      SELECT t1.c1
      FROM
        t1 INNER JOIN t3 ON t1.uid = t3.uid and t2.c3 > 10,
        t2
      WHERE t2.uid = t1.uid
    """, data_source())
    assert {:error, "Column `c3` of table `t2` is used out of scope."} = compile("""
      SELECT t1.c1
      FROM
        t1 INNER JOIN t3 ON t1.uid = t3.uid and c3 > 10,
        t2
      WHERE t2.uid = t1.uid
    """, data_source())
  end

  test "complains on ambiguous JOIN on condition" do
    assert {:error, "Column `c1` is ambiguous."} = compile("""
      SELECT t1.c1
      FROM t1 INNER JOIN t2 ON t1.uid = t2.uid and c1 > 10
    """, data_source())
  end

  test "Can JOIN on columns from earlier JOIN" do
    assert {:ok, _} = compile("""
      SELECT t1.c1
      FROM
        t1 INNER JOIN t2 ON t1.uid = t2.uid
           INNER JOIN t3 ON t3.uid = t1.uid AND t1.c2 > 10 AND t1.c2 < 20
    """, data_source())
  end

  test "rejecting invalid casts" do
    assert {:error, error} = compile("select cast(column as integer) from table", data_source())
    assert error == "Cannot cast value of type `datetime` to type `integer`."
  end

  test "accepting valid casts" do
    assert {:ok, _} = compile("select cast(column as date) from table", data_source())
  end

  test "subquery must return a user_id" do
    assert {:error, error} = compile("select c1 from (select c1 from t1) alias", data_source())
    assert error =~ "Missing a user id column"
  end

  test "missing group by in a subquery" do
    assert {:error, error} = compile("select c1 from (select uid, count(*) from t1) alias", data_source())
    assert error =~ "Column `uid` from table `t1` needs to appear in the `GROUP BY`"
  end

  test "integer operations are valid on sums of integer columns" do
    assert {:ok, _} = compile("select sum(numeric) % 3 from table", data_source())
  end

  test "integer operations are invalid on sums of real columns" do
    assert {:error, error} = compile("select sum(float) % 3 from table", data_source())
    assert error ==
        "Function `%` requires arguments of type (`integer`, `integer`), but got (`real`, `integer`)."
  end

  test "incorrect application of +" do
    assert {:error, error} = compile("select 'a' + 'b' from table", data_source())
    assert error == "Arguments of type (`text`, `text`) are incorrect for `+`."
  end

  test "rejects inequalities on numeric columns that are not ranges" do
    assert {:error, error} = compile("select * from table where numeric > 5", data_source())
    assert error == "Column `numeric` from table `table` must be limited to a finite range."
  end

  test "rejects inequalities on numeric columns that are negatives of ranges" do
    assert {:error, error} = compile("select * from table where numeric < 2 and numeric > 5", data_source())
    assert error == "Column `numeric` from table `table` must be limited to a finite range."
  end

  test "rejects inequalities on datetime columns that are negatives of ranges" do
    assert {:error, error} = compile("select * from table where column < '2015-01-01' and column > '2016-01-01'",
      data_source())
    assert error == "Column `column` from table `table` must be limited to a finite range."
  end

  test "rejects inequalities on datetime columns that are not ranges" do
    assert {:error, error} = compile("select * from table where column > '2015-01-01'", data_source())
    assert error == "Column `column` from table `table` must be limited to a finite range."
  end

  test "rejects inequalities on date columns that are negatives of ranges" do
    assert {:error, error} = compile("select * from table where column < '2015-01-01' and column > '2016-01-01'",
      date_data_source())
    assert error == "Column `column` from table `table` must be limited to a finite range."
  end

  test "rejects inequalities on date columns that are not ranges" do
    assert {:error, error} = compile("select * from table where column > '2015-01-01'", data_source())
    assert error == "Column `column` from table `table` must be limited to a finite range."
  end

  test "accepts inequalities on numeric columns that are ranges" do
    assert {:ok, _} = compile("select * from table where numeric > 5 and numeric < 8", data_source())
  end

  test "fixes alignment of ranges" do
    assert compile!("select * from table where numeric > 1 and numeric < 9", data_source()).where
      == compile!("select * from table where numeric > 0 and numeric < 10", data_source()).where
  end

  test "fixes alignment of datetime ranges" do
    aligned = compile!("select * from table where column > '2015-01-02' and column < '2016-07-01'", data_source())
    assert compile!("select * from table where column > '2015-01-01' and column < '2016-08-02'", data_source()).where
      == aligned.where
    assert aligned.info == ["The range for column `column` from table `table` has been adjusted to "
      <> "2015-01-01 00:00:00.000000 <= `column` < 2017-01-01 00:00:00.000000."]
  end

  test "no message when datetime alignment does not require fixing" do
    assert compile!("select * from table where column >= '2006-05-31' and column < '2006-06-01'", data_source()).
      info == []
  end

  test "fixes alignment of date ranges" do
    aligned = compile!("select * from table where column > '2015-01-02' and column < '2016-07-01'", date_data_source())
    assert compile!("select * from table where column > '2015-01-01' and column < '2016-08-02'", date_data_source()).
      where == aligned.where
    assert aligned.info == ["The range for column `column` from table `table` has been adjusted to 2015-01-01 <= "
      <> "`column` < 2017-01-01."]
  end

  test "fixes alignment of time ranges" do
    aligned = compile!("select * from table where column > '00:00:01' and column < '00:00:04'", time_data_source())
    unaligned = compile!("select * from table where column >= '00:00:00' and column < '00:00:05'", time_data_source())
    assert conditions_list(unaligned.where) == conditions_list(aligned.where)
    assert aligned.
      info == ["The range for column `column` from table `table` has been adjusted to 00:00:00.000000 <= "
        <> "`column` < 00:00:05.000000."]
  end

  test "no message when time alignment does not require fixing" do
    assert compile!("select * from table where column >= '00:00:00' and column < '00:00:05'", time_data_source()).
      info == []
  end

  test "includes an info message when the aligment is fixed" do
    assert [msg] = compile!("select count(*) from table where numeric >= 0.1 and numeric < 1.9", data_source()).info
    assert msg == "The range for column `numeric` from table `table` has been adjusted to 0.0 <= `numeric` < 2.0."
  end

  test "columns with an inequality are requested for fetching" do
    columns = compile!("select count(*) from table where numeric >= 0.1 and numeric < 1.9", data_source()).db_columns
    assert Enum.any?(columns, &(&1.name == "numeric"))
  end

  test "columns for fetching are not duplicated" do
    columns = compile!("select numeric from table where numeric >= 0.1 and numeric < 1.9", data_source()).db_columns
    assert Enum.count(columns, &(&1.name == "numeric")) == 1
  end

  test "does not include an info message when the alignment does not need to be fixed" do
    assert compile!("select count(*) from table where numeric >= 1 and numeric < 2", data_source()).info == []
  end

  test "silently discards redundant inequalities" do
    assert compile!("select count(*) from table
      where numeric >= 1 and numeric > 0.9 and numeric < 2 and numeric <= 2.1", data_source()) |> scrub_aliases() ==
      compile!("select count(*) from table where numeric >= 1 and numeric < 2", data_source()) |> scrub_aliases()
  end

  test "unquoted columns are case-insensitive" do
    first = "select CoLumn from table" |> compile!(data_source()) |> Map.drop([:column_titles])
    second = "select column from table" |> compile!(data_source()) |> Map.drop([:column_titles])
    assert first == second
  end

  test "quoted columns are case-sensitite" do
    assert {:error, reason} = compile("select \"CoLumn\" from table", data_source())
    assert reason =~ ~r/Column `CoLumn` doesn't exist/
  end

  test "unquoted qualified columns are case-insensitive" do
    first = "select table.CoLumn from table" |> compile!(data_source()) |> Map.drop([:column_titles])
    second = "select table.column from table" |> compile!(data_source()) |> Map.drop([:column_titles])
    assert first == second
  end

  test "quoted qualified columns are case-sensitite" do
    assert {:error, reason} = compile("select table.\"CoLumn\" from table", data_source())
    assert reason =~ ~r/Column `CoLumn` doesn't exist/
  end

  test "unquoted tables are case-insensitive" do
    first = "select tAbLe.column from tabLe" |> compile!(data_source()) |> Map.drop([:column_titles])
    second = "select table.column from table" |> compile!(data_source()) |> Map.drop([:column_titles])
    assert first == second
  end

  test "quoted tables are case-sensitive" do
    assert {:error, _} = compile("select table.column from \"tabLe\"", data_source())
    assert {:error, reason} = compile("select \"tAbLe\".column from table", data_source())
    assert reason =~ ~r/Missing FROM clause entry for table `tAbLe`/
  end

  test "bucket sizes are aligned, adding an info message" do
    first = compile!("select bucket(numeric by 0.11) as foo from table", data_source())
    second = compile!("select bucket(numeric by 0.1) as foo from table", data_source())

    assert Map.drop(first, [:info]) == Map.drop(second, [:info])
    assert ["Bucket size adjusted from 0.11 to 0.1"] = first.info
    assert [] = second.info
  end

  test "negative and 0 bucket sizes are not allowed" do
    assert {:error, _} = compile("select bucket(numeric by 0) from table", data_source())
    assert {:error, error} = compile("select bucket(numeric by -10) from table", data_source())
    assert error =~ ~r/Bucket size -10 must be > 0/
  end

  test "limit is aligned with a message in subqueries" do
    result = compile!("select count(*) from (select * from table order by numeric limit 24) foo", data_source())
    assert %{from: {:subquery, %{ast: %{limit: 20}}}} = result
    assert ["Limit adjusted from 24 to 20"] = result.info
  end

  test "minimum limit is 10 in subqueries" do
    result = compile!("select count(*) from (select * from table order by numeric limit 5) foo", data_source())
    assert %{from: {:subquery, %{ast: %{limit: 10}}}} = result
    assert ["Limit adjusted from 5 to 10"] = result.info
  end

  test "limit is not changed in the root query" do
    result = compile!("select * from table order by numeric limit 9", data_source())
    assert result.limit == 9
  end

  test "offset requires limit in subqueries" do
    assert {:error, error} = compile("select count(*) from (select * from table order by numeric offset 20) foo",
      data_source())
    assert error =~ ~r/Subquery `foo` has an OFFSET clause without a LIMIT clause/
  end

  test "offset must be a multiple of limit post-alignment" do
    result = compile!("select count(*) from (select * from table order by numeric limit 20 offset 31) foo",
      data_source())
    assert %{from: {:subquery, %{ast: %{offset: 40}}}} = result
    assert ["Offset adjusted from 31 to 40"] = result.info
  end

  test "having conditions are not adjusted in the root query" do
    assert {:ok, _} = compile("select count(*) from table group by numeric having avg(numeric) > 3", data_source())
  end

  test "having condition inequalities must be ranges in subqueries" do
    assert {:error, error} =
      compile("select count(*) from (select uid from table group by uid having avg(numeric) > 3) x", data_source())
    assert error == "Column `avg` must be limited to a finite range."
  end

  test "having condition ranges are aligned with a message in subqueries" do
    %{from: {:subquery, %{ast: aligned}}} = compile!("""
      select count(*) from (select uid from table group by uid having avg(numeric) >= 0.0 and avg(numeric) < 5.0) x
    """, data_source()) |> scrub_aliases()
    %{from: {:subquery, %{ast: unaligned}}} = compile!("""
      select count(*) from (select uid from table group by uid having avg(numeric) > 0.1 and avg(numeric) <= 4.9) x
    """, data_source()) |> scrub_aliases()

    assert Map.drop(aligned, [:info]) == Map.drop(unaligned, [:info])
    assert unaligned.info == ["The range for column `avg` has been adjusted to 0.0 <= `avg` < 5.0."]
  end

  test "rejects `or` conditions" do
    {:error, error} = compile("select * from table where numeric = 1 or numeric = 2", data_source())
    assert error == "Combining conditions with `OR` is not allowed."
  end

  test "propagating ranges for shrink and drop from a singly-nested having" do
    query = compile!("""
      select * from (
        select uid from table group by uid
        having avg(numeric) >= 0.0 and avg(numeric) < 100.0
      ) x
    """, data_source())
    {:subquery, %{ast: subquery}} = query.from

    assert [%{column: %{name: column_alias}, interval: {0.0, 100.0}}] = query.ranges
    assert Enum.any?(subquery.db_columns, &(&1.alias == column_alias))
  end

  test "propagating ranges for shrink and drop from a singly-nested where" do
    query = compile!("""
      select * from (
        select uid from table
        where numeric >= 0.0 and numeric < 100.0
        group by uid
      ) x
    """, data_source())
    {:subquery, %{ast: subquery}} = query.from

    aliases = Enum.map(query.ranges, &(&1.column.name))
    columns = Enum.filter(subquery.db_columns, &(&1.alias in aliases))
    assert Enum.any?(columns, &match?(%{function: "min"}, &1))
    assert Enum.any?(columns, &match?(%{function: "max"}, &1))
  end

  test "propagating ranges for shrink and drop from a singly-nested where without aggregation" do
    query = compile!("""
      select * from (
        select uid from table
        where numeric >= 0.0 and numeric < 100.0
      ) x
    """, data_source())
    {:subquery, %{ast: subquery}} = query.from

    assert [%{column: %{name: alias}, interval: {0.0, 100.0}}] = query.ranges
    assert Enum.any?(subquery.db_columns, &(&1.alias == alias))
  end

  test "dotted columns can be used unquoted" do
    assert %{columns: [column("table", "column.with.dots")]} =
      compile!("select column.with.dots from table", dotted_data_source())
    assert %{columns: [column("table", "column.with.dots")]} =
      compile!("select table.column.with.dots from table", dotted_data_source())
  end

  test "view error" do
    assert {:error, error} = compile("select foo from table_view", data_source(),
      views: %{"table_view" => "select"})

    assert error == "Error in the view `table_view`: Expected `column definition` at line 1, column 7."
  end

  test "view error in show columns" do
    assert {:error, error} = compile("show columns from table_view", data_source(),
      views: %{"table_view" => "select"})

    assert error == "Error in the view `table_view`: Expected `column definition` at line 1, column 7."
  end

  test "ambiguous view/table error" do
    assert {:error, error} = compile("select numeric from table", data_source(),
      views: %{"table" => "select numeric from table"})

    assert error == "There is both a table, and a view named `table`. Rename the view to resolve the conflict."
  end

  test "view is treated as a subquery" do
    assert {:error, error} = compile("select numeric from table_view", data_source(),
      views: %{"table_view" => "select numeric from table"})

    assert error =~ ~r/Missing a user id column in the select list of subquery `table_view`./
  end

  test "view validation error" do
    assert {:error, error} = validate_view("select", data_source())
    assert error =~ ~r/Expected `column definition`/
  end

  test "view has the same limitations as the subquery" do
    assert {:error, error} = validate_view("select uid, extract_match(string, '') from table", data_source())
    assert error == "Function `extract_match` is not allowed in subqueries."
  end

  test "compilation of row splitters" do
    {:ok, query} = compile("select extract_matches(string, 'thing') from table", data_source())
    assert [%Expression{name: "extract_matches_return_value", row_index: index}] = query.columns
    assert Enum.any?(query.db_columns, &match?(%Expression{name: "string"}, &1))
    assert [%{
      function_spec: %Expression{
        function?: true,
        function: "extract_matches",
        function_args: [%Expression{name: "string"}, %Expression{value: ~r/thing/ui}]
      },
      row_index: ^index
    }] = query.row_splitters
  end

  test "only needed columns are fetched from a projected table" do
    assert ["table.uid", "projected_table.a"] ==
      projected_table_db_column_names(compile!("select a from projected_table", data_source()))

    assert ["table.uid", "projected_table.a", "projected_table.b"] ==
      projected_table_db_column_names(compile!("select a, b from projected_table", data_source()))
  end

  test "reindexing in projected tables" do
    assert [0, 1] ==
      projected_table_db_column_indices(compile!("select a from projected_table", data_source()))

    assert [0, 1, 2] ==
      projected_table_db_column_indices(compile!("select a, b from projected_table", data_source()))
  end

  test "filtered column is retrieved from a projected table", do:
    assert ["table.uid", "projected_table.a"] ==
      projected_table_db_column_names(compile!("select count(*) from projected_table where a=1", data_source()))

  test "rejecting non-selected ORDER BY with an aggregator function" do
    assert {:error, "Column `float` from table `table` needs to appear in the `GROUP BY` clause" <> _} =
      compile("SELECT SUM(numeric) FROM table ORDER BY float", data_source())
  end

  defp projected_table_db_columns(query), do:
    query
    |> get_in([all_subqueries()])
    |> Enum.find(&match?({:join, %{lhs: "projected_table"}}, &1.from))
    |> Map.fetch!(:db_columns)

  defp projected_table_db_column_names(query), do:
    Enum.map(projected_table_db_columns(query), &"#{&1.table.name}.#{&1.name}")

  defp projected_table_db_column_indices(query), do:
    Enum.map(projected_table_db_columns(query), &(&1.row_index))


  defp scrub_aliases(query), do: put_in(query, [aliases()], nil)

  deflens aliases, do:
    all_subqueries() |> Query.Lenses.terminals() |> Lens.satisfy(&match?(%Expression{}, &1)) |> Lens.key(:alias)

  deflens all_subqueries(), do:
    Lens.both(Lens.recur(Query.Lenses.direct_subqueries() |> Lens.key(:ast)), Lens.root())

  defp compile!(query_string, data_source, options \\ []) do
    {:ok, result} = compile(query_string, data_source, options)
    result
  end

  defp compile(query_string, data_source, options \\ []) do
    query = Parser.parse!(query_string)
    Compiler.compile(data_source, query, Keyword.get(options, :parameters, []),
      Keyword.get(options, :views, %{}))
  end

  defp validate_view(view_sql, data_source, options \\ []) do
    with {:ok, parsed_view} <- Parser.parse(view_sql), do:
      Compiler.validate_view(data_source, parsed_view, Keyword.get(options, :views, %{}))
  end

  defp data_source(driver \\ Cloak.DataSource.PostgreSQL) do
    %{driver: driver, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [
          {"uid", :integer}, {"column", :datetime}, {"numeric", :integer}, {"float", :real}, {"string", :text}
        ],
        projection: nil
      },
      other_table: %{
        db_name: "other_table",
        name: "other_table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"other_column", :datetime}],
        projection: nil
      },
      projected_table: %{
        db_name: "projected_table",
        name: "projected_table",
        user_id: "uid",
        columns: [{"fk", :integer}, {"a", :integer}, {"b", :integer}],
        projection: %{table: "table", foreign_key: "fk", primary_key: "numeric"}
      },
      t1: %{
        db_name: "t1",
        name: "t1",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}, {"c2", :integer}],
        projection: nil
      },
      t2: %{
        db_name: "t2",
        name: "t2",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}, {"c3", :integer}],
        projection: nil
      },
      t3: %{
        db_name: "t3",
        name: "t3",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}],
        projection: nil
      },
      t4: %{
        db_name: "t4",
        name: "t4",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}],
        projection: nil
      }
    }}
  end

  def time_data_source do
    %{driver: Cloak.DataSource.PostgreSQL, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"column", :time}],
        projection: nil
      }
    }}
  end

  def date_data_source do
    %{driver: Cloak.DataSource.PostgreSQL, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"column", :date}],
        projection: nil
      }
    }}
  end

  def dotted_data_source do
    %{driver: Cloak.DataSource.MongoDB, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"column.with.dots", :number}],
        projection: nil
      }
    }}
  end

  defp conditions_list(clause), do: Query.Lenses.conditions() |> Lens.to_list(clause)
end
