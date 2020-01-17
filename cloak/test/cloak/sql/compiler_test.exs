defmodule Cloak.Sql.Compiler.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Expression, Compiler, Parser, Query}

  import Cloak.Test.QueryHelpers

  defmacrop column(table_name, column_name) do
    quote do
      %{kind: :column, name: unquote(column_name), table: %{db_name: unquote(table_name)}}
    end
  end

  defmacrop column(column_name) do
    quote do
      %{kind: :column, name: unquote(column_name)}
    end
  end

  defmacrop function(function_name, args) do
    quote do
      %{kind: :function, name: unquote(function_name), args: unquote(args)}
    end
  end

  test "adds an empty group by" do
    assert %{group_by: []} = compile!("select * from table", data_source())
  end

  test "adds a non-nil condition on user_id for top query" do
    query = compile!("select stddev(uid) from (select uid, column from table) as t", data_source())
    assert function("not", [function("is_null", [column("uid")])]) = query.where
    {:subquery, %{ast: subquery}} = query.from
    assert nil == subquery.where
  end

  for operator <- [:>, :>=, :<, :<=] do
    test "rejects inequalities on strings with #{operator}" do
      {:error, error} = compile("select * from table where string #{unquote(operator)} 'CEO'", data_source())
      assert error == "Inequalities on `text` values are currently not supported."
    end

    test "rejects inequalities on booleans with #{operator}" do
      {:error, error} = compile("select * from table where bool #{unquote(operator)} true", data_source())
      assert error == "Inequalities on `boolean` values are currently not supported."
    end
  end

  test "rejects mistyped where conditions" do
    {:error, error} = compile("select * from table where numeric = column", data_source())

    assert error == "Arguments of type (`integer`, `datetime`) are incorrect for `=`."
  end

  test "rejects mistyped where conditions with constants" do
    {:error, error} = compile("select count(*) from table where string = 2", data_source())

    assert error == "Arguments of type (`text`, `integer`) are incorrect for `=`."
  end

  test "rejects mistyped like conditions" do
    {:error, error} = compile("select * from table where numeric like 'something'", data_source())

    assert error ==
             "Function `like` requires arguments of type (`text`, `like_pattern`), " <>
               "but got (`integer`, `like_pattern`)."
  end

  test "rejects mistyped having conditions" do
    {:error, error} =
      compile(
        """
          select count(*) from table group by numeric, column having numeric = column
        """,
        data_source()
      )

    assert error == "Arguments of type (`integer`, `datetime`) are incorrect for `=`."
  end

  test "rejects mistyped having conditions in subqueries" do
    {:error, error} =
      compile(
        """
          select count(*) from (
            select uid from table group by uid, numeric, column having numeric = column) x
        """,
        data_source()
      )

    assert error == "Arguments of type (`integer`, `datetime`) are incorrect for `=`."
  end

  describe "rejects non-boolean filtering clauses" do
    test "where" do
      assert {:error, error} = compile("select count(*) from table where 1", data_source())

      assert error == "Row filtering clauses have to be boolean expressions."
    end

    test "having" do
      assert {:error, error} = compile("select count(*) from table having 1", data_source())

      assert error == "Row filtering clauses have to be boolean expressions."
    end

    test "on" do
      assert {:error, error} = compile("select count(*) from table as t1 join table as t2 on 1", data_source())

      assert error == "Row filtering clauses have to be boolean expressions."
    end
  end

  describe "conditions usage" do
    test "rejects conditions in anonymizing select" do
      assert {:error, error} = compile("select numeric = 0 from table", data_source())

      assert error ==
               "Conditions can not be used outside the `WHERE`, `HAVING` and `ON` clauses in anonymizing queries."
    end

    test "rejects conditions in anonymizing group by" do
      assert {:error, error} = compile("select 1 from table group by numeric in (0, 1)", data_source())

      assert error ==
               "Conditions can not be used outside the `WHERE`, `HAVING` and `ON` clauses in anonymizing queries."
    end

    test "rejects conditions in anonymizing order by" do
      assert {:error, error} = compile("select 1 from table order by string like '%aaa'", data_source())

      assert error ==
               "Conditions can not be used outside the `WHERE`, `HAVING` and `ON` clauses in anonymizing queries."
    end

    test "rejects conditions in function calls" do
      assert {:error, error} =
               compile("select count(*) from table where cast(numeric > 0 as integer) = 0", data_source())

      assert error ==
               "Conditions can not be used outside the `WHERE`, `HAVING` and `ON` clauses in anonymizing queries."
    end

    test "allow conditions in non-filtering clauses in standard queries" do
      assert {:ok, _} = compile_standard("select numeric = 0 from table", data_source())
    end
  end

  describe "boolean expressions" do
    test "allow conjunctions in anonymizing select" do
      assert {:ok, _} = compile("select bool and not bool from table", data_source())
    end

    test "allow disjunctions in standard select" do
      assert {:ok, _} = compile_standard("select bool or not bool from table", data_source())
    end

    test "allow disjunctions in standard where" do
      assert {:ok, _} = compile_standard("select count(*) from table where bool or not bool", data_source())
    end

    test "reject disjunction in anonymizing select" do
      assert {:error, "Combining boolean expressions with `OR` is not allowed in anonymizing queries" <> _} =
               compile("select bool or not bool from table", data_source())
    end

    test "rejects disjunction in anonymizing where" do
      {:error, "Combining boolean expressions with `OR` is not allowed in anonymizing queries" <> _} =
        compile("select * from table where numeric = 1 or numeric = 2", data_source())
    end
  end

  test "select NULL" do
    assert {:ok, _} = compile("select null from table", data_source())
  end

  test "NULL can have any type" do
    assert {:ok, _} = compile("select numeric + null from table", data_source())
  end

  test "NULL can be compared with any type" do
    assert {:ok, _} = compile("select count(*) from table where column = null", data_source())
  end

  test "reject invalid select with having conditions without group by" do
    {:error, error} = compile("select string from table having count(numeric) = 2", data_source())

    assert error ==
             "Column `string` from table `table` needs to appear in the `GROUP BY`" <>
               " clause or be used in an aggregate function."
  end

  test "rejects escape strings longer than 1" do
    {:error, error} = compile("select * from table where string like 'something' escape 'abc'", data_source())

    assert error == "Escape string must be one character."
  end

  test "rejects usage of * in function requiring argument of known type" do
    {:error, error} = compile("select length(*) from table", data_source())
    assert error =~ ~r/unspecified type/
  end

  test "casts datetime where conditions" do
    result =
      compile!(
        "select stddev(uid) from table where column > '2015-01-01' and column < '2016-01-01'",
        data_source()
      )

    assert [_is_not_null_id, function(">=", [column("table", "column"), value]), _lt_date] =
             conditions_list(result.where)

    assert value == Expression.constant(:datetime, ~N[2015-01-01 00:00:00.000000])
  end

  test "[Issue #2152] an invalid datetime comparison",
    do:
      assert(
        {:error, "Arguments of type (`datetime`, `integer`) are incorrect for `=`."} =
          compile("select * from table where column = 1000 - 100", data_source())
      )

  test "[Issue #2562] doesn't cast expressions that are already datetime" do
    result = compile!("select stddev(uid) from table where column = cast('2017-01-01' as datetime)", data_source())

    assert [_is_not_null_id, function("=", [column("table", "column"), value])] = conditions_list(result.where)
    assert value == Expression.constant(:datetime, ~N[2017-01-01 00:00:00.000000])
  end

  test "[Issue #2562] doesn't cast expressions that are already datetime in IN" do
    result =
      compile!(
        "select stddev(uid) from table where column IN (cast('2017-01-01' as datetime), '2017-02-02')",
        data_source()
      )

    assert [_is_not_null_id, function("in", [column("table", "column"), value1, value2])] =
             conditions_list(result.where)

    assert value1 == Expression.constant(:datetime, ~N[2017-01-01 00:00:00.000000])
    assert value2 == Expression.constant(:datetime, ~N[2017-02-02 00:00:00.000000])
  end

  test "allows comparing datetime columns to other datetime columns" do
    assert {:ok, _} = compile("select * from table where column = column", data_source())
  end

  test "casts time where conditions" do
    assert %{where: function("and", [_is_not_null_id, range])} =
             compile!(
               "select stddev(uid) from table where column >= '01:00:00' and column < '02:00:00'",
               time_data_source()
             )

    assert function("and", [function(">=", [column("table", "column"), value]), _rhs]) = range
    assert value == Expression.constant(:time, ~T[01:00:00.000000])
  end

  test "casts date where conditions" do
    assert %{where: function("and", [_is_not_null_id, range])} =
             compile!(
               "select stddev(uid) from table where column >= '2015-01-01' and column < '2016-01-01'",
               date_data_source()
             )

    assert function("and", [function(">=", [column("table", "column"), value]), _rhs]) = range
    assert value == Expression.constant(:date, ~D[2015-01-01])
  end

  test "casts datetime in `in` conditions" do
    result = compile!("select stddev(uid) from table where column in ('2015-01-01', '2015-01-02')", data_source())

    assert function("and", [
             function("not", [function("is_null", [column("table", "uid")])]),
             function("in", [column("table", "column") | times])
           ]) = result.where

    assert times |> Enum.map(& &1.value) |> Enum.sort() ==
             [~N[2015-01-01 00:00:00.000000], ~N[2015-01-02 00:00:00.000000]]
  end

  test "casts datetime in negated conditions" do
    result = compile!("select stddev(uid) from table where column <> '2015-01-01'", data_source())

    assert function("and", [
             function("not", [function("is_null", [column("table", "uid")])]),
             function("<>", [column("table", "column"), value])
           ]) = result.where

    assert value == Expression.constant(:datetime, ~N[2015-01-01 00:00:00.000000])
  end

  test "casts integers to reals in IN" do
    result = compile!("select stddev(uid) from table where float IN (1, 1.1)", data_source())

    assert function("and", [
             function("not", [function("is_null", [column("table", "uid")])]),
             function("in", [column("table", "float") | values])
           ]) = result.where

    assert [%Expression{type: :real, value: 1}, %Expression{type: :real, value: 1.1}] = values
  end

  test "reports malformed datetimes" do
    assert {:error, "Cannot cast `something stupid` to datetime."} =
             compile("select * from table where column > 'something stupid'", data_source())
  end

  for function <- ~w(min max sum avg stddev variance) do
    test "allowing aggregator #{function}(numeric)" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "disallowing aggregator #{function}(distinct numeric)" do
      assert {:error, error} = compile("select #{unquote(function)}(distinct numeric) from table", data_source())

      assert error == "Only the `count` and `count_noise` functions support the `DISTINCT` modifier."
    end
  end

  for function <- ~w(min max) do
    test "allowing #{function} on text columns in subqueries" do
      assert {:ok, _} =
               compile(
                 """
                   select * from (select uid, #{unquote(function)}(string) from table group by uid) t
                 """,
                 data_source()
               )
    end

    test "allowing #{function} on datetime columns in subqueries" do
      assert {:ok, _} =
               compile(
                 """
                   select * from (select uid, #{unquote(function)}(column) from table group by uid) t
                 """,
                 data_source()
               )
    end

    test "rejecting #{function} on text columns in top query" do
      assert {:error, _} = compile("select #{unquote(function)}(string) from table", data_source())

      assert {:error, _} = compile("select #{unquote(function)}(distinct string) from table", data_source())
    end
  end

  for function <- ~w(abs sqrt) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end
  end

  test "rejecting sum on non-numerical columns" do
    assert {:error, error} = compile("select sum(string) from table", data_source())

    assert error == "Function `sum` requires arguments of type (`integer`) or (`real`), but got (`text`)."
  end

  test "rejecting sqrt on non-numerical columns" do
    assert {:error, error} = compile("select sqrt(column) from table", data_source())

    assert error == "Function `sqrt` requires arguments of type (`integer` | `real`), but got (`datetime`)."
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

  for function <- ~w(count avg min max sum stddev) do
    test "rejecting #{function} in group by" do
      query = "select #{unquote(function)}(numeric) from table group by #{unquote(function)}(numeric)"

      assert {:error, error} = compile(query, data_source())

      assert error == "Aggregate function `#{unquote(function)}` can not be used in the `GROUP BY` clause."
    end

    test "rejecting a complex expression with #{function} in group by" do
      query = "select #{unquote(function)}(numeric) from table group by #{unquote(function)}(numeric) + 1"

      assert {:error, error} = compile(query, data_source())

      assert error == "Aggregate function `#{unquote(function)}` can not be used in the `GROUP BY` clause."
    end
  end

  for function <- ~w(year quarter month day hour minute second weekday) do
    test "allowing #{function} on datetime columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table", data_source())
    end

    test "allowing #{function} in group by" do
      assert {:ok, _} =
               compile(
                 "select #{unquote(function)}(column) from table group by #{unquote(function)}(column)",
                 data_source()
               )
    end

    test "allowing #{function} in select when the argument is grouped" do
      assert {:ok, _} =
               compile(
                 "select #{unquote(function)}(column) from table group by column",
                 data_source()
               )
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

  for function <- ~w(floor ceil) do
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

      assert error ==
               "Function `#{unquote(function)}` requires arguments of type" <>
                 " (`integer` | `real`) or (`integer` | `real`, `constant integer`), but got (`datetime`)."
    end
  end

  test "multiarg function argument verification" do
    assert {:error, error} = compile("select pow(numeric, column) from table", data_source())

    assert error ==
             "Function `pow` requires arguments of type (`integer` | `real`, `integer` | `real`), " <>
               "but got (`integer`, `datetime`)."
  end

  test "rejecting a function with too many arguments" do
    assert {:error, error} = compile("select avg(numeric, column) from table", data_source())

    assert error == "Function `avg` requires arguments of type (`integer` | `real`), but got (`integer`, `datetime`)."
  end

  test "rejecting a function with too few arguments" do
    assert {:error, error} = compile("select pow(numeric) from table", data_source())

    assert error ==
             "Function `pow` requires arguments of type (`integer` | `real`, `integer` | `real`), " <>
               "but got (`integer`)."
  end

  test "rejecting a column in select when its function is grouped" do
    assert {:error, error} = compile("select column from table group by day(column)", data_source())

    assert error ==
             "Column `column` from table `table` needs to appear in the `GROUP BY` clause" <>
               " or be used in an aggregate function."
  end

  test "rejecting a function in select when another function is grouped" do
    assert {:error, error} =
             compile(
               "select pow(numeric, numeric) from table group by abs(numeric)",
               data_source()
             )

    assert error ==
             "Column `numeric` from table `table` needs to appear in the `GROUP BY` clause or be used in an " <>
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

  test "accepting constants as aggregated",
    do: assert({:ok, _} = compile("select count(*), 1, abs(1) from table", data_source()))

  test "accepting constants as aggregated in queries with group by",
    do: assert({:ok, _} = compile("select 1, abs(1) from table group by numeric", data_source()))

  test "accepting proper joins" do
    assert {:ok, _} = compile("SELECT t1.c1 from t1, t2 WHERE t1.uid = t2.uid", data_source())
    assert {:ok, _} = compile("SELECT t1.c1 from t1, t2 WHERE t2.uid = t1.uid", data_source())

    assert {:ok, _} =
             compile(
               "SELECT t1.c1 from t1, t2, t3 WHERE t1.uid = t2.uid AND t2.uid = t3.uid",
               data_source()
             )

    assert {:ok, _} =
             compile(
               "SELECT t1.c1 from t1, t2, t3 WHERE t3.uid = t1.uid AND t3.uid = t2.uid",
               data_source()
             )

    assert {:ok, _} =
             compile(
               "SELECT t1.c1 from t1, t2, t3, t4 WHERE t3.uid = t1.uid AND t3.uid = t2.uid AND t1.uid = t4.uid",
               data_source()
             )
  end

  test "rejecting missing column" do
    assert {:error, "Column `a` doesn't exist in table `table`."} = compile("SELECT a FROM table", data_source())
  end

  test "rejecting missing column on join" do
    assert {:error, "Column `a` doesn't exist in any of the selected tables."} =
             compile("SELECT a FROM t1, t2", data_source())
  end

  test "rejecting missing qualified column" do
    assert {:error, "Column `a` doesn't exist in table `table`."} = compile("SELECT table.a FROM table", data_source())
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
             compile(
               "SELECT column FROM table WHERE other_table.other_column <> ''",
               data_source()
             )
  end

  test "rejecting joins with only one side of a range" do
    assert {:error, "Column `numeric` from table `table` must be limited to a finite, nonempty range."} =
             compile(
               "SELECT * FROM table JOIN other_table ON table.uid = other_table.uid AND numeric > 3",
               data_source()
             )
  end

  test "aligning ranges in joins" do
    query1 =
      compile!(
        "SELECT stddev(table.uid) FROM table JOIN other_table ON table.uid = other_table.uid AND numeric > 3" <>
          " AND numeric < 9",
        data_source()
      )

    query2 =
      compile!(
        "SELECT stddev(table.uid) FROM table JOIN other_table ON table.uid = other_table.uid AND numeric >= 0" <>
          " AND numeric < 10",
        data_source()
      )

    assert query1.info ==
             [
               "The range for column `numeric` from table `table` has been adjusted to 0.0 <= `numeric` < 10.0."
             ]

    {:join, %{condition: conditions1}} = query1.from
    {:join, %{condition: conditions2}} = query2.from
    assert conditions_list(conditions1) == conditions_list(conditions2)
  end

  test "rejecting improper joins" do
    assert {:error, "The tables `t1` and `t2` are not joined " <> _} =
             compile("SELECT t1.c1 from t1, t2", data_source())

    assert {:error, "The tables `t1` and `t3` are not joined " <> _} =
             compile("SELECT t1.c1 from t1, t2, t3 WHERE t1.uid = t2.uid", data_source())

    assert {:error, "The tables `t1` and `t3` are not joined " <> _} =
             compile(
               "SELECT t1.c1 from t1, t2, t3, t4 WHERE t1.uid = t2.uid AND t3.uid = t4.uid",
               data_source()
             )
  end

  test "rejecting improper joins with aliases" do
    assert {:error, "The tables `a1` and `a2` are not joined " <> _} =
             compile("SELECT a1.c1 from t1 a1, t2 a2", data_source())
  end

  test "rejecting a join with a subquery that is unconnected" do
    assert {:error, "The tables `sq` and `t1` are not joined " <> _} =
             compile("SELECT t1.c1 from t1, (select c1 from t2) sq", data_source())
  end

  test "rejecting a join when cast changes the uid type" do
    assert {:error, error} =
             compile(
               "SELECT t1.c1 from t1, t2 WHERE cast(t1.uid as text) = t2.uid",
               data_source()
             )

    assert error == "Arguments of type (`text`, `integer`) are incorrect for `=`."
  end

  test "allows qualified identifiers in function calls" do
    assert %{
             columns: [
               %Expression{
                 name: "variance",
                 args: [column("table", "numeric")]
               }
             ]
           } = compile!("select variance(table.numeric) from table", data_source())
  end

  test "qualifies all identifiers" do
    result =
      compile!(
        """
          SELECT column, variance(numeric)
          FROM table
          WHERE numeric >= 1 and numeric < 9 and column <> '2015-01-02'
          GROUP BY column
          ORDER BY variance(numeric) DESC, variance(table.numeric) DESC
        """,
        data_source()
      )

    assert [
             column("table", "column"),
             %Expression{name: "variance", args: [column("table", "numeric")]}
           ] = result.columns

    conditions = conditions_list(result.where)
    assert Enum.any?(conditions, &match?(function(">=", [column("table", "numeric"), _]), &1))
    assert Enum.any?(conditions, &match?(function("<", [column("table", "numeric"), _]), &1))
    assert Enum.any?(conditions, &match?(function("not", [function("is_null", [column("table", "uid")])]), &1))
    assert Enum.any?(conditions, &match?(function("<>", [column("table", "column"), _]), &1))
    assert [column("table", "column")] = result.group_by
    assert [{expr_1, :desc, _}, {expr_2, :desc, _}] = result.order_by
    assert %Expression{name: "variance"} = expr_1
    assert %Expression{name: "variance"} = expr_2
  end

  test "complains when tables don't exist" do
    assert {:error, "Table `t_doesnt_exist` doesn't exist."} =
             compile("SELECT c1 FROM t1, t_doesnt_exist", data_source())
  end

  test "expands all columns for all tables when joining" do
    result =
      compile!(
        "SELECT * FROM t1, t2 JOIN t3 on t2.uid = t3.uid WHERE t1.uid = t2.uid",
        data_source()
      )

    assert [
             %{value: :*},
             column("__ac_group_0"),
             column("__ac_group_1"),
             %{value: :*},
             column("__ac_group_2"),
             column("__ac_group_3"),
             %{value: :*},
             column("__ac_group_4")
           ] = result.columns
  end

  test "complains when an unqualified identifier cannot be pinned down" do
    assert {:error, "Column `c1` is ambiguous."} = compile("SELECT c1 FROM t1, t2", data_source())
  end

  test "allows for where, order by and group by when performing cross join" do
    result =
      compile!(
        """
          SELECT t1.c1, STDDEV(t1.uid)
          FROM t1, t2
          WHERE c2 > 10 AND c2 < 20
          AND t1.uid = t2.uid
          GROUP BY t1.c1, c3
          ORDER BY t1.c1 DESC
        """,
        data_source()
      )

    conditions = conditions_list(result.where)
    assert [column("t1", "c1"), _] = result.columns
    assert Enum.any?(conditions, &match?(function(">=", [column("t1", "c2"), _]), &1))
    assert Enum.any?(conditions, &match?(function("<", [column("t1", "c2"), _]), &1))

    assert Enum.any?(
             conditions,
             &match?(function("=", [column("t1", "uid"), column("t2", "uid")]), &1)
           )

    assert [column("t1", "c1"), column("t2", "c3")] = result.group_by
    assert [{column("t1", "c1"), :desc, _}] = result.order_by
  end

  test "complains when conditions not on columns of JOINed tables" do
    assert {:error, "Column `c3` of table `t2` is used out of scope."} =
             compile(
               """
                 SELECT t1.c1
                 FROM
                   t1 INNER JOIN t3 ON t1.uid = t3.uid and t2.c3 > 10,
                   t2
                 WHERE t2.uid = t1.uid
               """,
               data_source()
             )

    assert {:error, "Column `c3` of table `t2` is used out of scope."} =
             compile(
               """
                 SELECT t1.c1
                 FROM
                   t1 INNER JOIN t3 ON t1.uid = t3.uid and c3 > 10,
                   t2
                 WHERE t2.uid = t1.uid
               """,
               data_source()
             )
  end

  test "complains on ambiguous JOIN on condition" do
    assert {:error, "Column `c1` is ambiguous."} =
             compile(
               """
                 SELECT t1.c1
                 FROM t1 INNER JOIN t2 ON t1.uid = t2.uid and c1 > 10
               """,
               data_source()
             )
  end

  test "Can JOIN on columns from earlier JOIN" do
    assert {:ok, _} =
             compile(
               """
                 SELECT t1.c1
                 FROM
                   t1 INNER JOIN t2 ON t1.uid = t2.uid
                      INNER JOIN t3 ON t3.uid = t1.uid AND t1.c2 > 10 AND t1.c2 < 20
               """,
               data_source()
             )
  end

  test "rejecting invalid casts" do
    assert {:error, error} = compile("select cast(column as integer) from table", data_source())
    assert error == "Cannot cast value of type `datetime` to type `integer`."
  end

  test "accepting valid casts" do
    assert {:ok, _} = compile("select cast(column as date) from table", data_source())
  end

  test "missing group by in a subquery" do
    assert {:error, error} = compile("select c1 from (select uid, count(*) as c1 from t1) alias", data_source())

    assert error =~ "Column `uid` from table `t1` needs to appear in the `GROUP BY`"
  end

  test "integer operations are valid on sums of integer columns" do
    assert {:ok, _} = compile("select abs(sum(numeric)) from table", data_source())
  end

  test "incorrect application of +" do
    assert {:error, error} = compile("select 'a' + 'b' from table", data_source())
    assert error == "Arguments of type (`text`, `text`) are incorrect for `+`."
  end

  test "rejects inequalities on numeric columns that are not ranges" do
    assert {:error, error} = compile("select * from table where numeric > 5", data_source())

    assert error == "Column `numeric` from table `table` must be limited to a finite, nonempty range."
  end

  test "rejects inequalities on numeric columns that have equal endpoints" do
    assert {:error, error} =
             compile(
               "select count(*) from table where numeric > 10 and numeric < 10",
               data_source()
             )

    assert error == "Column `numeric` from table `table` must be limited to a finite, nonempty range."
  end

  test "rejects inequalities on numeric columns that are negatives of ranges" do
    assert {:error, error} = compile("select * from table where numeric < 2 and numeric > 5", data_source())

    assert error == "Column `numeric` from table `table` must be limited to a finite, nonempty range."
  end

  test "rejects inequalities on datetime columns that are negatives of ranges" do
    assert {:error, error} =
             compile(
               "select * from table where column < '2015-01-01' and column > '2016-01-01'",
               data_source()
             )

    assert error == "Column `column` from table `table` must be limited to a finite, nonempty range."
  end

  test "rejects inequalities on datetime columns that are not ranges" do
    assert {:error, error} = compile("select * from table where column > '2015-01-01'", data_source())

    assert error == "Column `column` from table `table` must be limited to a finite, nonempty range."
  end

  test "rejects inequalities on date columns that are negatives of ranges" do
    assert {:error, error} =
             compile(
               "select * from table where column < '2015-01-01' and column > '2016-01-01'",
               date_data_source()
             )

    assert error == "Column `column` from table `table` must be limited to a finite, nonempty range."
  end

  test "rejects inequalities on date columns that are not ranges" do
    assert {:error, error} = compile("select * from table where column > '2015-01-01'", data_source())

    assert error == "Column `column` from table `table` must be limited to a finite, nonempty range."
  end

  test "rejects datetime ranges smaller than 1 second" do
    assert {:error, error} =
             compile(
               """
                 select * from table
                 where column between '2017-01-02 12:22:33.010000' and '2017-01-02 12:22:33.020000'
               """,
               data_source()
             )

    assert error == "Column `column` from table `table` must be limited to a finite, nonempty range."
  end

  test "accepts inequalities on numeric columns that are ranges" do
    assert {:ok, _} = compile("select * from table where numeric > 5 and numeric < 8", data_source())
  end

  test "accepts inequalities without a constant side in top-level HAVING" do
    assert {:ok, _} =
             compile(
               "select count(*) from table group by numeric having numeric > numeric and numeric < 10",
               data_source()
             )
  end

  test "fixes alignment of ranges" do
    assert compile!("select * from table where numeric > 1 and numeric < 9", data_source()).where ==
             compile!("select * from table where numeric > 0 and numeric < 10", data_source()).where
  end

  test "fixes alignment of datetime ranges" do
    aligned =
      compile!(
        "select * from table where column > '2015-01-02' and column < '2016-07-01'",
        data_source()
      )

    assert compile!(
             "select * from table where column > '2015-01-01' and column < '2016-08-02'",
             data_source()
           ).where == aligned.where

    assert aligned.info == [
             "The range for column `column` from table `table` has been adjusted to " <>
               "2015-01-01 00:00:00.000000 <= `column` < 2017-01-01 00:00:00.000000."
           ]
  end

  test "no message when datetime alignment does not require fixing" do
    assert compile!(
             "select * from table where column >= '2006-05-31' and column < '2006-06-01'",
             data_source()
           ).info == []
  end

  test "fixes alignment of date ranges" do
    aligned =
      compile!(
        "select * from table where column > '2015-01-02' and column < '2016-07-01'",
        date_data_source()
      )

    assert compile!(
             "select * from table where column > '2015-01-01' and column < '2016-08-02'",
             date_data_source()
           ).where == aligned.where

    assert aligned.info == [
             "The range for column `column` from table `table` has been adjusted to 2015-01-01 <= " <>
               "`column` < 2017-01-01."
           ]
  end

  test "fixes alignment of time ranges" do
    aligned =
      compile!(
        "select * from table where column > '00:00:01' and column < '00:00:04'",
        time_data_source()
      )

    unaligned =
      compile!(
        "select * from table where column >= '00:00:00' and column < '00:00:05'",
        time_data_source()
      )

    assert conditions_list(unaligned.where) == conditions_list(aligned.where)

    assert aligned.info == [
             "The range for column `column` from table `table` has been adjusted to 00:00:00.000000 <= " <>
               "`column` < 00:00:05.000000."
           ]
  end

  test "no message when time alignment does not require fixing" do
    assert compile!(
             "select * from table where column >= '00:00:00' and column < '00:00:05'",
             time_data_source()
           ).info == []
  end

  test "includes an info message when the aligment is fixed" do
    assert [msg] =
             compile!(
               "select count(*) from table where numeric >= 0.1 and numeric < 1.9",
               data_source()
             ).info

    assert msg == "The range for column `numeric` from table `table` has been adjusted to 0.0 <= `numeric` < 2.0."
  end

  test "columns for fetching are not duplicated" do
    columns = compile!("select stddev(uid) from table where numeric >= 0.1 and numeric < 1.9", data_source()).db_columns

    assert Enum.count(columns, &(&1.name == "numeric")) == 1
  end

  test "does not include an info message when the alignment does not need to be fixed" do
    assert compile!(
             "select count(*) from table where numeric >= 1 and numeric < 2",
             data_source()
           ).info == []
  end

  test "bugfix: allows fully qualified column names when distinct case in column alias" do
    assert %Query{} =
             compile!(
               """
                 SELECT
                   table.column
                 FROM table JOIN (
                   SELECT uid, column as Column -- only differs from original column in case...
                   FROM table
                 ) as table_subquery ON table.uid = table_subquery.uid
                 GROUP BY 1
               """,
               data_source()
             )
  end

  test "silently discards redundant inequalities" do
    query1 = "select count(*) from table where numeric >= 1 and numeric > 0.9 and numeric < 2 and numeric <= 2.1"
    query2 = "select count(*) from table where numeric >= 1 and numeric < 2"
    assert compile!(query1, data_source()) == compile!(query2, data_source())
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
    first = compile!("select bucket(numeric by 0.11) as foo, stddev(uid) from table group by numeric", data_source())
    second = compile!("select bucket(numeric by 0.1) as foo, stddev(uid) from table group by numeric", data_source())

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
    result =
      compile!(
        "select stddev(uid) from (select * from table order by numeric limit 24) foo",
        data_source()
      )

    assert %{from: {:subquery, %{ast: %{limit: 20}}}} = result
    assert ["Limit adjusted from 24 to 20"] = result.info
  end

  test "minimum limit is 10 in subqueries" do
    result =
      compile!(
        "select stddev(uid) from (select * from table order by numeric limit 5) foo",
        data_source()
      )

    assert %{from: {:subquery, %{ast: %{limit: 10}}}} = result
    assert ["Limit adjusted from 5 to 10"] = result.info
  end

  test "limit is not changed in the root query" do
    result = compile!("select numeric, stddev(uid) from table group by 1 order by 1 limit 9", data_source())
    assert result.limit == 9
  end

  test "offset requires limit in subqueries" do
    assert {:error, error} =
             compile(
               "select count(*) from (select * from table order by numeric offset 20) foo",
               data_source()
             )

    assert error =~ ~r/`OFFSET` clause requires a `LIMIT` clause in `restricted` subqueries/
  end

  test "offset must be a multiple of limit post-alignment" do
    result =
      compile!(
        "select stddev(uid) from (select * from table order by numeric limit 20 offset 31) foo",
        data_source()
      )

    assert %{from: {:subquery, %{ast: %{offset: 40}}}} = result
    assert ["Offset adjusted from 31 to 40"] = result.info
  end

  test "having conditions are not adjusted in the root query" do
    assert {:ok, _} =
             compile(
               "select count(*) from table group by numeric having avg(numeric) > 3",
               data_source()
             )
  end

  test "having condition inequalities must be ranges in subqueries" do
    assert {:error, error} =
             compile(
               "select count(*) from (select uid from table group by uid having avg(numeric) > 3) x",
               data_source()
             )

    assert error == "Column `avg` must be limited to a finite, nonempty range."
  end

  test "having condition ranges are aligned with a message in subqueries" do
    %{from: {:subquery, %{ast: aligned}}} =
      compile!(
        """
          select stddev(uid) from (select uid from table group by uid having avg(numeric) >= 0.0 and avg(numeric) < 5.0) x
        """,
        data_source()
      )

    %{from: {:subquery, %{ast: unaligned}}} =
      compile!(
        """
          select stddev(uid) from (select uid from table group by uid having avg(numeric) > 0.1 and avg(numeric) <= 4.9) x
        """,
        data_source()
      )

    assert aligned |> Map.drop([:info, :column_titles]) |> scrub_locations() ==
             unaligned |> Map.drop([:info, :column_titles]) |> scrub_locations()

    assert unaligned.info == [
             "The range for column `avg` has been adjusted to 0.0 <= `avg` < 5.0."
           ]
  end

  test "rejects `FULL OUTER JOINs`" do
    {:error, error} =
      compile(
        "select * from table full join other_table ON table.uid = other_table.uid",
        data_source()
      )

    assert error == "FULL OUTER JOINs are not currently allowed."
  end

  test "dotted columns can be used unquoted" do
    assert %{columns: [column("table", "column.with.dots"), _]} =
             compile!("select column.with.dots, stddev(uid) from table group by 1", dotted_data_source())

    assert %{columns: [column("table", "column.with.dots"), _]} =
             compile!("select table.column.with.dots, stddev(uid) from table group by 1", dotted_data_source())
  end

  test "view error" do
    assert {:error, error} =
             compile(
               "select foo from table_view",
               data_source(),
               views: %{"table_view" => "select"}
             )

    assert error == "Error in the view `table_view`: Expected `column definition` at line 1, column 7."
  end

  test "view error in show columns" do
    assert {:error, error} =
             compile(
               "show columns from table_view",
               data_source(),
               views: %{"table_view" => "select"}
             )

    assert error == "Error in the view `table_view`: Expected `column definition` at line 1, column 7."
  end

  test "ambiguous view/table error" do
    assert {:error, error} =
             compile(
               "select numeric from table",
               data_source(),
               views: %{"table" => "select numeric from table"}
             )

    assert error == "There is both a table, and a view named `table`. Rename the view to resolve the conflict."
  end

  test "view is treated as a subquery" do
    assert {:ok, _query} =
             compile(
               "select numeric from table_view",
               data_source(),
               views: %{"table_view" => "select numeric from table group by numeric"}
             )
  end

  test "view validation error" do
    assert {:error, error} = validate_view("select", data_source())
    assert error =~ ~r/Expected `column definition`/
  end

  test "rejecting non-aggregated non-selected ORDER BY column in an aggregated function" do
    assert {:error, "Column `float` from table `table` needs to appear in the `GROUP BY` clause" <> _} =
             compile("SELECT SUM(numeric) FROM table ORDER BY float", data_source())
  end

  test "rejecting non-aggregated column when an aggregate is in a non-selected ORDER BY" do
    assert {:error, "Column `numeric` from table `table` needs to appear in the `GROUP BY` clause" <> _} =
             compile("SELECT numeric FROM table ORDER BY max(float)", data_source())
  end

  test "rejecting non-aggregated column when count(*) is in a non-selected ORDER BY" do
    assert {:error, "Column `numeric` from table `table` needs to appear in the `GROUP BY` clause" <> _} =
             compile("SELECT numeric FROM table ORDER BY count(*)", data_source())
  end

  test "rejecting duplicate table",
    do:
      assert(
        {:error, "Table name `t1` specified more than once."} == compile("SELECT t1.uid from t1, t1", data_source())
      )

  test "rejecting duplicate subquery",
    do:
      assert(
        {:error, "Table name `a` specified more than once."} ==
          compile("SELECT a.uid from (select * from t1) a, (select * from t1) a", data_source())
      )

  test "real name of an aliased table can't be used as a prefix" do
    assert {:error, reason} = compile("select t1.c1 from t1 a", data_source())
    assert reason == "Missing FROM clause entry for table `t1`."
  end

  test "can't use the same alias twice" do
    assert {:error, reason} = compile("select t1.c1 from t1 a, t2 a", data_source())
    assert reason == "Table alias `a` used more than once."
  end

  test "the first argument to date_trunc has to be a constant" do
    assert {:error, reason} = compile("select date_trunc(string, column) from table", data_source())

    assert reason ==
             "Function `date_trunc` requires arguments of type (`constant text`, `date`)" <>
               " or (`constant text`, `datetime`) or (`constant text`, `time`), but got (`text`, `datetime`)."
  end

  test "rejecting aggregates in the WHERE-clause" do
    assert {:error, reason} =
             compile(
               "select count(*) from table where count(*) > 10 group by numeric",
               data_source()
             )

    assert reason == "Expression `count` is not valid in the `WHERE` clause."
  end

  describe "key columns" do
    test "marking key columns" do
      result = compile!("SELECT key, stddev(uid) FROM table group by 1", data_source())

      assert result.columns |> Enum.at(0) |> Expression.key?()
    end

    test "marking aliased key columns" do
      result = compile!("SELECT key AS something FROM table", data_source())
      assert result.columns |> Enum.at(0) |> Expression.key?()
    end

    test "marking key columns from subqueries" do
      result =
        compile!(
          "SELECT something FROM (SELECT uid, key as something FROM table) foo",
          data_source()
        )

      assert result.columns |> Enum.at(0) |> Expression.key?()
    end
  end

  describe "remove redundant casts" do
    Enum.each(
      [
        {:integer, "numeric"},
        {:float, "float"},
        {:datetime, "column"},
        {:text, "string"}
      ],
      fn {target, column} ->
        test "removes redundant cast to #{target}",
          do:
            assert(
              compile!(
                "SELECT cast(#{unquote(column)} as #{unquote(target)}) as c FROM table",
                data_source()
              ) == compile!("SELECT #{unquote(column)} as c FROM table", data_source())
            )
      end
    )
  end

  test "unsafe functions can't be used in non-standard queries" do
    assert {:error, error} = compile("SELECT dec_b64(string) FROM table", data_source())
    assert error =~ "Function `dec_b64` can only be used in non-anonymizing queries."
  end

  test "`case` requires a default branch" do
    assert {:error, "`case` expression requires a default branch."} =
             compile("select case(true, string) from table", data_source())
  end

  test "`case` with default branch only works" do
    assert {:ok, _} = compile("select case(string) from table", data_source())
  end

  test "`case` test conditions have to be booleans" do
    assert {:error, "`case` expression requires a `boolean` argument for the test condition."} =
             compile("select case(true, 1, string, 0, 2) from table", data_source())
  end

  test "`case` return values have to be identical" do
    assert {:error, "`case` expression requires that all branches return the same type."} =
             compile("select case(false, string, 2) from table", data_source())
  end

  test "rejects usage of distinct in non-aggregates" do
    {:error, error} = compile("select length(distinct string) from table", data_source())
    assert error =~ "Only the `count` and `count_noise` functions support the `DISTINCT` modifier."
  end

  test "rejects *-selecting duplicated columns" do
    {:error, error} =
      compile("select * from (select uid, count(*), count(numeric) from table group by 1) t", data_source())

    assert error =~ "Selecting all from subquery `t` is not supported because the column name `count` is ambiguous."
  end

  test "quoted alias reference",
    do: assert({:ok, _} = compile(~s/select numeric as x from table where "x"=1/, data_source()))

  test "error duplicate aliases with mixed quoting" do
    assert({:error, error} = compile(~s/select numeric as x, uid as "x" from table where "x"=1/, data_source()))
    assert error =~ "Usage of `x` is ambiguous."
  end

  test "output user ids are censored" do
    assert %{
             columns: [%Expression{value: :*}],
             group_by: [%Expression{value: :*}],
             order_by: [{%Expression{value: :*}, _, _}]
           } = compile!("select uid from table group by 1 order by 1", data_source())
  end

  test "internal queries are validated" do
    assert {:error, "Expected a constant of type `integer`, got `text`."} =
             compile_standard("select count(*) from table where numeric in (3, 's')", data_source())
  end

  test "rejects numeric constant values out of range" do
    {:error, error} = compile("select count(numeric - 10^19) from table", data_source())

    assert error =~
             "Constant expression is out of valid range: numeric values have to be inside the interval [-10^18, 10^18]."
  end

  test "rejects date constant values out of range" do
    {:error, error} = compile("select count(date '1801-01-01' - interval 'P10Y') from table", data_source())

    assert error =~
             "Constant expression is out of valid range: date values have to be inside the interval [`1800-01-01`, `2999-12-31`]."
  end

  test "rejects interval constant values out of range" do
    {:error, error} = compile("select count(- 99 * interval 'P10Y') from table", data_source())

    assert error =~ "Constant expression is out of valid range: interval values have to be less than `100` years."
  end

  test "invalid parameter format" do
    assert {:error, "Invalid parameter format for type `date` - `something stupid`."} =
             compile("SELECT COUNT(*) FROM table WHERE column = $1::datetime", data_source(),
               parameters: [%{type: :date, value: "something stupid"}]
             )
  end

  test "casting date parameter" do
    assert {:error, "Constant expression is out of valid range" <> _} =
             compile("SELECT COUNT(*) FROM table WHERE column = $1::datetime", data_source(),
               parameters: [%{type: :date, value: "4000-01-01"}]
             )
  end

  test "casting datetime parameter" do
    assert {:error, "Constant expression is out of valid range" <> _} =
             compile("SELECT COUNT(*) FROM table WHERE column = $1", data_source(),
               parameters: [%{type: :datetime, value: "4000-01-01 10:20:30"}]
             )
  end

  test "casting interval parameter" do
    assert {:error, "Constant expression is out of valid range" <> _} =
             compile("SELECT COUNT(*) FROM table WHERE column - column = $1", data_source(),
               parameters: [%{type: :interval, value: "P1000Y"}]
             )
  end

  test "casting time parameter" do
    assert [%{type: :time, value: ~T[10:20:30]}] =
             compile!("SELECT COUNT(*) FROM table WHERE cast(column as time) = $1", data_source(),
               parameters: [%{type: :time, value: "10:20:30"}]
             ).parameters
  end

  test "casting null parameter" do
    assert {:ok, _} =
             compile("SELECT COUNT(*) FROM table WHERE column = $1::datetime", data_source(),
               parameters: [%{type: :date, value: nil}]
             )
  end

  test "NOT IN is not rewritten in standard queries" do
    {:ok, result} = compile_standard("select count(*) from table where numeric not in (1, 2, 3)", data_source())

    assert [function("not", [function("in", [column("table", "numeric"), value1, value2, value3])])] =
             conditions_list(result.where)

    assert value1 = Expression.constant(:integer, 1)
    assert value2 = Expression.constant(:integer, 2)
    assert value3 = Expression.constant(:integer, 3)
  end

  defp compile_standard(query_string, data_source) do
    {:ok, parsed_query} = Parser.parse(query_string)

    try do
      {:ok, Compiler.compile_standard!(parsed_query, nil, data_source)}
    rescue
      error in Cloak.Sql.CompilationError -> {:error, error.message}
    end
  end

  defp validate_view(view_sql, data_source, options \\ []) do
    with {:ok, parsed_view} <- Parser.parse(view_sql),
         do: Compiler.validate_view(nil, data_source, parsed_view, Keyword.get(options, :views, %{}))
  end

  defp data_source() do
    %{
      name: "compiler_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("column", :datetime),
              Table.column("numeric", :integer),
              Table.column("float", :real),
              Table.column("string", :text),
              Table.column("key", :integer),
              Table.column("bool", :boolean)
            ],
            keys: %{"key" => :unknown}
          ),
        other_table:
          Cloak.DataSource.Table.new(
            "other_table",
            "uid",
            db_name: "other_table",
            columns: [
              Table.column("uid", :integer),
              Table.column("other_column", :datetime)
            ]
          ),
        t1:
          Cloak.DataSource.Table.new(
            "t1",
            "uid",
            db_name: "t1",
            columns: [
              Table.column("uid", :integer),
              Table.column("c1", :integer),
              Table.column("c2", :integer)
            ]
          ),
        t2:
          Cloak.DataSource.Table.new(
            "t2",
            "uid",
            db_name: "t2",
            columns: [
              Table.column("uid", :integer),
              Table.column("c1", :integer),
              Table.column("c3", :integer)
            ]
          ),
        t3:
          Cloak.DataSource.Table.new(
            "t3",
            "uid",
            db_name: "t3",
            columns: [
              Table.column("uid", :integer),
              Table.column("c1", :integer)
            ]
          ),
        t4:
          Cloak.DataSource.Table.new(
            "t4",
            "uid",
            db_name: "t4",
            columns: [
              Table.column("uid", :integer),
              Table.column("c1", :integer)
            ]
          )
      }
    }
  end

  def time_data_source do
    %{
      name: "compiler_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("column", :time)
            ]
          )
      }
    }
  end

  def date_data_source do
    %{
      name: "compiler_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("column", :date)
            ]
          )
      }
    }
  end

  def dotted_data_source do
    %{
      name: "compiler_test_data_source",
      driver: Cloak.DataSource.MongoDB,
      driver_info: "3.6.0",
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("column.with.dots", :number)
            ]
          )
      }
    }
  end

  defp conditions_list(clause), do: Query.Lenses.conditions() |> Lens.to_list(clause)
end
