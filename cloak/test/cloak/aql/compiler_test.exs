defmodule Cloak.Aql.Compiler.Test do
  use ExUnit.Case, async: true

  import Lens.Macros

  alias Cloak.Aql.{Column, Compiler, Parser, Query}

  defmacrop column(table_name, column_name) do
    quote do
      %{name: unquote(column_name), table: %{db_name: unquote(table_name)}}
    end
  end

  test "adds an empty group by" do
    assert %{group_by: []} = compile!("select * from table", data_source())
  end

  test "adds a non-nil condition on user_id" do
    query = compile!("select * from table", data_source())
    assert [{:not, {:is, %{name: "uid"}, :null}}] = query.where
    assert [] = query.lcf_check_conditions
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

  test "casts datetime where conditions" do
    result = compile!("select * from table where column > '2015-01-01' and column < '2016-01-01'", data_source())

    assert [{:comparison, column("table", "column"), :>=, value} | _] = result.where
    assert value == Column.constant(:datetime, ~N[2015-01-01 00:00:00.000000])
  end

  test "allows comparing datetime columns to other datetime columns" do
    assert {:ok, _} = compile("select * from table where column = column", data_source())
  end

  test "casts time where conditions" do
    assert %{where: [{:comparison, column("table", "column"), :>=, value} | _]} =
      compile!("select * from table where column >= '01:00:00' and column < '02:00:00'", time_data_source())
    assert value == Column.constant(:time, ~T[01:00:00.000000])
  end

  test "casts date where conditions" do
    assert %{where: [{:comparison, column("table", "column"), :>=, value} | _]} =
      compile!("select * from table where column >= '2015-01-01' and column < '2016-01-01'", date_data_source())
    assert value == Column.constant(:date, ~D[2015-01-01])
  end

  test "casts datetime in `in` conditions" do
    result = compile!("select * from table where column in ('2015-01-01', '2015-01-02')", data_source())

    assert [{:in, column("table", "column"), times}] = result.lcf_check_conditions
    assert times |> Enum.map(&(&1.value)) |> Enum.sort() ==
      [~N[2015-01-01 00:00:00.000000], ~N[2015-01-02 00:00:00.000000]]
  end

  test "casts datetime in negated conditions" do
    result = compile!("select * from table where column <> '2015-01-01'", data_source())

    assert [{:not, {:comparison, column("table", "column"), :=, value}}] =
      result.lcf_check_conditions
    assert value == Column.constant(:datetime, ~N[2015-01-01 00:00:00.000000])
  end

  test "reports malformed datetimes" do
    assert {:error, "Cannot cast `something stupid` to datetime."} =
      compile("select * from table where column > 'something stupid'", data_source())
  end

  for function <- ~w(sum median) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "rejecting #{function} on non-numerical columns" do
      assert {:error, error} = compile("select #{unquote(function)}(string) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer`) or (`real`), but got (`text`)."
    end
  end

  for function <- ~w(min max) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "rejecting #{function} on non-numerical columns" do
      assert {:error, error} = compile("select #{unquote(function)}(string) from table", data_source())
      assert error == "Arguments of type (`text`) are incorrect for `#{unquote(function)}`."
    end
  end

  for function <- ~w(avg stddev abs sqrt) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "rejecting #{function} on non-numerical columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer` | `real`), but got (`datetime`)."
    end
  end

  for function <- ~w(count) do
    test "allowing #{function} on non-numerical columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table", data_source())
    end
  end

  for function <- ~w(count avg min max sum stddev median) do
    test "rejecting #{function} in group by" do
      query = "select #{unquote(function)}(numeric) from table group by #{unquote(function)}(numeric)"
      assert {:error, error} = compile(query, data_source)
      assert error == "Aggregate function `#{unquote(function)}` can not be used in the `GROUP BY` clause."
    end
  end

  for function <- ~w(year month day hour minute second weekday) do
    test "allowing #{function} on datetime columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(column) from table", data_source())
    end

    test "allowing #{function} in group by" do
      assert {:ok, _} = compile(
        "select #{unquote(function)}(column) from table group by #{unquote(function)}(column)",
        data_source
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

  for function <- ~w(year month day weekday) do
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
    assert error ==
      "Columns (`numeric`, `numeric`) need to appear in the `GROUP BY` clause or be used in an aggregate function."
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
      data_source
    )

    assert {:ok, _} = compile(
      "SELECT t1.c1 from t1, t2, t3 WHERE t3.uid = t1.uid AND t3.uid = t2.uid",
      data_source
    )

    assert {:ok, _} = compile(
      "SELECT t1.c1 from t1, t2, t3, t4 WHERE t3.uid = t1.uid AND t3.uid = t2.uid AND t1.uid = t4.uid",
      data_source
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

  test "rejecting improper joins" do
    assert {:error, error} = compile("SELECT t1.c1 from t1, t2", data_source())
    assert error =~ ~r/Missing where comparison.*`t1` and `t2`/

    assert {:error, error} = compile("SELECT t1.c1 from t1, t2, t3 WHERE t1.uid = t2.uid", data_source())
    assert error =~ ~r/Missing where comparison.*`t2` and `t3`/

    assert {:error, error} = compile(
      "SELECT t1.c1 from t1, t2, t3, t4 WHERE t1.uid = t2.uid AND t3.uid = t4.uid",
      data_source
    )
    assert error =~ ~r/Missing where comparison.*`t2` and `t3`/
  end

  Enum.each(["count", "min", "max", "median", "stddev"], fn(function) ->
    test "allows qualified identifiers in function calls (function #{function})" do
      assert %{columns: [{:function, unquote(function), [column("table", "numeric")]}]} =
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
      data_source)
    assert [column("table", "column"), {:function, "count", [column("table", "column")]}] = result.columns
    assert Enum.any?(result.where, &match?({:comparison, column("table", "numeric"), :>=, _}, &1))
    assert Enum.any?(result.where, &match?({:comparison, column("table", "numeric"), :<, _}, &1))
    assert Enum.any?(result.where, &match?({:not, {:is, column("table", "column"), :null}}, &1))
    assert [{:not, {:comparison, column("table", "column"), :=, _}}] = result.lcf_check_conditions
    assert [column("table", "column")] = result.unsafe_filter_columns
    assert [column("table", "column")] = result.group_by
    assert result.order_by == [{1, :desc}, {1, :desc}]
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
      data_source)
    assert [column("t1", "c1")] = result.columns
    assert Enum.any?(result.where, &match?({:comparison, column("t1", "c2"), :>=, _}, &1))
    assert Enum.any?(result.where, &match?({:comparison, column("t1", "c2"), :<, _}, &1))
    assert Enum.any?(result.where, &match?({:comparison, column("t1", "uid"), :=, column("t2", "uid")}, &1))
    assert [column("t1", "c1"), column("t2", "c3")] = result.group_by
    assert result.order_by == [{0, :desc}]
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
           INNER JOIN t3 ON t3.uid = t1.uid AND t1.c2 > 10
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

  test "negative condition in subquery is not supported" do
    assert {:error, error} = compile("select c1 from (select uid, c1 from t1 where c1 <> 100) alias", data_source())
    assert error =~ "<> is not supported in a subquery."
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
      <> "2015-01-01 00:00:00.000000 <= `column` from table `table` < 2017-01-01 00:00:00.000000."]
  end

  test "no message when datetime alignment does not require fixing" do
    assert compile!("select * from table where column >= '2014-01-01' and column < '2016-01-01'", data_source()).
      info == []
  end

  test "fixes alignment of date ranges" do
    aligned = compile!("select * from table where column > '2015-01-02' and column < '2016-07-01'", date_data_source())
    assert compile!("select * from table where column > '2015-01-01' and column < '2016-08-02'", date_data_source()).
      where == aligned.where
    assert aligned.info == ["The range for column `column` from table `table` has been adjusted to 2015-01-01 <= "
      <> "`column` from table `table` < 2017-01-01."]
  end

  test "fixes alignment of time ranges" do
    aligned = compile!("select * from table where column > '00:00:01' and column < '00:00:04'", time_data_source())
    assert compile!("select * from table where column >= '00:00:00' and column < '00:00:05'", time_data_source()).
      where == aligned.where
    assert aligned.
      info == ["The range for column `column` from table `table` has been adjusted to 00:00:00.000000 <= "
        <> "`column` from table `table` < 00:00:05.000000."]
  end

  test "no message when time alignment does not require fixing" do
    assert compile!("select * from table where column >= '00:00:00' and column < '00:00:05'", time_data_source()).
      info == []
  end

  test "includes an info message when the aligment is fixed" do
    assert [msg] = compile!("select count(*) from table where numeric >= 0.1 and numeric < 1.9", data_source()).info
    assert msg == "The range for column `numeric` from table `table` has been adjusted to 0.0 <= `numeric` from table "
      <> "`table` < 2.0."
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
    assert compile("select count(*) from table
      where numeric >= 1 and numeric > 0.9 and numeric < 2 and numeric <= 2.1", data_source()) ==
      compile("select count(*) from table where numeric >= 1 and numeric < 2", data_source())
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

  test "propagating ranges for shrink and drop from a singly-nested having" do
    query = compile!("""
      select * from (
        select uid from table group by uid
        having avg(numeric) >= 0.0 and avg(numeric) < 100.0
      ) x
    """, data_source())
    {:subquery, %{ast: subquery}} = query.from

    assert [%{alias: column_alias}] = Enum.reject(subquery.db_columns, &(&1.name == "uid"))
    assert [%{column: %{name: ^column_alias}, interval: {0.0, 100.0}}] = query.ranges
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

    assert [
      %{function: "min", alias: min_alias},
      %{function: "max", alias: max_alias}
    ] = Enum.reject(subquery.db_columns, &(&1.name == "uid"))
    assert Enum.any?(query.ranges, &match?(%{column: %{name: ^min_alias}, interval: {0.0, 100.0}}, &1))
    assert Enum.any?(query.ranges, &match?(%{column: %{name: ^max_alias}, interval: {0.0, 100.0}}, &1))
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

  describe "rejects queries selecing columns that have seen math, discontinuity and constants" do
    Enum.each(~w(+ - / * ^), fn(math_function) ->
      Enum.each(~w(abs ceil floor round trunc sqrt), fn(discontinuous_function) ->
        test "when on the same level ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(numeric #{unquote(math_function)} 3)
            FROM table
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "when across queries ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(num)
            FROM (
              SELECT uid, numeric #{unquote(math_function)} 3 as num
              FROM table
            ) t
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "ok when no constant ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(numeric #{unquote(math_function)} numeric)
            FROM table
          """
          assert select_columns_have_valid_transformations(query)
        end

        test "ok when math is only with constants ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(1 #{unquote(math_function)} 3)
            FROM table
          """
          assert select_columns_have_valid_transformations(query)
        end
      end)

      Enum.each(~w(div mod), fn(discontinuous_function) ->
        test "when on the same level ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(numeric, 2) #{unquote(math_function)} 3
            FROM table
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "when across queries ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT num #{unquote(math_function)} 3
            FROM (
              SELECT uid, #{unquote(discontinuous_function)}(numeric, 2) as num
              FROM table
            ) t
          """
          refute select_columns_have_valid_transformations(query)
        end

        test "ok when math is only with constants ('#{math_function}' and '#{discontinuous_function}')" do
          query = """
            SELECT #{unquote(discontinuous_function)}(1, 2) #{unquote(math_function)} 3
            FROM table
          """
          assert select_columns_have_valid_transformations(query)
        end
      end)

      test "when on the same level ('#{math_function}' and 'bucket')" do
        query = """
          SELECT bucket(numeric by 10) #{unquote(math_function)} 3
          FROM table
        """
        refute select_columns_have_valid_transformations(query)
      end

      test "when across queries ('#{math_function}' and 'bucket')" do
        query = """
          SELECT num #{unquote(math_function)} 3
          FROM (
            SELECT uid, bucket(numeric by 10) as num
            FROM table
          ) t
        """
        refute select_columns_have_valid_transformations(query)
      end

      test "when on the same level ('#{math_function}' and '%')" do
        query = """
          SELECT (numeric % 7) #{unquote(math_function)} 3
          FROM table
        """
        refute select_columns_have_valid_transformations(query)
      end

      test "when across queries ('#{math_function}' and '%')" do
        query = """
          SELECT num #{unquote(math_function)} 3
          FROM (
            SELECT uid, numeric % 7 as num
            FROM table
          ) t
        """
        refute select_columns_have_valid_transformations(query)
      end
    end)
  end

  test "/ becomes a dangerous discontinuous function if divisor is touched by a constant" do
    query = "SELECT numeric / (numeric * 2) FROM table"
    refute select_columns_have_valid_transformations(query)
  end

  describe "casts are considered dangerously discontinuous when a constant is involved" do
    Enum.each(~w(integer real boolean), fn(cast_target) ->
      test "cast from integer to #{cast_target}" do
        query = "SELECT cast(numeric + 1 as #{unquote(cast_target)}) FROM table"
        refute select_columns_have_valid_transformations(query)
      end
    end)

    Enum.each(~w(datetime date), fn(cast_target) ->
      test "cast from text to #{cast_target}" do
        query = """
        SELECT string FROM (
          SELECT uid, string, cast(rtrim(string, 'constant') as #{unquote(cast_target)}) as danger
          FROM table
        ) t
        WHERE danger >= '2010-01-01' and danger < '2020-01-01'
        """
        refute where_columns_have_valid_transformations(query)
      end
    end)

    test "cast from text to time" do
      query = """
      SELECT string FROM (
        SELECT uid, string, cast(rtrim(string, 'constant') as time) as danger
        FROM table
      ) t
      WHERE danger >= '10:10:10' and danger < '12:12:12'
      """
      refute where_columns_have_valid_transformations(query)
    end
  end

  describe "WHERE-inequalities affected by dangerous math OR discontinuity are forbidden" do
    Enum.each(~w(abs ceil floor round trunc sqrt), fn(discontinuous_function) ->
      test "#{discontinuous_function} without constant" do
        query = """
          SELECT value FROM (
            SELECT uid, #{unquote(discontinuous_function)}(numeric) as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        assert where_columns_have_valid_transformations(query)
      end
    end)

    Enum.each(~w(div mod), fn(discontinuous_function) ->
      test "#{discontinuous_function} with constant" do
        query = """
          SELECT numeric FROM (
            SELECT
              uid,
              numeric,
              #{unquote(discontinuous_function)}(numeric, 3) as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        refute where_columns_have_valid_transformations(query)
      end

      test "#{discontinuous_function} without constant" do
        query = """
          SELECT value FROM (
            SELECT uid, #{unquote(discontinuous_function)}(numeric, numeric) as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        assert where_columns_have_valid_transformations(query)
      end
    end)

    Enum.each(~w(+ - / * ^), fn(math_function) ->
      test "#{math_function} without constant" do
        query = """
          SELECT value FROM (
            SELECT uid, numeric #{unquote(math_function)} numeric as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        assert where_columns_have_valid_transformations(query)
      end
    end)

    # Note: we can't test with / because it is treated as a dangerous discontinuous
    # function as soon as a constant is involved, and hence halts query compilation.
    Enum.each(~w(+ - * ^), fn(math_function) ->
      test "#{math_function} with constant" do
        query = """
          SELECT value FROM (
            SELECT uid, numeric #{unquote(math_function)} 3 as value
            FROM table
          ) t
          WHERE value >= 10 and value < 20
        """
        refute where_columns_have_valid_transformations(query)
      end
    end)

    test "string and constant converted to number" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            length(btrim(string, 'constant')) as value
          FROM table
        ) t
        WHERE value >= 0 and value < 10
      """
      refute where_columns_have_valid_transformations(query)
    end

    test "dangerous cast column" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            cast(btrim(string, 'constant') as integer) as value
          FROM table
        ) t
        WHERE value >= 0 and value < 10
      """
      refute where_columns_have_valid_transformations(query)
    end
  end

  describe "WHERE-equalities on dangerous columns are allowed" do
    test "unsafe discontinuity" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            length(btrim(string, 'constant')) as value
          FROM table
        ) t
        WHERE value = 0
      """
      assert where_columns_have_valid_transformations(query)
    end

    test "on a cast column" do
      query = """
        SELECT numeric FROM (
          SELECT
            uid,
            numeric,
            cast(string as integer) as value
          FROM table
        ) t
        WHERE value = 10
      """
      assert where_columns_have_valid_transformations(query)
    end

    test "affected by math" do
      query = """
        SELECT value FROM (
          SELECT uid, numeric + 2 as value
          FROM table
        ) t
        WHERE value = 10
      """
      assert where_columns_have_valid_transformations(query)
    end
  end

  defp scrub_aliases(query), do: put_in(query, [aliases()], nil)

  deflens aliases, do:
    all_subqueries() |> Query.Lenses.terminals() |> Lens.satisfy(&match?(%Column{}, &1)) |> Lens.key(:alias)

  deflens all_subqueries(), do:
    Lens.both(Lens.recur(Query.Lenses.direct_subqueries() |> Lens.key(:ast)), Lens.root())

  defp select_columns_have_valid_transformations(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      {:error, reason} ->
        if reason =~ ~r/is influenced by math, a discontinuous function/ do
          false
        else
          raise "Compilation failed with other reason than illegal math/discontinuity: #{inspect reason}"
        end
    end
  end

  defp where_columns_have_valid_transformations(query) do
    case compile(query, data_source()) do
      {:ok, _} -> true
      {:error, reason} ->
        if reason =~ ~r/WHERE-clause inequalities/ do
          false
        else
          raise "Compilation failed with other reason than illegal WHERE-clause: #{inspect reason}"
        end
    end
  end

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
end
