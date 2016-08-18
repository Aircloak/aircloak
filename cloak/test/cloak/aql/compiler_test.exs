defmodule Cloak.Aql.Compiler.Test do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Compiler
  alias Cloak.Aql.Parser

  defmacrop column(table_name, column_name) do
    quote do
      %{name: unquote(column_name), table: %{db_name: unquote(table_name)}}
    end
  end

  test "adds an empty group by" do
    assert %{group_by: []} = compile!("select * from table", data_source())
  end

  test "casts timestamp where conditions" do
    result = compile!("select * from table where column > '2015-01-01'", data_source())

    time = %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}
    assert [{:comparison, column("table", "column"), :>, ^time}] = result.where
  end

  test "casts timestamp in `in` conditions" do
    result = compile!("select * from table where column in ('2015-01-01', '2015-01-02')", data_source())

    assert [{:in, column("table", "column"), times}] = result.where
    assert Enum.sort(times) == [
      %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)},
      %Timex.DateTime{year: 2015, month: 1, day: 2, timezone: Timex.Timezone.get(:utc)},
    ]
  end

  test "casts timestamp in negated conditions" do
    result = compile!("select * from table where column <> '2015-01-01'", data_source())

    time = %Timex.DateTime{year: 2015, month: 1, day: 1, timezone: Timex.Timezone.get(:utc)}
    assert [{:comparison, column("table", "column"), :=, ^time}] = result.where_not
  end

  test "reports malformed timestamps" do
    assert {:error, "Cannot cast `something stupid` to timestamp."} =
      compile("select * from table where column > 'something stupid'", data_source())
  end

  for function <- ~w(min max sum median) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "rejecting #{function} on non-numerical columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer`) or (`real`), but got (`timestamp`)"
    end
  end

  for function <- ~w(avg stddev abs sqrt) do
    test "allowing #{function} on numeric columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(numeric) from table", data_source())
    end

    test "rejecting #{function} on non-numerical columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer` | `real`), but got (`timestamp`)"
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
      assert error == "Aggregate function `#{unquote(function)}` used in the group by clause"
    end
  end

  for function <- ~w(year month day hour minute second weekday) do
    test "allowing #{function} on timestamp columns" do
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
    test "rejecting #{function} on non-timestamp columns" do
      assert {:error, error} = compile("select #{unquote(function)}(numeric) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`timestamp` | `time`), but got (`integer`)"
    end
  end

  for function <- ~w(year month day weekday) do
    test "rejecting #{function} on non-timestamp columns" do
      assert {:error, error} = compile("select #{unquote(function)}(numeric) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`timestamp` | `date`), but got (`integer`)"
    end
  end

  for function <- ~w(floor ceil ceiling) do
    test "allowing #{function} on real columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(float) from table", data_source())
    end

    test "rejecting #{function} on non-numeric columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error ==
        "Function `#{unquote(function)}` requires arguments of type (`integer` | `real`), but got (`timestamp`)"
    end
  end

  for function <- ~w(trunc round) do
    test "allowing #{function} on real columns" do
      assert {:ok, _} = compile("select #{unquote(function)}(float) from table", data_source())
    end

    test "rejecting #{function} on non-numeric columns" do
      assert {:error, error} = compile("select #{unquote(function)}(column) from table", data_source())
      assert error == "Function `#{unquote(function)}` requires arguments of type"
       <> " (`integer` | `real`) or (`integer` | `real`, `integer`), but got (`timestamp`)"
    end
  end

  test "multiarg function argument verification" do
    assert {:error, error} = compile("select div(numeric, column) from table", data_source())
    assert error ==
      "Function `div` requires arguments of type (`integer`, `integer`), but got (`integer`, `timestamp`)"
  end

  test "rejecting a function with too many arguments" do
    assert {:error, error} = compile("select avg(numeric, column) from table", data_source())
    assert error ==
      "Function `avg` requires arguments of type (`integer` | `real`), but got (`integer`, `timestamp`)"
  end

  test "rejecting a function with too few arguments" do
    assert {:error, error} = compile("select div(numeric) from table", data_source())
    assert error == "Function `div` requires arguments of type (`integer`, `integer`), but got (`integer`)"
  end

  test "rejecting a column in select when its function is grouped" do
    assert {:error, error} = compile("select column from table group by day(column)", data_source())
    assert error ==
      "Column `column` from table `table` needs to appear in the `group by` clause" <>
      " or be used in an aggregate function."
  end

  test "rejecting a function in select when another function is grouped" do
    assert {:error, error} = compile("select div(numeric, numeric) from table group by abs(numeric)", data_source())
    assert error ==
      "Columns (`numeric`, `numeric`) need to appear in the `group by` clause or be used in an aggregate function."
  end

  test "rejecting concat on non-strings" do
    assert {:error, error} = compile("select concat(numeric) from table", data_source())
    assert error == "Function `concat` requires arguments of type ([`text`]+), but got (`integer`)"
  end

  test "rejecting ill-typed nested function calls" do
    assert {:error, error} = compile("select concat(avg(numeric)) from table", data_source())
    assert error == "Function `concat` requires arguments of type ([`text`]+), but got (`real`)"
  end

  test "typechecking nested function calls recursively" do
    assert {:error, error} = compile("select sqrt(abs(avg(column))) from table", data_source())
    assert error == "Function `avg` requires arguments of type (`integer` | `real`), but got (`timestamp`)"
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

  test "rejecting outer where clause in queries unchecked sub-select" do
    assert {:error, "WHERE-clause in outer SELECT is not allowed in combination with a subquery"} =
      compile("SELECT a FROM (unchecked inner select) t WHERE a > 10", data_source(Cloak.DataSource.DsProxy))
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
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT other_table.other_column FROM table", data_source())
  end

  test "rejecting qualified SELECT from not selected table when join" do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT other_table.other_column FROM t1, t2", data_source())
  end

  test "rejecting qualified ORDER BY from not selected table" do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT column FROM table ORDER BY other_table.other_column", data_source())
  end

  test "rejecting qualified GROUP BY from not selected table" do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
      compile("SELECT column FROM table GROUP BY other_table.other_column", data_source())
  end

  test "rejecting qualified WHERE from not selected table" do
    assert {:error, "Missing FROM clause entry for table `other_table`"} =
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
        WHERE column > '2015-01-01' and column <> '2015-01-02'
        GROUP BY column
        ORDER BY count(column) DESC, count(table.column) DESC
      """,
      data_source)
    assert [column("table", "column"), {:function, "count", [column("table", "column")]}] = result.columns
    assert [{:comparison, column("table", "column"), :>, _}] = result.where
    assert [{:comparison, column("table", "column"), :=, _}] = result.where_not
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
      column("t1", "uid"),
      column("t1", "c1"),
      column("t1", "c2"),
      column("t2", "uid"),
      column("t2", "c1"),
      column("t2", "c3"),
      column("t3", "uid"),
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
        WHERE c2 > 10 AND t1.uid = t2.uid
        GROUP BY t1.c1, c3
        ORDER BY t1.c1 DESC
      """,
      data_source)
    assert [column("t1", "c1")] = result.columns
    assert [comparison1, comparison2] = result.where
    assert {:comparison, column("t1", "c2"), :>, _} = comparison1
    assert {:comparison, column("t1", "uid"), :=, column("t2", "uid")} = comparison2
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
    assert error == "Cannot cast value of type `timestamp` to type `integer`."
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

  test "group by in subquery is not supported" do
    assert {:error, error} = compile("select c1 from (select uid, avg(c1) from t1 group by uid) alias", data_source())
    assert error =~ "`GROUP BY` is not supported in a subquery."
  end

  test "integer operations are valid on sums of integer columns" do
    assert {:ok, _} = compile("select sum(numeric) % 3 from table", data_source())
  end

  test "integer operations are invalid on sums of real columns" do
    assert {:error, error} = compile("select sum(float) % 3 from table", data_source())
    assert error ==
        "Function `%` requires arguments of type (`integer`, `integer`), but got (`real`, `integer`)"
  end

  defp compile!(query_string, data_source) do
    {:ok, result} = compile(query_string, data_source)
    result
  end

  defp compile(query_string, data_source) do
    query = Parser.parse!(data_source, query_string)
    Compiler.compile(data_source, query)
  end

  defp data_source(driver \\ Cloak.DataSource.PostgreSQL) do
    %{driver: driver, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"column", :timestamp}, {"numeric", :integer}, {"float", :real}]
      },
      other_table: %{
        db_name: "other_table",
        name: "other_table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"other_column", :timestamp}]
      },
      t1: %{
        db_name: "t1",
        name: "t1",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}, {"c2", :integer}]
      },
      t2: %{
        db_name: "t2",
        name: "t2",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}, {"c3", :integer}]
      },
      t3: %{
        db_name: "t3",
        name: "t3",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}]
      },
      t4: %{
        db_name: "t4",
        name: "t4",
        user_id: "uid",
        columns: [{"uid", :integer}, {"c1", :integer}]
      }
    }}
  end
end
