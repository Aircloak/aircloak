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

  test "casts datetime where conditions" do
    result = compile!("select * from table where column > '2015-01-01'", data_source())

    assert [{:comparison, column("table", "column"), :>, ~N[2015-01-01 00:00:00.000000]}] = result.where
  end

  test "casts time where conditions" do
    assert %{where: [{:comparison, column("table", "column"), :>, ~T[01:02:03.000000]}]} =
      compile!("select * from table where column > '01:02:03'", time_data_source())
  end

  test "casts date where conditions" do
    assert %{where: [{:comparison, column("table", "column"), :>, ~D[2015-01-02]}]} =
      compile!("select * from table where column > '2015-01-02'", date_data_source())
  end

  test "casts datetime in `in` conditions" do
    result = compile!("select * from table where column in ('2015-01-01', '2015-01-02')", data_source())

    assert [{:in, column("table", "column"), times}] = result.lcf_check_conditions
    assert Enum.sort(times) == [~N[2015-01-01 00:00:00.000000], ~N[2015-01-02 00:00:00.000000]]
  end

  test "casts datetime in negated conditions" do
    result = compile!("select * from table where column <> '2015-01-01'", data_source())

    assert [{:not, {:comparison, column("table", "column"), :=, ~N[2015-01-01 00:00:00.000000]}}] =
      result.lcf_check_conditions
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

  test "rejecting outer where clause in queries unchecked sub-select" do
    assert {:error, "WHERE-clause in outer SELECT is not allowed in combination with a subquery."} =
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
        WHERE column > '2015-01-01' and column <> '2015-01-02'
        GROUP BY column
        ORDER BY count(column) DESC, count(table.column) DESC
      """,
      data_source)
    assert [column("table", "column"), {:function, "count", [column("table", "column")]}] = result.columns
    assert [{:comparison, column("table", "column"), :>, _}] = result.where
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
    assert [comparison1, comparison2, comparison3] = result.where
    assert {:comparison, column("t1", "c2"), :>=, _} = comparison1
    assert {:comparison, column("t1", "c2"), :<, _} = comparison2
    assert {:comparison, column("t1", "uid"), :=, column("t2", "uid")} = comparison3
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
    assert error == "Column `numeric` must be limited to a finite range."
  end

  test "rejects inequalities on numeric columns that are negatives of ranges" do
    assert {:error, error} = compile("select * from table where numeric < 2 and numeric > 5", data_source())
    assert error == "Column `numeric` must be limited to a finite range."
  end

  test "accepts inequalities on numeric columns that are ranges" do
    assert {:ok, _} = compile("select * from table where numeric > 5 and numeric < 8", data_source())
  end

  test "fixes alignment of ranges" do
    assert compile!("select * from table where numeric > 1 and numeric < 9", data_source()).where
      == compile!("select * from table where numeric > 0 and numeric < 10", data_source()).where
  end

  test "includes an info message when the aligment is fixed" do
    assert [msg] = compile!("select count(*) from table where numeric >= 0.1 and numeric < 1.9", data_source()).info
    assert msg == "The range for column `numeric` has been adjusted to 0.0 <= `numeric` < 2.0"
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
    first = compile!("select bucket(numeric by 0.11) from table", data_source())
    second = compile!("select bucket(numeric by 0.1) from table", data_source())

    assert Map.drop(first, [:info]) == Map.drop(second, [:info])
    assert ["Bucket size adjusted from 0.11 to 0.1"] = first.info
    assert [] = second.info
  end

  test "negative and 0 bucket sizes are not allowed" do
    assert {:error, _} = compile("select bucket(numeric by 0) from table", data_source())
    assert {:error, error} = compile("select bucket(numeric by -10) from table", data_source())
    assert error =~ ~r/Bucket size -10 must be > 0/
  end

  test "math can be disabled with a config setting" do
    assert {:error, error} = compile("select numeric * 2 from table", data_source(), %{math: false})
    assert error =~ ~r/Unknown function `*`/
  end

  defp compile!(query_string, data_source) do
    {:ok, result} = compile(query_string, data_source)
    result
  end

  defp compile(query_string, data_source, features \\ Cloak.Features.from_config) do
    query = Parser.parse!(data_source, query_string)
    Compiler.compile(data_source, query, features)
  end

  defp data_source(driver \\ Cloak.DataSource.PostgreSQL) do
    %{driver: driver, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [
          {"uid", :integer}, {"column", :datetime}, {"numeric", :integer}, {"float", :real}, {"string", :text}
        ]
      },
      other_table: %{
        db_name: "other_table",
        name: "other_table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"other_column", :datetime}]
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

  def time_data_source do
    %{driver: Cloak.DataSource.PostgreSQL, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"column", :time}]
      }
    }}
  end

  def date_data_source do
    %{driver: Cloak.DataSource.PostgreSQL, tables: %{
      table: %{
        db_name: "table",
        name: "table",
        user_id: "uid",
        columns: [{"uid", :integer}, {"column", :date}]
      }
    }}
  end
end
