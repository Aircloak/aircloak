defmodule Cloak.Sql.Compiler.Optimizer.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  test "unused columns selected in subquery are dropped" do
    assert %{from: {:subquery, %{ast: subquery}}} =
             compile!("SELECT COUNT(x.numeric) FROM (SELECT * FROM table) x", data_source())

    assert ["uid", "numeric"] = subquery.column_titles
  end

  test "unused columns selected in subquery with join are dropped" do
    assert %{from: {:subquery, %{ast: subquery}}} =
             compile!(
               """
                 SELECT min(n) FROM (
                   SELECT 1 AS a, t1.numeric AS n, t1.uid, t1.string, t2.* FROM
                   table AS t1 JOIN table AS t2 ON t1.uid = t2.uid
                 ) AS x
               """,
               data_source()
             )

    assert ["n", "uid", "uid"] = subquery.column_titles
  end

  test "grouped by columns selected in subquery are kept" do
    assert %{from: {:subquery, %{ast: subquery}}} =
             compile!(
               "SELECT COUNT(*) FROM (SELECT * FROM table) x GROUP BY numeric",
               data_source()
             )

    assert ["uid", "numeric"] = subquery.column_titles
  end

  test "unused columns selected in joined subquery are dropped" do
    assert %{from: {:join, %{lhs: {:subquery, %{ast: subquery}}}}} =
             compile!(
               """
                 SELECT avg(t1.numeric) FROM (SELECT * FROM table) AS t1 JOIN table AS t2 ON t1.uid = t2.uid
               """,
               data_source()
             )

    assert ["uid", "numeric"] = subquery.column_titles
  end

  test "simple conditions in joined subquery are pushed down" do
    assert %{from: {:join, %{lhs: {:subquery, %{ast: subquery}}, conditions: conditions}}} =
             compile!(
               """
                 SELECT count(*) FROM
                   (SELECT uid, numeric FROM table) AS t1
                   JOIN table AS t2
                   ON t1.uid = t2.uid AND t1.numeric BETWEEN 0 AND 100
               """,
               data_source()
             )

    assert {:and, _, _} = subquery.where
    assert {:comparison, _, :=, _} = conditions
  end

  test "simple conditions in complex joined subquery are pushed down" do
    assert %{from: from} =
             compile!(
               """
                 SELECT count(*) FROM
                   (SELECT uid, numeric FROM table) AS t1
                   JOIN table AS t2 ON t1.uid = t2.uid
                   LEFT JOIN table AS t3
                   ON t2.uid = t3.uid AND t1.numeric <> 0
               """,
               data_source()
             )

    assert {:join, %{lhs: lhs_branch, conditions: {:comparison, _, :=, _}}} = from
    assert {:join, %{lhs: {:subquery, %{ast: subquery}}}} = lhs_branch
    assert {:comparison, %{name: "numeric"}, :<>, %{value: 0}} = subquery.where
  end

  test "simple conditions in upper query are pushed down" do
    assert %{from: {:subquery, %{ast: subquery}}, where: where} =
             compile!(
               """
                 SELECT count(*) FROM
                   (SELECT uid, numeric FROM table) AS t
                 WHERE numeric = 0
               """,
               data_source()
             )

    assert {:not, {:is, %{name: "uid"}, :null}} = where
    assert {:comparison, %{name: "numeric"}, :=, %{value: 0}} = subquery.where
  end

  test "[BUG] pushing a range with negative number into subquery" do
    assert %{from: {:subquery, %{ast: subquery}}, where: where} =
             compile!(
               """
                 SELECT count(*) FROM
                 (SELECT uid, numeric FROM table) AS t
                 WHERE numeric BETWEEN -5 AND 0
               """,
               data_source()
             )

    assert {:not, {:is, %{name: "uid"}, :null}} = where

    assert {:and, {:comparison, %{name: "numeric"}, :>=, %{value: -5.0}},
            {:comparison, %{name: "numeric"}, :<, %{value: 0.0}}} = subquery.where
  end

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            columns: [
              Table.column("uid", :integer),
              Table.column("numeric", :integer),
              Table.column("string", :text)
            ]
          )
      }
    }
  end
end
