defmodule Cloak.Sql.TransformerTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.{Table, SqlBuilder}
  alias Cloak.Sql.{Compiler, Parser, Query.Lenses}
  alias Cloak.Sql.Compiler.Anonymization.Transformer

  defp data_source() do
    %{
      name: "transformer_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("col1", :integer),
              Table.column("col2", :integer)
            ]
          )
      }
    }
  end

  defp compile(statement),
    do: statement |> Parser.parse!() |> Compiler.core_compile!(nil, data_source(), [], %{})

  defp query_to_string(query) do
    query
    |> update_in([Lenses.all_queries()], fn query ->
      columns =
        query.columns
        |> Enum.zip(query.column_titles)
        |> Enum.map(fn {column, title} -> %{column | alias: title} end)

      %{query | subquery?: true, db_columns: columns}
    end)
    |> SqlBuilder.build()
    |> String.replace("\"", "")
    |> String.replace("__ac_", "")
    |> String.replace(", ", ",")
  end

  defp transform(statement, transformer), do: statement |> compile() |> transformer.() |> query_to_string()

  defp flatten(statement) do
    statement
    |> String.replace("\n", " ")
    |> String.replace(~r/\s+/, " ")
    |> String.replace(", ", ",")
    |> String.replace("( ", "(")
    |> String.replace(" )", ")")
    |> String.trim()
  end

  describe "group by uid" do
    test "select column" do
      assert transform("select col1 from table", &Transformer.group_by_uid/1) ==
               flatten("""
               SELECT
                uid_grouping.group_0 AS col1
               FROM (
                SELECT
                  table.uid AS uid,
                  table.col1 AS group_0,
                  COUNT(*) AS agg_0
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid, table.col1
               ) AS uid_grouping
               """)
    end

    test "select count(*)" do
      assert transform("select count(*) from table", &Transformer.group_by_uid/1) ==
               flatten("""
               SELECT 
                SUM(uid_grouping.agg_0) AS count
               FROM (
                SELECT
                  table.uid AS uid,
                  COUNT(*) AS agg_0
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid
               ) AS uid_grouping
               """)
    end

    test "select count(column)" do
      assert transform("select count(col1) from table", &Transformer.group_by_uid/1) ==
               flatten("""
               SELECT
                SUM(uid_grouping.agg_0) AS count
               FROM (
                SELECT
                  table.uid AS uid,
                  COUNT(table.col1) AS agg_0
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid
               ) AS uid_grouping
               """)
    end

    test "where filter" do
      assert transform("select count(*) from table where col1 = 0", &Transformer.group_by_uid/1) ==
               flatten("""
               SELECT
                SUM(uid_grouping.agg_0) AS count
               FROM (
                SELECT
                  table.uid AS uid,
                  COUNT(*) AS agg_0
                FROM table
                WHERE (table.uid IS NOT NULL AND (table.col1 = 0))
                GROUP BY table.uid
               ) AS uid_grouping
               """)
    end

    test "group by" do
      assert transform("select col1 from table group by col1", &Transformer.group_by_uid/1) ==
               flatten("""
               SELECT
                uid_grouping.group_0 AS col1
               FROM (
                SELECT
                  table.uid AS uid,
                  table.col1 AS group_0
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid, table.col1
               ) AS uid_grouping
               GROUP BY uid_grouping.group_0
               """)
    end

    test "group by with sum" do
      assert transform("select col1, sum(col2) from table group by col1", &Transformer.group_by_uid/1) ==
               flatten("""
               SELECT
                uid_grouping.group_0 AS col1,
                SUM(uid_grouping.agg_0) AS sum
               FROM (
                SELECT
                  table.uid AS uid,
                  table.col1 AS group_0,
                  SUM(table.col2) AS agg_0
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid, table.col1
               ) AS uid_grouping
               GROUP BY uid_grouping.group_0
               """)
    end

    test "group by with having" do
      assert transform("select col1 from table group by col1 having sum(col2) = 0", &Transformer.group_by_uid/1) ==
               flatten("""
               SELECT
                uid_grouping.group_0 AS col1
               FROM (
                SELECT
                  table.uid AS uid,
                  table.col1 AS group_0,
                  SUM(table.col2) AS agg_0
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid, table.col1
               ) AS uid_grouping
               GROUP BY uid_grouping.group_0
               HAVING (SUM(uid_grouping.agg_0) = 0)
               """)
    end

    test "offload grouping sets" do
      assert transform(
               "select count(*) from table group by grouping sets (col1, col2)",
               &(&1 |> Transformer.group_by_uid() |> Transformer.offload_grouping_sets())
             ) ==
               flatten("""
               SELECT
                SUM(uid_grouping.agg_0) AS count
               FROM (
               SELECT
                table.uid AS uid,
                table.col1 AS group_0,
                table.col2 AS group_1,
                COUNT(*) AS agg_0,
                GROUPING(table.col1, table.col2) AS grouping_id
               FROM table
               WHERE table.uid IS NOT NULL
               GROUP BY GROUPING SETS ((table.uid, table.col1), (table.uid, table.col2))
               ) AS uid_grouping
               GROUP BY GROUPING SETS ((uid_grouping.group_0), (uid_grouping.group_1))
               """)
    end
  end

  describe "main statistics" do
    test "simple select" do
      assert transform(
               "select col1 from table",
               &(&1
                 |> Transformer.group_by_uid()
                 |> Transformer.offload_grouping_sets()
                 |> Transformer.compute_main_statistics())
             ) ==
               flatten("""
               SELECT
                uid_grouping.grouping_id AS grouping_id,
                uid_grouping.group_0 AS group_0,
                COUNT(uid_grouping.uid) AS count_duid,
                MIN(uid_grouping.uid) AS min_uid,
                MAX(uid_grouping.uid) AS max_uid,
                COUNT(uid_grouping.agg_0) AS agg_0_count,
                SUM(uid_grouping.agg_0) AS agg_0_sum,
                MIN(uid_grouping.agg_0) AS agg_0_min,
                MAX(uid_grouping.agg_0) AS agg_0_max,
                STDDEV(uid_grouping.agg_0) AS agg_0_stddev
               FROM (
                SELECT
                  table.uid AS uid,
                  table.col1 AS group_0,
                  COUNT(*) AS agg_0,
                  0 AS grouping_id
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid, table.col1
               ) AS uid_grouping
               GROUP BY uid_grouping.grouping_id, uid_grouping.group_0
               """)
    end

    test "global aggregator" do
      assert transform(
               "select sum(col1) from table",
               &(&1
                 |> Transformer.group_by_uid()
                 |> Transformer.offload_grouping_sets()
                 |> Transformer.compute_main_statistics())
             ) ==
               flatten("""
               SELECT
                uid_grouping.grouping_id AS grouping_id,
                COUNT(uid_grouping.uid) AS count_duid,
                MIN(uid_grouping.uid) AS min_uid,
                MAX(uid_grouping.uid) AS max_uid,
                COUNT(uid_grouping.agg_0) AS agg_0_count,
                SUM(uid_grouping.agg_0) AS agg_0_sum,
                MIN(uid_grouping.agg_0) AS agg_0_min,
                MAX(uid_grouping.agg_0) AS agg_0_max,
                STDDEV(uid_grouping.agg_0) AS agg_0_stddev
               FROM (
                SELECT
                  table.uid AS uid,
                  SUM(table.col1) AS agg_0,
                  0 AS grouping_id
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid
               ) AS uid_grouping
               GROUP BY uid_grouping.grouping_id
               """)
    end

    test "grouping" do
      assert transform(
               "select count(col1) from table group by col2",
               &(&1
                 |> Transformer.group_by_uid()
                 |> Transformer.offload_grouping_sets()
                 |> Transformer.compute_main_statistics())
             ) ==
               flatten("""
               SELECT
                uid_grouping.grouping_id AS grouping_id,
                uid_grouping.group_0 AS group_0,
                COUNT(uid_grouping.uid) AS count_duid,
                MIN(uid_grouping.uid) AS min_uid,
                MAX(uid_grouping.uid) AS max_uid,
                COUNT(uid_grouping.agg_0) AS agg_0_count,
                SUM(uid_grouping.agg_0) AS agg_0_sum,
                MIN(uid_grouping.agg_0) AS agg_0_min,
                MAX(uid_grouping.agg_0) AS agg_0_max,
                STDDEV(uid_grouping.agg_0) AS agg_0_stddev
               FROM (
                SELECT
                  table.uid AS uid,
                  table.col2 AS group_0,
                  COUNT(table.col1) AS agg_0,
                  0 AS grouping_id
                FROM table
                WHERE table.uid IS NOT NULL
                GROUP BY table.uid, table.col2
               ) AS uid_grouping
               GROUP BY uid_grouping.grouping_id, uid_grouping.group_0
               """)
    end
  end

  describe "distinct statistics" do
    defp distinct_count_target(query) do
      [%{name: "count", args: [{:distinct, target}]} | _] = query.aggregators
      target
    end

    test "global count" do
      assert transform(
               "select count(distinct col1) from table",
               &Transformer.compute_distinct_statistics(&1, distinct_count_target(&1))
             ) ==
               flatten("""
               SELECT
                uid_grouping.grouping_id AS grouping_id,
                MAX((CAST(uid_grouping.user_id IS NOT NULL AS integer)*CAST(uid_grouping.count_distinct AS bigint)))
                  AS noise_factor,
                SUM(uid_grouping.count_distinct) AS count_distinct
               FROM (
                SELECT
                  distinct_values.grouping_id AS grouping_id,
                  distinct_values.user_id AS user_id,
                  COUNT(distinct_values.target) AS count_distinct
                FROM (
                  SELECT
                    0 AS grouping_id,
                    table.col1 AS target,
                    CASE WHEN (COUNT(DISTINCT table.uid) < 3) THEN MIN(table.uid) ELSE NULL END AS user_id
                  FROM table
                  WHERE table.uid IS NOT NULL
                  GROUP BY table.col1
               ) AS distinct_values
               GROUP BY distinct_values.grouping_id, distinct_values.user_id
               ) AS uid_grouping
               GROUP BY uid_grouping.grouping_id
               """)
    end

    test "grouping" do
      assert transform(
               "select count(distinct col1) from table group by col2",
               &Transformer.compute_distinct_statistics(&1, distinct_count_target(&1))
             ) ==
               flatten("""
               SELECT
                uid_grouping.grouping_id AS grouping_id,
                uid_grouping.group_0 AS group_0,
                MAX((CAST(uid_grouping.user_id IS NOT NULL AS integer)*CAST(uid_grouping.count_distinct AS bigint)))
                  AS noise_factor,
                SUM(uid_grouping.count_distinct) AS count_distinct
               FROM (
                SELECT
                  distinct_values.grouping_id AS grouping_id,
                  distinct_values.user_id AS user_id,
                  distinct_values.group_0 AS group_0,
                  COUNT(distinct_values.target) AS count_distinct
                FROM (
                  SELECT
                    0 AS grouping_id,
                    table.col2 AS group_0,
                    table.col1 AS target,
                    CASE WHEN (COUNT(DISTINCT table.uid) < 3) THEN MIN(table.uid) ELSE NULL END AS user_id
                  FROM table
                  WHERE table.uid IS NOT NULL
                  GROUP BY table.col2, table.col1
                ) AS distinct_values
                GROUP BY distinct_values.grouping_id, distinct_values.user_id, distinct_values.group_0
               ) AS uid_grouping
               GROUP BY uid_grouping.grouping_id, uid_grouping.group_0
               """)
    end
  end
end
