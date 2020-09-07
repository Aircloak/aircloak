defmodule Cloak.Sql.Query.Explainer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Query.Explainer
  alias Cloak.DataSource.Table

  describe "single table" do
    test "with statistics anonymization" do
      assert explain("select count(col_1) from table_1") == [
               "query (anonymized, statistics)",
               "  --> Aircloak:regular_stats (restricted)",
               "    --> Aircloak:uid_grouping (restricted)",
               "      --> table_1 (table)"
             ]
    end

    test "with user_id anonymization" do
      assert explain("select stddev(col_1) from table_1") == [
               "query (anonymized, user_id)",
               "  --> table_1 (table)"
             ]
    end

    test "with single noise layer" do
      assert explain("select count(col_1) from table_1 where col_1 between 0 and 10") == [
               "query (anonymized, statistics, 1 noise layer)",
               "  --> Aircloak:regular_stats (restricted, 1 noise layer)",
               "    --> Aircloak:uid_grouping (restricted, 1 noise layer)",
               "      --> table_1 (table)"
             ]
    end

    test "with multiple noise layers" do
      assert explain("""
             select count(col_1)
             from table_1
             where col_1 between 0 and 10 and col_2 = 0
             """) == [
               "query (anonymized, statistics, 3 noise layers)",
               "  --> Aircloak:regular_stats (restricted, 3 noise layers)",
               "    --> Aircloak:uid_grouping (restricted, 3 noise layers)",
               "      --> table_1 (table)"
             ]
    end

    test "directly selecting columns" do
      assert explain("select col_1 from table_1") == [
               "query (anonymized, statistics, 2 noise layers)",
               "  --> Aircloak:regular_stats (restricted, 2 noise layers)",
               "    --> Aircloak:uid_grouping (restricted, 2 noise layers)",
               "      --> table_1 (table)"
             ]
    end

    test "shows the table alias" do
      assert explain("select count(*) from table_1 t1") == [
               "query (anonymized, statistics)",
               "  --> Aircloak:regular_stats (restricted)",
               "    --> Aircloak:uid_grouping (restricted)",
               "      --> t1 (table)"
             ]
    end
  end

  describe "restricted subquery" do
    test "with statistics anonymization" do
      assert explain("select count(x.aliased) from (select col_1 as aliased from table_1) x") == [
               "query (anonymized, statistics)",
               "  --> Aircloak:regular_stats (restricted)",
               "    --> Aircloak:uid_grouping (restricted)",
               "      --> x (restricted)",
               "        --> table_1 (table)"
             ]
    end

    test "with user_id anonymization" do
      assert explain("select stddev(x.aliased) from (select col_1 as aliased from table_1) x") == [
               "query (anonymized, user_id)",
               "  --> x (restricted)",
               "    --> table_1 (table)"
             ]
    end

    test "directly selecting columns" do
      assert explain("select x.aliased from (select col_1 as aliased from table_1) x") == [
               "query (anonymized, statistics, 2 noise layers)",
               "  --> Aircloak:regular_stats (restricted, 2 noise layers)",
               "    --> Aircloak:uid_grouping (restricted, 2 noise layers)",
               "      --> x (restricted, 2 noise layers)",
               "        --> table_1 (table)"
             ]
    end

    test "with noise layers" do
      assert explain("""
             select count(x.aliased)
             from (
               select col_1 as aliased from table_1
               where col_1 between 0 and 10 and col_2 = 0
              ) x
             """) == [
               "query (anonymized, statistics, 3 noise layers)",
               "  --> Aircloak:regular_stats (restricted, 3 noise layers)",
               "    --> Aircloak:uid_grouping (restricted, 3 noise layers)",
               "      --> x (restricted, 3 noise layers)",
               "        --> table_1 (table)"
             ]
    end
  end

  describe "joins" do
    test "2 tables" do
      assert explain("""
             select count(table_1.col_1)
             from table_1
             inner join table_2
             on table_1.col_2 = table_2.col_2
             """) == [
               "query (anonymized, statistics)",
               "  --> Aircloak:regular_stats (restricted)",
               "    --> Aircloak:uid_grouping (restricted)",
               "      --> table_1 (table)",
               "      --> table_2 (table)"
             ]
    end

    test "table and subquery" do
      assert explain("""
             select count(table_1.col_1)
             from table_1
             inner join (
               select col_2
               from table_2
             ) x
             on table_1.col_2 = x.col_2
             """) == [
               "query (anonymized, statistics)",
               "  --> Aircloak:regular_stats (restricted)",
               "    --> Aircloak:uid_grouping (restricted)",
               "      --> table_1 (table)",
               "      --> x (restricted)",
               "        --> table_2 (table)"
             ]
    end

    test "mixed content type tables and subqueries" do
      assert explain("""
             select count(table_1.col_1)
             from table_1
             inner join (
               select col_2
               from table_2
             ) t2
             on table_1.col_2 = t2.col_2
             inner join (
              select * from table_3
             ) t3
             on table_1.col_2 = t3.col_2
             """) == [
               "query (anonymized, statistics)",
               "  --> Aircloak:regular_stats (restricted)",
               "    --> Aircloak:uid_grouping (restricted)",
               "      --> table_1 (table)",
               "      --> t2 (restricted)",
               "        --> table_2 (table)",
               "      --> t3 (standard)",
               "        --> table_3 (table)"
             ]
    end
  end

  test "inner anonymized query" do
    assert explain("""
           select sum(x.col_1) from (
             select col_1, sum(col_2)
             from table_1
             group by 1
           ) x
           """) == [
             "query (emulated, standard)",
             "  --> x (anonymized, statistics, 2 noise layers)",
             "    --> Aircloak:regular_stats (restricted, 2 noise layers)",
             "      --> Aircloak:uid_grouping (restricted, 2 noise layers)",
             "        --> table_1 (table)"
           ]
  end

  test "selecting from non-personal table" do
    assert explain("select * from table_3") == [
             "query (standard)",
             "  --> table_3 (table)"
           ]
  end

  test "selecting from view" do
    assert explain("select count(*) from some_view",
             views: %{
               "some_view" => %{sql: "select * from table_1 where col_1 between 0 and 10"}
             }
           ) == [
             "query (anonymized, statistics, 1 noise layer)",
             "  --> Aircloak:regular_stats (restricted, 1 noise layer)",
             "    --> Aircloak:uid_grouping (restricted, 1 noise layer)",
             "      --> some_view (view, restricted, 1 noise layer)",
             "        --> table_1 (table)"
           ]
  end

  test "emulated query" do
    assert explain("""
           select count(*) from (
             select col_2 from table_1
             where col_1 between 0 and 10
           ) x
           inner join (
             select col_2 from table_3
             order by dec_b64(col_text)
           ) y
           on x.col_2 = y.col_2
           """) == [
             "query (emulated, anonymized, statistics, 1 noise layer)",
             "  --> Aircloak:regular_stats (emulated, restricted, 1 noise layer)",
             "    --> Aircloak:uid_grouping (emulated, restricted, 1 noise layer)",
             "      --> x (restricted, 1 noise layer)",
             "        --> table_1 (table)",
             "      --> y (emulated, standard)",
             "        --> table_3 (table)"
           ]
  end

  @data_source %{
    name: "explainer_test_data_source",
    driver: Cloak.DataSource.PostgreSQL,
    tables: %{
      table_1:
        Cloak.DataSource.Table.new(
          "table_1",
          "user_id",
          db_name: "table",
          columns: [
            Table.column("user_id", :integer),
            Table.column("col_1", :integer),
            Table.column("col_2", :integer)
          ],
          keys: %{
            "col_2" => :key
          }
        ),
      table_2:
        Cloak.DataSource.Table.new(
          "table_2",
          "user_id",
          db_name: "table",
          columns: [
            Table.column("user_id", :integer),
            Table.column("col_1", :integer),
            Table.column("col_2", :integer)
          ],
          keys: %{
            "col_2" => :key
          }
        ),
      table_3:
        Cloak.DataSource.Table.new(
          "table_3",
          nil,
          db_name: "table",
          columns: [
            Table.column("col_1", :integer),
            Table.column("col_2", :integer),
            Table.column("col_text", :text)
          ],
          keys: %{
            "col_2" => :key
          }
        )
    }
  }

  defp explain(query, options \\ []) do
    query
    |> Cloak.Test.QueryHelpers.compile!(@data_source, options)
    |> Cloak.Query.DbEmulator.compile()
    |> Explainer.explain()
    |> Explainer.format_explanation()
  end
end
