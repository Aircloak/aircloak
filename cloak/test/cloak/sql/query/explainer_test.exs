defmodule Cloak.Sql.Query.Explainer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Query.Explainer
  alias Cloak.DataSource.Table

  describe "single table" do
    test "with statistics anonymization" do
      assert explain("select count(col_1) from table_1") == [
               "query (anonymized, statistics, default noise layer)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> table_1 (personal table)"
             ]

      assert explain_for_editor("select count(col_1) from table_1") ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :anonymized,
                   range: %{end: %{ch: 32, line: 0}, start: %{ch: 0, line: 0}}
                 }
               ]
    end

    test "with user_id anonymization" do
      assert explain("select stddev(col_1) from table_1") == [
               "query (anonymized, user_id, default noise layer)",
               "  --> table_1 (personal table)"
             ]

      assert explain_for_editor("select stddev(col_1) from table_1") ==
               [
                 %{
                   anonymization_type: :user_id,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :anonymized,
                   range: %{end: %{ch: 33, line: 0}, start: %{ch: 0, line: 0}}
                 }
               ]
    end

    test "with single noise layer" do
      assert explain("select count(col_1) from table_1 where col_1 between 0 and 10") == [
               "query (anonymized, statistics, 1 noise layer)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> table_1 (personal table)"
             ]

      assert explain_for_editor("select count(col_1) from table_1 where col_1 between 0 and 10") ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 1,
                   query_type: :anonymized,
                   range: %{end: %{ch: 61, line: 0}, start: %{ch: 0, line: 0}}
                 }
               ]
    end

    test "with multiple noise layers" do
      query = """
      select count(col_1)
      from table_1
      where col_1 between 0 and 10 and col_2 = 0
      """

      assert explain(query) == [
               "query (anonymized, statistics, 3 noise layers)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> table_1 (personal table)"
             ]

      assert explain_for_editor(query) ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 3,
                   query_type: :anonymized,
                   range: %{end: %{ch: 0, line: 3}, start: %{ch: 0, line: 0}}
                 }
               ]
    end

    test "directly selecting columns" do
      assert explain("select col_1 from table_1") == [
               "query (anonymized, statistics, 2 noise layers)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> table_1 (personal table)"
             ]

      assert explain_for_editor("select col_1 from table_1") ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 2,
                   query_type: :anonymized,
                   range: %{end: %{ch: 25, line: 0}, start: %{ch: 0, line: 0}}
                 }
               ]
    end

    test "shows the table alias" do
      assert explain("select count(*) from table_1 t1") == [
               "query (anonymized, statistics, default noise layer)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> t1 (personal table)"
             ]

      assert explain_for_editor("select count(*) from table_1 t1") ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :anonymized,
                   range: %{end: %{ch: 31, line: 0}, start: %{ch: 0, line: 0}}
                 }
               ]
    end
  end

  describe "restricted subquery" do
    test "with statistics anonymization" do
      assert explain("select count(x.aliased) from (select col_1 as aliased from table_1) x") == [
               "query (anonymized, statistics, default noise layer)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> x (restricted)",
               "        --> table_1 (personal table)"
             ]

      assert explain_for_editor("select count(x.aliased) from (select col_1 as aliased from table_1) x") ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :anonymized,
                   range: %{end: %{ch: 69, line: 0}, start: %{ch: 0, line: 0}}
                 },
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :restricted,
                   range: %{end: %{ch: 69, line: 0}, start: %{ch: 29, line: 0}}
                 }
               ]
    end

    test "with user_id anonymization" do
      assert explain("select stddev(x.aliased) from (select col_1 as aliased from table_1) x") == [
               "query (anonymized, user_id, default noise layer)",
               "  --> x (restricted)",
               "    --> table_1 (personal table)"
             ]

      assert explain_for_editor("select stddev(x.aliased) from (select col_1 as aliased from table_1) x") ==
               [
                 %{
                   anonymization_type: :user_id,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :anonymized,
                   range: %{end: %{ch: 70, line: 0}, start: %{ch: 0, line: 0}}
                 },
                 %{
                   anonymization_type: :user_id,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :restricted,
                   range: %{end: %{ch: 70, line: 0}, start: %{ch: 30, line: 0}}
                 }
               ]
    end

    test "directly selecting columns" do
      assert explain("select x.aliased from (select col_1 as aliased from table_1) x") == [
               "query (anonymized, statistics, 2 noise layers)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> x (restricted)",
               "        --> table_1 (personal table)"
             ]

      assert explain_for_editor("select x.aliased from (select col_1 as aliased from table_1) x") ==
               [
                 %{
                   emulated: false,
                   query_type: :anonymized,
                   anonymization_type: :statistics,
                   noise_layers: 2,
                   range: %{start: %{ch: 0, line: 0}, end: %{line: 0, ch: 62}}
                 },
                 %{
                   emulated: false,
                   query_type: :restricted,
                   anonymization_type: :statistics,
                   noise_layers: 2,
                   range: %{end: %{line: 0, ch: 62}, start: %{line: 0, ch: 22}}
                 }
               ]
    end

    test "with noise layers" do
      query = """
      select count(x.aliased)
      from (
        select col_1 as aliased from table_1
        where col_1 between 0 and 10 and col_2 = 0
       ) x
      """

      assert explain(query) == [
               "query (anonymized, statistics, 3 noise layers)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> x (restricted)",
               "        --> table_1 (personal table)"
             ]

      assert explain_for_editor(query) ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   query_type: :anonymized,
                   noise_layers: 3,
                   range: %{start: %{ch: 0, line: 0}, end: %{ch: 0, line: 5}}
                 },
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   query_type: :restricted,
                   noise_layers: 3,
                   range: %{end: %{ch: 0, line: 5}, start: %{ch: 5, line: 1}}
                 }
               ]
    end
  end

  describe "joins" do
    test "2 tables" do
      query = """
      select count(table_1.col_1)
      from table_1
      inner join table_2
      on table_1.col_2 = table_2.col_2
      """

      assert explain(query) == [
               "query (anonymized, statistics, default noise layer)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> table_1 (personal table)",
               "      --> table_2 (personal table)"
             ]

      assert explain_for_editor(query) ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   query_type: :anonymized,
                   noise_layers: 0,
                   range: %{start: %{ch: 0, line: 0}, end: %{ch: 0, line: 4}}
                 }
               ]
    end

    test "table and subquery" do
      query = """
      select count(table_1.col_1)
      from table_1
      inner join (
        select col_2
        from table_2
      ) x
      on table_1.col_2 = x.col_2
      """

      assert explain(query) == [
               "query (anonymized, statistics, default noise layer)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> table_1 (personal table)",
               "      --> x (restricted)",
               "        --> table_2 (personal table)"
             ]

      assert explain_for_editor(query) ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   query_type: :anonymized,
                   noise_layers: 0,
                   range: %{start: %{ch: 0, line: 0}, end: %{ch: 0, line: 7}}
                 },
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :restricted,
                   range: %{end: %{ch: 0, line: 6}, start: %{ch: 11, line: 2}}
                 }
               ]
    end

    test "mixed content type tables and subqueries" do
      query = """
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
      """

      assert explain(query) == [
               "query (anonymized, statistics, default noise layer)",
               "  --> regular_stats (Aircloak generated, restricted)",
               "    --> uid_grouping (Aircloak generated, restricted)",
               "      --> table_1 (personal table)",
               "      --> t2 (restricted)",
               "        --> table_2 (personal table)",
               "      --> t3 (standard)",
               "        --> table_3 (non-personal table)"
             ]

      assert explain_for_editor(query) ==
               [
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :anonymized,
                   range: %{start: %{ch: 0, line: 0}, end: %{ch: 0, line: 11}}
                 },
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :restricted,
                   range: %{end: %{ch: 0, line: 6}, start: %{ch: 11, line: 2}}
                 },
                 %{
                   anonymization_type: :statistics,
                   emulated: false,
                   noise_layers: 0,
                   query_type: :standard,
                   range: %{end: %{ch: 0, line: 10}, start: %{ch: 11, line: 7}}
                 }
               ]
    end
  end

  test "inner anonymized query" do
    query = """
    select sum(x.col_1) from (
      select col_1, sum(col_2)
      from table_1
      group by 1
    ) x
    """

    assert explain(query) == [
             "query (emulated, standard)",
             "  --> x (anonymized, statistics, 2 noise layers)",
             "    --> regular_stats (Aircloak generated, restricted)",
             "      --> uid_grouping (Aircloak generated, restricted)",
             "        --> table_1 (personal table)"
           ]

    assert explain_for_editor(query) ==
             [
               %{
                 noise_layers: 0,
                 anonymization_type: :user_id,
                 emulated: true,
                 query_type: :standard,
                 range: %{start: %{ch: 0, line: 0}, end: %{ch: 0, line: 5}}
               },
               %{
                 anonymization_type: :statistics,
                 emulated: false,
                 noise_layers: 2,
                 query_type: :anonymized,
                 range: %{end: %{ch: 0, line: 5}, start: %{ch: 25, line: 0}}
               }
             ]
  end

  test "selecting from non-personal table" do
    assert explain("select * from table_3") == [
             "query (standard)",
             "  --> table_3 (non-personal table)"
           ]

    assert explain_for_editor("select * from table_3") == [
             %{
               anonymization_type: :user_id,
               emulated: false,
               noise_layers: 0,
               query_type: :standard,
               range: %{end: %{ch: 21, line: 0}, start: %{ch: 0, line: 0}}
             }
           ]
  end

  test "selecting from view" do
    assert explain("select count(*) from some_view",
             views: %{
               "some_view" => %{sql: "select * from table_1 where col_1 between 0 and 10"}
             }
           ) == [
             "query (anonymized, statistics, 1 noise layer)",
             "  --> regular_stats (Aircloak generated, restricted)",
             "    --> uid_grouping (Aircloak generated, restricted)",
             "      --> some_view (view, restricted)",
             "        --> table_1 (personal table)"
           ]

    assert explain_for_editor("select count(*) from some_view",
             views: %{
               "some_view" => %{sql: "select * from table_1 where col_1 between 0 and 10"}
             }
           ) == [
             %{
               anonymization_type: :statistics,
               emulated: false,
               noise_layers: 1,
               query_type: :anonymized,
               range: %{end: %{ch: 30, line: 0}, start: %{ch: 0, line: 0}}
             }
           ]
  end

  test "emulated query" do
    query = """
    select count(*) from (
      select col_2 from table_1
      where col_1 between 0 and 10
    ) x
    inner join (
      select col_2 from table_3
      order by dec_b64(col_text)
    ) y
    on x.col_2 = y.col_2
    """

    assert explain(query) == [
             "query (emulated, anonymized, statistics, 1 noise layer)",
             "  --> regular_stats (Aircloak generated, emulated, restricted)",
             "    --> uid_grouping (Aircloak generated, emulated, restricted)",
             "      --> x (restricted)",
             "        --> table_1 (personal table)",
             "      --> y (emulated, standard)",
             "        --> table_3 (non-personal table)"
           ]

    assert explain_for_editor(query) ==
             [
               %{
                 anonymization_type: :statistics,
                 emulated: true,
                 noise_layers: 1,
                 query_type: :anonymized,
                 range: %{end: %{ch: 0, line: 9}, start: %{ch: 0, line: 0}}
               },
               %{
                 anonymization_type: :statistics,
                 emulated: false,
                 noise_layers: 1,
                 query_type: :restricted,
                 range: %{end: %{ch: 0, line: 4}, start: %{ch: 21, line: 0}}
               },
               %{
                 anonymization_type: :statistics,
                 emulated: true,
                 noise_layers: 0,
                 query_type: :standard,
                 range: %{end: %{ch: 0, line: 8}, start: %{ch: 11, line: 4}}
               }
             ]
  end

  test "union" do
    query = "select stddev(0) from table_1 union select stddev(0) from table_2"

    assert explain(query) == [
             "query (emulated, standard)",
             "  --> union (anonymized, user_id, default noise layer)",
             "    --> table_1 (personal table)",
             "  --> union (anonymized, user_id, default noise layer)",
             "    --> table_2 (personal table)"
           ]

    assert [
             %{
               anonymization_type: :user_id,
               emulated: true,
               noise_layers: 0,
               query_type: :standard,
               range: %{end: %{ch: 65, line: 0}, start: %{ch: 0, line: 0}}
             },
             _,
             _
           ] = explain_for_editor(query)
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

  defp explain_for_editor(query, options \\ []) do
    query
    |> Cloak.Test.QueryHelpers.compile!(@data_source, options)
    |> Cloak.Query.DbEmulator.compile()
    |> Explainer.explain()
    |> Explainer.for_editor()
  end

  defp explain(query, options \\ []) do
    query
    |> Cloak.Test.QueryHelpers.compile!(@data_source, options)
    |> Cloak.Query.DbEmulator.compile()
    |> Explainer.explain()
    |> Explainer.format_explanation()
  end
end
