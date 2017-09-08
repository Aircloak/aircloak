defmodule Compliance.Join.Test do
  use ExUnit.Case, async: true

  @moduletag :compliance

  alias Compliance.Helpers
  alias Cloak.DataSource.MongoDB

  setup_all do
    data_sources = if System.get_env("TRAVIS") do
      Compliance.DataSources.all_from_config_initialized("compliance_travis")
    else
      Compliance.DataSources.all_from_config_initialized("compliance")
    end

    assert(length(data_sources) > 1, "More than one data source is needed to ensure compliance")

    {:ok, data_sources: data_sources}
  end

  describe "Distinct JOIN types" do
    Enum.each(Helpers.table_pairs(), fn({{table1, uid1}, {table2, uid2}}) ->
      Enum.each([
        "INNER JOIN",
        "LEFT OUTER JOIN",
        "RIGHT OUTER JOIN",
      ], fn(join_type) ->
        test "#{join_type} between #{table1} and #{table2}", context do
          context
          |> Helpers.disable_for(MongoDB, unquote(join_type) == "INNER JOIN")
          |> Helpers.assert_consistent_and_not_failing("""
            SELECT count(*)
            FROM #{unquote(table1)} a #{unquote(join_type)} #{unquote(table2)}
              ON a.#{unquote(uid1)} = #{unquote(table2)}.#{unquote(uid2)}
          """)
        end

        test "#{join_type} between subqueries of #{table1} and #{table2}", context do
          Helpers.assert_consistent_and_not_failing(context, """
            SELECT SUM(a.aggregate), SUM(b.aggregate)
            FROM (
              SELECT #{unquote(uid1)}, count(*) as aggregate
              FROM #{unquote(table1)}
              GROUP BY 1
            ) a #{unquote(join_type)} (
              SELECT #{unquote(uid2)}, count(*) as aggregate
              FROM #{unquote(table2)}
              GROUP BY 1
            ) b
            ON a.#{unquote(uid1)} = b.#{unquote(uid2)}
          """)
        end

        test "#{join_type} between subquery of #{table1} and table #{table2}", context do
          context
          |> Helpers.disable_for(MongoDB, unquote(join_type) == "INNER JOIN")
          |> Helpers.assert_consistent_and_not_failing("""
            SELECT SUM(a.aggregate)
            FROM (
              SELECT #{unquote(uid1)}, count(*) as aggregate
              FROM #{unquote(table1)}
              GROUP BY 1
            ) a #{unquote(join_type)} #{unquote(table2)} as b
            ON a.#{unquote(uid1)} = b.#{unquote(uid2)}
          """)
        end
      end)

      Enum.each([
        "CROSS JOIN",
        _implicit_cross_join = ",",
      ], fn(join_type) ->

        test "CROSS JOIN (`#{join_type}`) between #{table1} and #{table2}", context do
          Helpers.assert_consistent_and_not_failing(context, """
            SELECT count(*)
            FROM #{unquote(table1)} a #{unquote(join_type)} #{unquote(table2)}
            WHERE a.#{unquote(uid1)} = #{unquote(table2)}.#{unquote(uid2)}
          """)
        end
      end)
    end)
  end
end
