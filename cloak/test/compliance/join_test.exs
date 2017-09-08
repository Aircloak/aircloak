Enum.each([
  "INNER JOIN",
  "LEFT OUTER JOIN",
  "RIGHT OUTER JOIN",
], fn(join_type) ->
  defmodule Module.concat([Compliance.Join, String.to_atom(join_type), Test]) do
    use ExUnit.Case, async: true

    @moduletag :exclude_in_dev
    @moduletag :compliance
    @moduletag :"join_type"
    @moduletag report: [:compliance]

    alias Compliance.Helpers
    alias Cloak.DataSource.MongoDB

    setup_all do
      {:ok, data_sources: Helpers.data_sources()}
    end

    Enum.each(Helpers.table_pairs(), fn({{table1, uid1}, {table2, uid2}}) ->
      @tag compliance: "#{join_type} #{table1} #{table2}"
      test "#{join_type} between #{table1} and #{table2}", context do
        context
        |> Helpers.disable_for(MongoDB, unquote(join_type) == "INNER JOIN")
        |> Helpers.assert_consistent_and_not_failing("""
          SELECT count(*)
          FROM #{unquote(table1)} a #{unquote(join_type)} #{unquote(table2)}
            ON a.#{unquote(uid1)} = #{unquote(table2)}.#{unquote(uid2)}
        """)
      end

      @tag compliance: "#{join_type} #{table1} #{table2} subqueries"
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

      @tag compliance: "#{join_type} #{table1} #{table2} subquery"
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
  end
end)

defmodule Module.Compliance.Join.CrossJoin.Test do
  use ExUnit.Case, async: true

  @moduletag :exclude_in_dev
  @moduletag :compliance
  @moduletag :"CROSS JOIN"
  @moduletag report: [:compliance]

  alias Compliance.Helpers

  setup_all do
    {:ok, data_sources: Helpers.data_sources()}
  end

  Enum.each([
    "CROSS JOIN",
    _implicit_cross_join = ",",
  ], fn(join_type) ->
    Enum.each(Helpers.table_pairs(), fn({{table1, uid1}, {table2, uid2}}) ->
      @tag compliance: "#{join_type} #{table1} #{table2}"
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
