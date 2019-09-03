Enum.each(
  [
    "INNER JOIN",
    "LEFT OUTER JOIN",
    "RIGHT OUTER JOIN"
  ],
  fn join_type ->
    defmodule Module.concat([Compliance.Join, String.to_atom(join_type), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{join_type}"

      @tag compliance: "#{join_type}"
      test "#{join_type} between tables", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT count(*)
          FROM users #{unquote(join_type)} addresses
          ON users.id = user_fk
        """)
      end

      @tag compliance: "#{join_type} subqueries"
      test "#{join_type} between subqueries", context do
        assert_consistent_and_not_failing(context, """
          SELECT SUM(a.aggregate), SUM(b.aggregate)
          FROM (
            SELECT user_id, count(*) as aggregate
            FROM users
            GROUP BY 1
          ) a #{unquote(join_type)} (
            SELECT user_id, count(*) as aggregate
            FROM addresses INNER JOIN users
            ON users.id = user_fk
            GROUP BY 1
          ) b
          ON a.user_id = b.user_id
        """)
      end

      @tag compliance: "#{join_type} between table and subquery"
      test "#{join_type} between table and subquery", context do
        assert_consistent_and_not_failing(context, """
          SELECT SUM(aggregate)
          FROM (
            SELECT user_id, id, count(*) as aggregate
            FROM users
            GROUP BY 1, 2
          ) u #{unquote(join_type)} addresses
          ON u.id = user_fk
        """)
      end
    end
  end
)

defmodule Compliance.Join.CrossJoin.Test do
  use ComplianceCase, async: true

  @moduletag :"CROSS JOIN"

  Enum.each(
    [
      "CROSS JOIN",
      _implicit_cross_join = ","
    ],
    fn join_type ->
      @tag compliance: "#{join_type}"
      test "CROSS JOIN (`#{join_type}`)", context do
        assert_consistent_and_not_failing(context, """
          SELECT count(*)
          FROM (
            SELECT *
            FROM users #{unquote(join_type)} addresses
            WHERE users.id = user_fk
          ) t
        """)
      end
    end
  )
end
