Enum.each(
  [
    # {aggregate function, whether it is supported in subqueries}
    {"count(*)", true},
    {"count_noise(*)", false},
    {"count(<col>)", true},
    {"count_noise(<col>)", false},
    {"count(distinct <col>)", true},
    {"count_noise(distinct <col>)", false},
    {"avg(<col>)", true},
    {"avg_noise(<col>)", false},
    {"round(stddev(<col>), 6)", true},
    {"stddev_noise(<col>)", false},
    {"round(variance(<col>), 6)", true},
    {"variance_noise(<col>)", false},
    {"max(<col>)", true},
    {"min(<col>)", true}
  ],
  fn {aggregate, allowed_in_subquery} ->
    defmodule Module.concat([Compliance.AggregateFunctions, String.to_atom(aggregate), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{aggregate}"
      Enum.each(numerical_columns(), fn {column, table} ->
        if allowed_in_subquery do
          @tag compliance: "#{aggregate} #{column} #{table} subquery"
          test "aggregate #{aggregate} on input #{column} in a sub-query on #{table}", context do
            context
            |> assert_consistent_and_not_failing("""
              SELECT
                aggregate
              FROM (
                SELECT
                  user_id,
                  #{on_column(unquote(aggregate), unquote(column))} as aggregate
                FROM #{unquote(table)}
                GROUP BY 1
              ) table_alias
              ORDER BY 1
            """)
          end
        end

        @tag compliance: "#{aggregate} #{column} #{table} query"
        test "aggregate #{aggregate} on input #{column} in query on #{table}", context do
          assert_consistent_and_not_failing(context, """
            SELECT #{on_column(unquote(aggregate), unquote(column))}
            FROM #{unquote(table)}
          """)
        end
      end)
    end
  end
)
