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
    {"avg(distinct <col>)", true},
    {"avg_noise(distinct <col>)", false},
    {"round(stddev(<col>), 6)", true},
    {"stddev_noise(<col>)", false},
    {"round(stddev(distinct <col>), 6)", true},
    {"stddev_noise(distinct <col>)", false},
    {"median(<col>)", true},
    {"median(distinct <col>)", true},
    {"max(<col>)", true},
    {"max(distinct <col>)", true},
    {"min(<col>)", true},
    {"min(distinct <col>)", true}
  ],
  fn {aggregate, allowed_in_subquery} ->
    defmodule Module.concat([Compliance.AggregateFunctions, String.to_atom(aggregate), Test]) do
      use ComplianceCase, async: true
      alias Cloak.DataSource.{MongoDB, DrillRODBC}

      @moduletag :"#{aggregate}"
      @integer_columns for {column, _table, _user_id} <- integer_columns(), do: column

      Enum.each(numerical_columns(), fn {column, table, uid} ->
        if allowed_in_subquery do
          @tag compliance: "#{aggregate} #{column} #{table} subquery"
          test "aggregate #{aggregate} on input #{column} in a sub-query on #{table}", context do
            context
            |> disable_for(DrillRODBC, unquote(aggregate) =~ ~r/stddev/)
            |> assert_consistent_and_not_failing("""
              SELECT
                aggregate
              FROM (
                SELECT
                  #{unquote(uid)},
                  #{on_column(unquote(aggregate), unquote(column))} as aggregate
                FROM #{unquote(table)}
                GROUP BY #{unquote(uid)}
              ) table_alias
              ORDER BY 1
            """)
          end
        end

        @tag compliance: "#{aggregate} #{column} #{table} query"
        test "aggregate #{aggregate} on input #{column} in query on #{table}", context do
          [function | _] = String.split(unquote(aggregate), "(")

          context
          |> disable_for(
            MongoDB,
            function in ~w(min max median) and unquote(column) in @integer_columns
          )
          |> assert_consistent_and_not_failing("""
            SELECT #{on_column(unquote(aggregate), unquote(column))}
            FROM #{unquote(table)}
          """)
        end
      end)
    end
  end
)
