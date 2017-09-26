Enum.each([
  "day(<col>)",
  "hour(<col>)",
  "minute(<col>)",
  "month(<col>)",
  "quarter(<col>)",
  "second(<col>)",
  "year(<col>)",
  "weekday(<col>)",
  "date_trunc('year', <col>)",
  "date_trunc('quarter', <col>)",
  "date_trunc('month', <col>)",
  "date_trunc('day', <col>)",
  "date_trunc('hour', <col>)",
  "date_trunc('minute', <col>)",
  "date_trunc('second', <col>)",
  "<col> + interval 'P1Y'",
  "<col> - interval 'P1M'",
  "<col> - <col>",
  "2 * (<col> + interval 'P1Y' - <col>)",
], fn(function) ->
  defmodule Module.concat([Compliance.DateTimeFunctions, String.to_atom(function), Test]) do
    use ComplianceCase, async: true

    @moduletag :"#{function}"

    Enum.each(datetime_columns(), fn({column, table, uid}) ->
      @tag compliance: "#{function} #{column} #{table} subquery"
      test "#{function} on input #{column} in a sub-query on #{table}", context do
        context
        |> disable_for(:all, match?("weekday" <> _, unquote(function)))
        |> assert_consistent_and_not_failing("""
          SELECT
            output
          FROM (
            SELECT
              #{unquote(uid)},
              #{on_column(unquote(function), unquote(column))} as output
            FROM #{unquote(table)}
            ORDER BY 1, 2
          ) table_alias
          ORDER BY output
        """)
      end

      @tag compliance: "#{function} #{column} #{table} query"
      test "#{function} on input #{column} in query on #{table}", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT #{on_column(unquote(function), unquote(column))}
          FROM #{unquote(table)}
          ORDER BY 1
        """)
      end
    end)
  end
end)
