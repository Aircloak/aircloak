# NOTE:
# - weekday(<col>) produces inconsistent results and has therefore been disabled
Enum.each([
  "day(<col>)",
  "hour(<col>)",
  "minute(<col>)",
  "month(<col>)",
  "quarter(<col>)",
  "second(<col>)",
  "year(<col>)",
  "date_trunc('year', <col>)",
  "date_trunc('quarter', <col>)",
  "date_trunc('month', <col>)",
  "date_trunc('day', <col>)",
  "date_trunc('hour', <col>)",
  "date_trunc('minute', <col>)",
  "date_trunc('second', <col>)",
], fn(function) ->
  defmodule Module.concat([Compliance.DateTimeFunctions, String.to_atom(function), Test]) do
    use ComplianceCase, async: true
    alias Cloak.DataSource.MongoDB

    @moduletag :"#{function}"

    Enum.each(datetime_columns(), fn({column, table, uid}) ->
      @tag compliance: "#{function} #{column} #{table} subquery"
      test "#{function} on input #{column} in a sub-query on #{table}", context do
        context
        |> disable_for(MongoDB, match?("date_trunc" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("month" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("minute" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("hour" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("day" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("year" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("quarter" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("second" <> _, unquote(function)))
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
        |> disable_for(MongoDB, match?("date_trunc" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("month" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("minute" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("hour" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("day" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("year" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("quarter" <> _, unquote(function)))
        |> disable_for(MongoDB, match?("second" <> _, unquote(function)))
        |> assert_consistent_and_not_failing("""
          SELECT #{on_column(unquote(function), unquote(column))}
          FROM #{unquote(table)}
          ORDER BY 1
        """)
      end
    end)
  end
end)
