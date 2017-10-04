# NOTE:
# - substring(<col> FROM 0) has been disabled as it produces wildly different results when emulated
Enum.each([
  "<col> || 'text-value'",
  "'text-value' || <col>",
  "btrim(<col>)",
  "concat(<col>, 'text-value')",
  "concat('text-value', <col>)",
  "hex(<col>)",
  "lcase(<col>)",
  "left(<col>, 1)",
  "left(<col>, 10)",
  "left(<col>, 1000000)",
  "length(<col>)",
  "lower(<col>)",
  "ltrim(<col>)",
  "right(<col>, 1)",
  "right(<col>, 10)",
  "right(<col>, 1000000)",
  "rtrim(<col>)",
  "substring(<col> FROM 1 FOR 1)",
  "substring(<col> FROM 1 FOR 1000)",
  "substring(<col> FROM 10 FOR 10)",
  "substring(<col> FROM 10 FOR 1000)",
  "substring(<col> FROM 10)",
  "substring(<col> FOR 10)",
  "substring(<col> FOR 1000)",
  "trim(<col>)",
  "ucase(<col>)",
  "upper(<col>)",
  "extract_words(<col>)",
], fn(function) ->
  defmodule Module.concat([Compliance.StringFunctions, String.to_atom(function), Test]) do
    use ComplianceCase, async: true, timeout: :timer.minutes(2)

    @moduletag :"#{function}"

    Enum.each(text_columns(), fn({column, table, uid}) ->
      @tag compliance: "#{function} #{column} #{table} subquery"
      test "#{function} on input #{column} in a sub-query on #{table}", context do
        context
        |> disable_for(Cloak.DataSource.SQLServer, match?("hex" <> _, unquote(function)))
        |> disable_for(Cloak.DataSource.SQLServerTds, match?("hex" <> _, unquote(function)))
        |> disallowed_in_subqueries("extract_words", unquote(function))
        |> assert_consistent_and_not_failing("""
          SELECT
            output
          FROM (
            SELECT
              #{unquote(uid)},
              #{on_column(unquote(function), "\"#{unquote(column)}\"")} as output
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
          SELECT #{on_column(unquote(function), "\"#{unquote(column)}\"")}
          FROM #{unquote(table)}
          ORDER BY 1
        """)
      end
    end)

    defp disallowed_in_subqueries(context, function, current_test), do:
      disable_for(context, :all, String.starts_with?(current_test, function))
  end
end)
