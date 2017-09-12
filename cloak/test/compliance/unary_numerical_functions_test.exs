Enum.each([
  "abs(<col>)",
  "ceil(<col>)",
  "ceiling(<col>)",
  "floor(<col>)",
  "round(<col>)",
  "sqrt(<col>)",
  "trunc(<col>)",
], fn(function) ->
  defmodule Module.concat([Compliance.UnaryNumericalFunctions, String.to_atom(function), Test]) do
    use ComplianceCase, async: true

    @moduletag :"#{function}"

    Enum.each(numerical_columns(), fn({column, table, uid}) ->
      @tag compliance: "#{function} #{column} #{table} subquery"
      test "numerical unary function #{function} on input #{column} in a sub-query on #{table}", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT
            output
          FROM (
            SELECT
              #{unquote(uid)},
              #{on_column(unquote(function), unquote(column))} as output
            FROM #{unquote(table)}
          ) table_alias
          ORDER BY output
        """)
      end

      @tag compliance: "#{function} #{column} #{table} query"
      test "numerical unary function #{function} on input #{column} in query on #{table}", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT #{on_column(unquote(function), unquote(column))} as output
          FROM #{unquote(table)}
          ORDER BY output
        """)
      end
    end)
  end
end)
