Enum.each(
  [
    "-<col>",
    "+<col>",
    "abs(<col>)",
    "ceil(<col>)",
    "ceiling(<col>)",
    "floor(<col>)",
    "round(<col>)",
    "round(<col>, 4)",
    "round(<col>, -1)",
    "sqrt(<col>)",
    "trunc(<col>)",
    "trunc(<col>, 4)",
    "trunc(<col>, -1)",
    "cast(<col> as integer)",
    "cast(<col> as real)",
    "cast(<col> as text)"
  ],
  fn function ->
    defmodule Module.concat([Compliance.UnaryNumericalFunctions, String.to_atom(function), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{function}"

      columns =
        if function =~ ~r/round/ or function =~ ~r/trunc/ do
          numerical_columns() |> raw_columns()
        else
          numerical_columns()
        end

      Enum.each(columns, fn {column, table, uid} ->
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
  end
)
