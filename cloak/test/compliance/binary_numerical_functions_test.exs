Enum.each(
  [
    "<col1> * <col2>",
    "<col1> + <col2>",
    "<col1> - <col2>",
    "<col1> / <col2>",
    "<col1> ^ <col2>",
    "pow(<col1>, <col2>)",
    "bucket(<col1> by <col2>)",
    "bucket(<col1> by <col2> align lower)",
    "bucket(<col1> by <col2> align upper)",
    "bucket(<col1> by <col2> align middle)"
  ],
  fn function ->
    defmodule Module.concat([Compliance.BinaryNumericalFunctions, String.to_atom(function), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{function}"

      columns =
        if function =~ ~r/bucket/ do
          numerical_columns() |> raw_columns()
        else
          numerical_columns()
        end

      test_reverse_parameters? = not String.starts_with?(function, "bucket")

      Enum.each(columns, fn {column, table, uid} ->
        @tag compliance: "#{function} #{column} #{table} parameter 1 subquery"
        test "#{function} on input column #{column} from table #{table} as parameter 1, in a sub-query",
             context do
          context
          |> assert_consistent_and_not_failing("""
            SELECT
              output
            FROM (
              SELECT
                #{unquote(uid)},
                #{on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output
          """)
        end

        if test_reverse_parameters? do
          @tag compliance: "#{function} #{column} #{table} parameter 2 subquery"
          test "#{function} on input column #{column} from table #{table} as parameter 2, in a sub-query",
               context do
            context
            |> assert_consistent_and_not_failing("""
              SELECT
                output
              FROM (
                SELECT
                  #{unquote(uid)},
                  #{on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
                FROM #{unquote(table)}
              ) table_alias
              ORDER BY output
            """)
          end
        end

        @tag compliance: "#{function} #{column} #{table} parameter 1 query"
        test "#{function} on input column #{column} from table #{table} as parameter 1, in main query",
             context do
          context
          |> assert_consistent_and_not_failing("""
            SELECT
              #{on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
            FROM #{unquote(table)}
            ORDER BY output
          """)
        end

        if test_reverse_parameters? do
          @tag compliance: "#{function} #{column} #{table} parameter 2 query"
          test "#{function} on input column #{column} from table #{table} as parameter 2, in main query",
               context do
            context
            |> assert_consistent_and_not_failing("""
              SELECT
                #{on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
              FROM #{unquote(table)}
              ORDER BY output
            """)
          end
        end
      end)
    end
  end
)
