Enum.each(
  [
    "<col1> + <col2> + NULL",
    "<col1> * <col2>",
    "<col1> + <col2>",
    "<col1> - <col2>",
    "<col1> / <col2>",
    "<col1> ^ <col2>",
    "<col1> / (<col2> - 15)",
    "<col1> % <col2>",
    "<col1> % (<col2> - 15)",
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

      test_reverse_parameters? = not String.starts_with?(function, "bucket")

      Enum.each(numerical_columns(), fn {column, table} ->
        @tag compliance: "#{function} #{column} #{table} parameter 1 subquery"
        test "#{function} on input column #{column} from table #{table} as parameter 1, in a sub-query", context do
          context
          |> disable_divide_by_zero(unquote(function))
          |> disable_modulo(unquote(function), {unquote(column), unquote(table)})
          |> assert_consistent_and_not_failing("""
            SELECT
              output
            FROM (
              SELECT
                user_id,
                #{on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output NULLS FIRST
          """)
        end

        if test_reverse_parameters? do
          @tag compliance: "#{function} #{column} #{table} parameter 2 subquery"
          test "#{function} on input column #{column} from table #{table} as parameter 2, in a sub-query", context do
            context
            |> disable_divide_by_zero(unquote(function))
            |> disable_modulo(unquote(function), {unquote(column), unquote(table)})
            |> assert_consistent_and_not_failing("""
              SELECT
                output
              FROM (
                SELECT
                  user_id,
                  #{on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
                FROM #{unquote(table)}
              ) table_alias
              ORDER BY output NULLS FIRST
            """)
          end
        end

        @tag compliance: "#{function} #{column} #{table} parameter 1 query"
        test "#{function} on input column #{column} from table #{table} as parameter 1, in main query", context do
          context
          |> disable_divide_by_zero(unquote(function))
          |> disable_modulo(unquote(function), {unquote(column), unquote(table)})
          |> assert_consistent_and_not_failing("""
            SELECT
              #{on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
            FROM #{unquote(table)}
            ORDER BY output NULLS FIRST
          """)
        end

        if test_reverse_parameters? do
          @tag compliance: "#{function} #{column} #{table} parameter 2 query"
          test "#{function} on input column #{column} from table #{table} as parameter 2, in main query", context do
            context
            |> disable_divide_by_zero(unquote(function))
            |> disable_modulo(unquote(function), {unquote(column), unquote(table)})
            |> assert_consistent_and_not_failing("""
              SELECT
                #{on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
              FROM #{unquote(table)}
              ORDER BY output NULLS FIRST
            """)
          end
        end
      end)

      def disable_divide_by_zero(context, function) do
        disable_for(context, Cloak.DataSource.MongoDB, function =~ ~r/\/.*-/)
      end

      def disable_modulo(context, function, column) do
        context
        |> disable_for(:all, function =~ ~r/%/ and column in float_columns())
        |> disable_for(Cloak.DataSource.MongoDB, function =~ ~r/%/)
      end
    end
  end
)
