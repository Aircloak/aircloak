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

      Enum.each(numerical_columns(), fn {column, table} ->
        function =
          if function == "cast(<col> as text)" do
            # In this case, we're numeric a float column to text. This can lead to some strange differences, such as
            # one database reporting 174.88 and another one 174.8800048828125. To eliminate this situation, we're
            # casting the result back to real, which should give us the results which are same or similar enough
            # (according to delta comparison). Oracle seems to sometimes add a ".0" for no good reason, so this is done
            # for integer columns as well.
            "cast(cast(<col> as text) as real)"
          else
            function
          end

        @tag compliance: "#{function} #{column} #{table} subquery"
        test "numerical unary function #{function} on input #{column} in a sub-query on #{table}", context do
          context
          |> assert_consistent_and_not_failing("""
            SELECT
              output
            FROM (
              SELECT
                user_id,
                #{on_column(unquote(function), unquote(column))} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output NULLS LAST
          """)
        end

        @tag compliance: "#{function} #{column} #{table} query"
        test "numerical unary function #{function} on input #{column} in query on #{table}", context do
          context
          |> assert_consistent_and_not_failing("""
            SELECT #{on_column(unquote(function), unquote(column))} as output
            FROM #{unquote(table)}
            ORDER BY output NULLS LAST
          """)
        end
      end)
    end
  end
)
