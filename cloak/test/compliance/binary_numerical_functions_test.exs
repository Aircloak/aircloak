Enum.each([
  "<col1> * <col2>",
  "<col1> + <col2>",
  "<col1> - <col2>",
  "<col1> / <col2>",
  "<col1> ^ <col2>",
  "div(<col1>, <col2>)",
  "mod(<col1>, <col2>)",
  "pow(<col1>, <col2>)",
  "bucket(<col1> by <col2>)",
  "bucket(<col1> by <col2> align lower)",
  "bucket(<col1> by <col2> align upper)",
  "bucket(<col1> by <col2> align middle)",
], fn(function) ->
  defmodule Module.concat([Compliance.BinaryNumericalFunctions, String.to_atom(function), Test]) do
    use ExUnit.Case, async: true

    @moduletag :exclude_in_dev
    @moduletag :compliance
    @moduletag :"#{function}"
    @moduletag report: [:compliance]

    alias Compliance.Helpers
    alias Cloak.DataSource.MongoDB

    setup_all do
      {:ok, data_sources: Helpers.data_sources()}
    end

    test_reverse_parameters? = not String.starts_with?(function, "bucket")
    Enum.each(Helpers.integer_columns(), fn({column, table, uid}) ->
      @tag compliance: "#{function} #{column} #{table} parameter 1 subquery"
      test "#{function} on input column #{column} from table #{table} as parameter 1, in a sub-query", context do
        context
        |> Helpers.disable_for(MongoDB, match?("<col1> /" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> +" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> *" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> -" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> ^" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("div" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("pow" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("mod" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("length" <> _, unquote(column)))
        |> Helpers.assert_consistent_and_not_failing("""
          SELECT
            output
          FROM (
            SELECT
              #{unquote(uid)},
              #{Helpers.on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
            FROM #{unquote(table)}
          ) table_alias
          ORDER BY output
        """)
      end

      if test_reverse_parameters? do
        @tag compliance: "#{function} #{column} #{table} parameter 2 subquery"
        test "#{function} on input column #{column} from table #{table} as parameter 2, in a sub-query", context do
          context
          |> Helpers.disable_for(MongoDB, match?("<col1> /" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> +" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> *" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> -" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> ^" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("div" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("pow" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("mod" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("length" <> _, unquote(column)))
          |> Helpers.assert_consistent_and_not_failing("""
            SELECT
              output
            FROM (
              SELECT
                #{unquote(uid)},
                #{Helpers.on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output
          """)
        end
      end

      @tag compliance: "#{function} #{column} #{table} parameter 1 query"
      test "#{function} on input column #{column} from table #{table} as parameter 1, in main query", context do
        context
        |> Helpers.disable_for(MongoDB, match?("<col1> /" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> +" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> *" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> -" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("<col1> ^" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("div" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("pow" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("mod" <> _, unquote(function)))
        |> Helpers.disable_for(MongoDB, match?("length" <> _, unquote(column)))
        |> Helpers.assert_consistent_and_not_failing("""
          SELECT
            #{Helpers.on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
          FROM #{unquote(table)}
          ORDER BY output
        """)
      end

      if test_reverse_parameters? do
        @tag compliance: "#{function} #{column} #{table} parameter 2 query"
        test "#{function} on input column #{column} from table #{table} as parameter 2, in main query", context do
          context
          |> Helpers.disable_for(MongoDB, match?("<col1> /" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> +" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> *" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> -" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("<col1> ^" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("div" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("pow" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("mod" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("length" <> _, unquote(column)))
          |> Helpers.assert_consistent_and_not_failing("""
            SELECT
              #{Helpers.on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
            FROM #{unquote(table)}
            ORDER BY output
          """)
        end
      end
    end)
  end
end)
