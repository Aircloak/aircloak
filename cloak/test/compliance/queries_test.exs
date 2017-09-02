defmodule Compliance.Queries.Test do
  use ExUnit.Case, async: true

  @moduletag :exclude_in_dev
  @moduletag :compliance

  import Cloak.Test.QueryHelpers
  require Aircloak.DeployConfig

  setup_all do
    data_sources = Compliance.DataSources.all_from_config_initialized("compliance")

    assert(length(data_sources) > 1, "More than one data source is needed to ensure compliance")

    {:ok, data_sources: data_sources}
  end

  # NOTE to query/test writers
  # The schema available to you for writing queries is as follows:
  #
  # users                                           addresses
  # - id (integer)                                  - uid (uid column - integer)
  # - user_id (user-id column - integer)            - user_fk (integer - foreign key for users.id)
  # - age (integer)                                 - home.city (string)
  # - height (float)                                - home.postal_code (integer)
  # - active (boolean)                              - work.city (string)
  # - name (string)                                 - work.postal_code (integer)
  #
  # notes                                           notes_changes
  # - uid (integer - projected uid column)          - uid (uid column - integer)
  # - user_fk (integer - foreign key for users.id)  - note_id (integer - foreign key for notes.id)
  # - id (integer)                                  - user_fk (integer - foreign key for users.id)
  # - title (string)                                - title (string)
  # - content (string)                              - content (string)
  #                                                 - changes.change (string)
  #                                                 - changes.date (datetime)

  @float_columns [
    # {column name, table name, uid column in table}
    {"height", "users", "user_id"},
  ]
  @integer_columns [
    # {column name, table name, uid column in table}
    {"length(name)", "users", "user_id"},
    {"length(title)", "notes", "uid"},
    {"age", "users", "user_id"},
    {"id", "notes", "uid"},
    {"user_fk", "addresses", "uid"},
    {"home.postal_code", "addresses", "uid"},
    {"work.postal_code", "addresses", "uid"},
    {"length(home.city)", "addresses", "uid"},
    {"length(work.city)", "addresses", "uid"},
    {"user_fk", "notes", "uid"},
    {"length(content)", "notes", "uid"},
    {"length(title)", "notes_changes", "uid"},
    {"length(content)", "notes_changes", "uid"},
    {"length(changes.change)", "notes_changes", "uid"},
    {"note_id", "notes_changes", "uid"},
  ]
  @numerical_columns @float_columns ++ @integer_columns
  @datetime_columns [
    # {column name, table name, uid column in table}
    {"changes.date", "notes_changes", "uid"},
  ]
  @text_columns [
    # {column name, table name, uid column in table}
    {"name", "users", "user_id"},
    {"home.city", "addresses", "uid"},
    {"work.city", "addresses", "uid"},
    {"title", "notes", "uid"},
    {"content", "notes", "uid"},
    {"title", "notes_changes", "uid"},
    {"content", "notes_changes", "uid"},
    {"changes.change", "notes_changes", "uid"},
  ]

  describe "aggregate functions" do
    # NOTE:
    # - stddev[_noise] is missing because it crashes on values from the users table
    Enum.each([
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
      {"median(<col>)", true},
      {"median(distinct <col>)", true},
      {"max(<col>)", true},
      {"max(distinct <col>)", true},
      {"min(<col>)", true},
      {"min(distinct <col>)", true},
    ], fn({aggregate, allowed_in_subquery}) ->

      Enum.each(@numerical_columns, fn({column, table, uid}) ->

        if allowed_in_subquery do
          test "aggregate #{aggregate} on input #{column} in a sub-query on #{table}", context do
            assert_consistent_and_not_failing context, """
              SELECT
                aggregate
              FROM (
                SELECT
                  #{unquote(uid)},
                  #{on_column(unquote(aggregate), unquote(column))} as aggregate
                FROM #{unquote(table)}
                GROUP BY #{unquote(uid)}
              ) table_alias
              ORDER BY aggregate
            """
          end
        end

        test "aggregate #{aggregate} on input #{column} in query on #{table}", context do
          assert_consistent_and_not_failing context,  """
            SELECT #{on_column(unquote(aggregate), unquote(column))}
            FROM #{unquote(table)}
          """
        end
      end)
    end)
  end

  describe "unary numerical functions" do
    Enum.each([
      "abs(<col>)",
      "ceil(<col>)",
      "ceiling(<col>)",
      "floor(<col>)",
      "round(<col>)",
      "sqrt(<col>)",
      "trunc(<col>)",
    ], fn(function) ->

      Enum.each(@numerical_columns, fn({column, table, uid}) ->

        test "numerical unary function #{function} on input #{column} in a sub-query on #{table}", context do
          assert_consistent_and_not_failing context, """
            SELECT
              output
            FROM (
              SELECT
                #{unquote(uid)},
                #{on_column(unquote(function), unquote(column))} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output
          """
        end

        test "numerical unary function #{function} on input #{column} in query on #{table}", context do
          assert_consistent_and_not_failing context, """
            SELECT #{on_column(unquote(function), unquote(column))} as output
            FROM #{unquote(table)}
            ORDER BY output
          """
        end
      end)
    end)
  end

  describe "binary numerical functions" do
    Enum.each([
      "<col1> * <col2>",
      "<col1> + <col2>",
      "<col1> - <col2>",
      "<col1> / <col2>",
      "<col1> ^ <col2>",
      "div(<col1>, <col2>)",
      "mod(<col1>, <col2>)",
      "pow(<col1>, <col2>)",
    ], fn(function) ->

      Enum.each(@integer_columns, fn({column, table, uid}) ->
        test "#{function} on input column #{column} from table #{table} as parameter 1, in a sub-query", context do
          assert_consistent_and_not_failing context, """
            SELECT
              output
            FROM (
              SELECT
                #{unquote(uid)},
                #{on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output
          """
        end

        test "#{function} on input column #{column} from table #{table} as parameter 2, in a sub-query", context do
          assert_consistent_and_not_failing context, """
            SELECT
              output
            FROM (
              SELECT
                #{unquote(uid)},
                #{on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output
          """
        end

        test "#{function} on input column #{column} from table #{table} as parameter 1, in main query", context do
          assert_consistent_and_not_failing context, """
            SELECT
              #{on_columns(unquote(function), ["#{unquote(column)}", "1"])} as output
            FROM #{unquote(table)}
            ORDER BY output
          """
        end

        test "#{function} on input column #{column} from table #{table} as parameter 2, in main query", context do
          assert_consistent_and_not_failing context, """
            SELECT
              #{on_columns(unquote(function), ["1", "#{unquote(column)}"])} as output
            FROM #{unquote(table)}
            ORDER BY output
          """
        end
      end)
    end)
  end

  describe "datetime functions" do
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

      Enum.each(@datetime_columns, fn({column, table, uid}) ->
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          assert_consistent_and_not_failing context, """
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
          """
        end

        test "#{function} on input #{column} in query on #{table}", context do
          assert_consistent_and_not_failing context, """
            SELECT #{on_column(unquote(function), unquote(column))}
            FROM #{unquote(table)}
            ORDER BY 1
          """
        end
      end)
    end)
  end

  describe "string functions" do
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
      "substring(<col> FROM 0 FOR 1)",
      "substring(<col> FROM 0 FOR 1000)",
      "substring(<col> FROM 10 FOR 10)",
      "substring(<col> FROM 10 FOR 1000)",
      "substring(<col> FROM 10)",
      "substring(<col> FOR 10)",
      "substring(<col> FOR 1000)",
      "trim(<col>)",
      "ucase(<col>)",
      "upper(<col>)",
    ], fn(function) ->

      Enum.each(@text_columns, fn({column, table, uid}) ->
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          assert_consistent_and_not_failing context, """
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
          """
        end

        test "#{function} on input #{column} in query on #{table}", context do
          assert_consistent_and_not_failing context, """
            SELECT #{on_column(unquote(function), "\"#{unquote(column)}\"")}
            FROM #{unquote(table)}
            ORDER BY 1
          """
        end
      end)
    end)
  end

  defp assert_consistent_and_not_failing(%{data_sources: data_sources}, query) do
    result = assert_query_consistency(query, data_sources: data_sources)
    if match?(%{error: _}, result) do
      raise ExUnit.AssertionError,
        message: "Query execution failed. Query was:\n#{query}.\n\nError:\n#{inspect result}"
    else
      :ok
    end
  end

  def on_column(form, column_name), do:
    String.replace(form, "<col>", column_name)

  def on_columns(form, columns), do:
    columns
    |> Enum.with_index(1)
    |> Enum.reduce(form, fn({column, index}, acc) -> String.replace(acc, "<col#{index}>", column) end)
end
