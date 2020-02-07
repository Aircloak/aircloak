Enum.each(
  [
    "day(<col>)",
    "hour(<col>)",
    "minute(<col>)",
    "month(<col>)",
    "quarter(<col>)",
    "second(<col>)",
    "year(<col>)",
    # "weekday(<col>)",
    "date_trunc('year', <col>)",
    "date_trunc('quarter', <col>)",
    "date_trunc('month', <col>)",
    "date_trunc('day', <col>)",
    "date_trunc('hour', <col>)",
    "date_trunc('minute', <col>)",
    "date_trunc('second', <col>)"
  ],
  fn function ->
    defmodule Module.concat([Compliance.DateTimeFunctions, String.to_atom(function), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{function}"

      columns = if function =~ ~r/<col>/, do: datetime_columns() ++ date_columns(), else: [hd(datetime_columns())]

      Enum.each(columns, fn {column, table} ->
        @tag compliance: "#{function} #{column} #{table} subquery"
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          context
          |> disable_unsupported_on_dates(unquote(function), {unquote(column), unquote(table)})
          |> assert_consistent_and_not_failing("""
            SELECT
              output, STDDEV(0)
            FROM (
              SELECT
                user_id,
                #{on_column(unquote(function), unquote(column))} as output
              FROM #{unquote(table)}
              ORDER BY 1, 2
            ) table_alias
            GROUP BY 1
            ORDER BY 1
          """)
        end

        @tag compliance: "#{function} #{column} #{table} query"
        test "#{function} on input #{column} in query on #{table}", context do
          context
          |> disable_unsupported_on_dates(unquote(function), {unquote(column), unquote(table)})
          |> assert_consistent_and_not_failing("""
            SELECT #{on_column(unquote(function), unquote(column))}
            FROM #{unquote(table)}
            ORDER BY 1
          """)
        end
      end)

      defp disable_unsupported_on_dates(context, function, column) do
        context
        |> disable_for(:all, column in date_columns() and unsupported_on_dates?(function))
        |> disable_for(Cloak.DataSource.MongoDB, column in date_columns())
        # Impala maps dates to datetime (TIMESTAMP). The returned results will be equivalent,
        # but tests will fail because other databases return results as date only.
        |> disable_for(Cloak.DataSource.ClouderaImpala, column in date_columns())
      end

      defp unsupported_on_dates?(function) do
        String.contains?(function, "second") or String.contains?(function, "minute") or
          String.contains?(function, "hour") or String.contains?(function, "-")
      end
    end
  end
)

Enum.each(
  [
    "<col> - (<col> - interval 'P1D') = interval 'P1D'"
  ],
  fn condition ->
    defmodule Module.concat([Compliance.DateTimeFunctions.Where, String.to_atom(condition), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{condition} in where"

      Enum.each(datetime_columns(), fn {column, table} ->
        @tag compliance: "#{condition} in where #{column} #{table} query"
        test "#{condition} on input #{column} in where in query on table #{table}", context do
          context
          # Impala does not support intervals as standalone expressions such as x - y = interval 2 days
          |> disable_for(Cloak.DataSource.ClouderaImpala)
          |> assert_consistent_and_not_failing("""
            SELECT COUNT(*)
            FROM #{unquote(table)}
            WHERE #{on_column(unquote(condition), unquote(column))}
          """)
        end

        @tag compliance: "#{condition} in where #{column} #{table} subquery"
        test "#{condition} on input #{column} in where in subquery on table #{table}", context do
          context
          |> disable_for(Cloak.DataSource.ClouderaImpala)
          |> assert_consistent_and_not_failing("""
            SELECT COUNT(*)
            FROM (
              SELECT user_id
              FROM #{unquote(table)}
              WHERE #{on_column(unquote(condition), unquote(column))}
            ) foo
          """)
        end
      end)
    end
  end
)

Enum.each(
  [
    "cast(<col> as time)"
  ],
  fn function ->
    defmodule Module.concat([Compliance.DateTimeFunctions.TimestampOnly, String.to_atom(function), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{function}"

      Enum.each(datetime_columns(), fn {column, table} ->
        @tag compliance: "#{function} #{column} #{table} subquery"
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          context
          |> assert_consistent_and_not_failing("""
            SELECT
              output
            FROM (
              SELECT
                user_id,
                #{on_column(unquote(function), unquote(column))} as output
              FROM #{unquote(table)}
              ORDER BY 1, 2
            ) table_alias
            ORDER BY output
          """)
        end
      end)
    end
  end
)
