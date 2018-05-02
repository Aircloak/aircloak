Enum.each(
  [
    "day(<col>)",
    "hour(<col>)",
    "minute(<col>)",
    "month(<col>)",
    "quarter(<col>)",
    "second(<col>)",
    "year(<col>)",
    "weekday(<col>)",
    "date_trunc('year', <col>)",
    "date_trunc('quarter', <col>)",
    "date_trunc('month', <col>)",
    "date_trunc('day', <col>)",
    "date_trunc('hour', <col>)",
    "date_trunc('minute', <col>)",
    "date_trunc('second', <col>)",
    "<col> + interval 'P1Y'",
    "<col> - interval 'P1M'",
    "<col> - (<col> - interval 'P1D')",
    "<col> + 2 * interval 'P1Y'",
    "interval 'P1DT1H'"
  ],
  fn function ->
    defmodule Module.concat([Compliance.DateTimeFunctions, String.to_atom(function), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{function}"

      Enum.each(datetime_columns(), fn {column, table, uid} ->
        @tag compliance: "#{function} #{column} #{table} subquery"
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          context
          |> disable_for(:all, match?("weekday" <> _, unquote(function)))
          |> disable_subquery_interval(unquote(function))
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
          |> assert_consistent_and_not_failing("""
            SELECT #{on_column(unquote(function), unquote(column))}
            FROM #{unquote(table)}
            ORDER BY 1
          """)
        end
      end)

      defp disable_subquery_interval(context, function) do
        if function =~ ~r/<col> (\+|-) .*interval/ do
          # See #2634 (https://github.com/Aircloak/aircloak/issues/2634) for details.
          context
          |> disable_for(Cloak.DataSource.MySQL, true)
          |> disable_for(Cloak.DataSource.SQLServer, true)
          |> disable_for(Cloak.DataSource.SQLServerTds, true)
          |> disable_for(Cloak.DataSource.SQLServerRODBC, true)
          |> disable_for(Cloak.DataSource.SAPHana, true)
          |> disable_for(Cloak.DataSource.MongoDB, true)
        else
          context
        end
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

      Enum.each(datetime_columns(), fn {column, table, uid} ->
        @tag compliance: "#{condition} in where #{column} #{table} query"
        test "#{condition} on input #{column} in where in query on table #{table}", context do
          context
          |> assert_consistent_and_not_failing("""
            SELECT COUNT(*)
            FROM #{unquote(table)}
            WHERE #{on_column(unquote(condition), unquote(column))}
          """)
        end

        @tag compliance: "#{condition} in where #{column} #{table} subquery"
        test "#{condition} on input #{column} in where in subquery on table #{table}", context do
          context
          |> assert_consistent_and_not_failing("""
            SELECT COUNT(*)
            FROM (
              SELECT #{unquote(uid)}
              FROM #{unquote(table)}
              WHERE #{on_column(unquote(condition), unquote(column))}
            ) foo
          """)
        end
      end)
    end
  end
)
