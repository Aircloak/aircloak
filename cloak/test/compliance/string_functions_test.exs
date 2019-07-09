Enum.each(
  [
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
    "upper(<col>)"
  ],
  fn function ->
    defmodule Module.concat([Compliance.StringFunctions, String.to_atom(function), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{function}"

      Enum.each(text_columns(), fn {column, table, uid} ->
        @tag compliance: "#{function} #{column} #{table} subquery"
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          context
          |> disable_for(Cloak.DataSource.SQLServer, match?("hex" <> _, unquote(function)))
          |> disable_unicode(unquote(function), unquote(column))
          |> assert_consistent_and_not_failing("""
            SELECT
              output,
              COUNT(*),
              MEDIAN(1)
            FROM (
              SELECT
                #{unquote(uid)},
                #{on_column(unquote(function), "\"#{unquote(column)}\"")} as output
              FROM #{unquote(table)}
              ORDER BY 1, 2
            ) table_alias
            GROUP BY output
            ORDER BY output
          """)
        end
      end)

      defp disable_unicode(context, function, column) do
        if column == "name" do
          context
          |> disable_for(Cloak.DataSource.MongoDB, String.starts_with?(function, ~w(lower lcase upper ucase)))
          |> disable_for(Cloak.DataSource.SQLServer, String.starts_with?(function, ~w(lower lcase upper ucase)))
          |> disable_for(Cloak.DataSource.Oracle, function in ["substring(<col> FROM 10 FOR 10)", "right(<col>, 10)"])
        else
          context
        end
      end
    end
  end
)
