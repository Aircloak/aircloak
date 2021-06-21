Enum.each(
  [
    "<col> || 'text-value'",
    "'text-value' || <col>",
    "btrim(<col>)",
    "btrim(<col>, 'AESrui')",
    "concat('_', <col>, '_')",
    "hex(<col>)",
    "lcase(<col>)",
    "left(<col>, 1)",
    "left(<col>, 10)",
    "left(<col>, 1000000)",
    "left(<col>, -1)",
    "left(<col>, -10)",
    "left(<col>, -1000000)",
    "length(<col>)",
    "lower(<col>)",
    "ltrim(<col>)",
    "ltrim(<col>, 'AESrui')",
    "right(<col>, 1)",
    "right(<col>, 10)",
    "right(<col>, 1000000)",
    "right(<col>, -1)",
    "right(<col>, -10)",
    "right(<col>, -1000000)",
    "rtrim(<col>)",
    "rtrim(<col>, 'AESrui')",
    "substring(<col> FROM 1 FOR 1)",
    "substring(<col> FROM 1 FOR 1000)",
    "substring(<col> FROM 10 FOR 10)",
    "substring(<col> FROM 10 FOR 1000)",
    "substring(<col> FROM 10)",
    "substring(<col> FOR 10)",
    "substring(<col> FOR 1000)",
    "ucase(<col>)",
    "upper(<col>)"
  ],
  fn function ->
    defmodule Module.concat([Compliance.StringFunctions, String.to_atom(function), Test]) do
      use ComplianceCase, async: true

      @moduletag :"#{function}"

      Enum.each(text_columns(), fn {column, table} ->
        @tag compliance: "#{function} #{column} #{table} subquery"
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          context
          |> disable_for(:all, unquote(table) == "users_public")
          |> disable_unicode(unquote(function), unquote(column))
          |> assert_consistent_and_not_failing("""
            SELECT
              output,
              COUNT(*),
              STDDEV(1)
            FROM (
              SELECT
                user_id,
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
        if column == "name_unicode" do
          context
          |> disable_for(
            Cloak.DataSource.ClouderaImpala,
            String.starts_with?(function, ~w(length lower lcase upper ucase substring left right))
          )
          |> disable_for(Cloak.DataSource.Oracle, function in ["substring(<col> FROM 10 FOR 10)", "right(<col>, 10)"])
        else
          context
        end
      end
    end
  end
)
