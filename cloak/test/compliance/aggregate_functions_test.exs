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
  defmodule Module.concat([Compliance.AggregateFunctions, String.to_atom(aggregate), Test]) do
    use ExUnit.Case, async: true

    @moduletag :exclude_in_dev
    @moduletag :compliance
    @moduletag :"#{aggregate}"
    @moduletag report: [:compliance]

    alias Compliance.Helpers
    alias Cloak.DataSource.MongoDB

    setup_all do
      {:ok, data_sources: Helpers.data_sources()}
    end

    Enum.each(Helpers.numerical_columns(), fn({column, table, uid}) ->

      if allowed_in_subquery do
        @tag compliance: "#{aggregate} #{column} #{table} subquery"
        test "aggregate #{aggregate} on input #{column} in a sub-query on #{table}", context do
          context
          |> Helpers.disable_for(MongoDB, match?("length" <> _, unquote(column)))
          |> Helpers.assert_consistent_and_not_failing("""
            SELECT
              aggregate
            FROM (
              SELECT
                #{unquote(uid)},
                #{Helpers.on_column(unquote(aggregate), unquote(column))} as aggregate
              FROM #{unquote(table)}
              GROUP BY #{unquote(uid)}
            ) table_alias
            ORDER BY aggregate
          """)
        end
      end

      @tag compliance: "#{aggregate} #{column} #{table} query"
      test "aggregate #{aggregate} on input #{column} in query on #{table}", context do
        context
        |> Helpers.disable_for(MongoDB, match?("avg" <> _, unquote(aggregate)))
        |> Helpers.disable_for(MongoDB, match?("max" <> _, unquote(aggregate)))
        |> Helpers.disable_for(MongoDB, match?("median" <> _, unquote(aggregate)))
        |> Helpers.disable_for(MongoDB, match?("min" <> _, unquote(aggregate)))
        |> Helpers.assert_consistent_and_not_failing("""
          SELECT #{Helpers.on_column(unquote(aggregate), unquote(column))}
          FROM #{unquote(table)}
        """)
      end
    end)
  end
end)
