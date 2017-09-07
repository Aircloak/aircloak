defmodule Compliance.AggregateFunctions.Test do
  use ExUnit.Case, async: true

  @moduletag :exclude_in_dev
  @moduletag :compliance
  @moduletag report: [:compliance]

  alias Compliance.Helpers
  alias Cloak.DataSource.MongoDB

  setup_all do
    data_sources = if System.get_env("TRAVIS") do
      Compliance.DataSources.all_from_config_initialized("compliance_travis")
    else
      Compliance.DataSources.all_from_config_initialized("compliance")
    end

    assert(length(data_sources) > 1, "More than one data source is needed to ensure compliance")

    {:ok, data_sources: data_sources}
  end

  describe "aggregate functions" do
    functions = [
      # {aggregate function, whether it is supported in subqueries}
      # NOTE:
      # - stddev[_noise] is missing because it crashes on values from the users table
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
    ]

    for function <- functions, column <- Helpers.numerical_columns() do
      {aggregate, allowed_in_subquery} = function
      {column, table, uid} = column

      if allowed_in_subquery do
        @tag aggregate: aggregate
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

      @tag aggregate: aggregate
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
    end
  end
end
