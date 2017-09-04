defmodule Compliance.DateTimeFunctions.Test do
  use ExUnit.Case, async: true

  @moduletag :exclude_in_dev
  @moduletag :compliance

  alias Compliance.Helpers

  setup_all do
    data_sources = if System.get_env("TRAVIS") do
      Compliance.DataSources.all_from_config_initialized("compliance_travis")
    else
      Compliance.DataSources.all_from_config_initialized("compliance")
    end

    assert(length(data_sources) > 1, "More than one data source is needed to ensure compliance")

    {:ok, data_sources: data_sources}
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

      Enum.each(Helpers.datetime_columns(), fn({column, table, uid}) ->
        test "#{function} on input #{column} in a sub-query on #{table}", context do
          Helpers.assert_consistent_and_not_failing context, """
            SELECT
              output
            FROM (
              SELECT
                #{unquote(uid)},
                #{Helpers.on_column(unquote(function), unquote(column))} as output
              FROM #{unquote(table)}
              ORDER BY 1, 2
            ) table_alias
            ORDER BY output
          """
        end

        test "#{function} on input #{column} in query on #{table}", context do
          Helpers.assert_consistent_and_not_failing context, """
            SELECT #{Helpers.on_column(unquote(function), unquote(column))}
            FROM #{unquote(table)}
            ORDER BY 1
          """
        end
      end)
    end)
  end

end
