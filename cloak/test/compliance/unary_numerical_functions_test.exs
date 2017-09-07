defmodule Compliance.UnaryNumericalFunctions.Test do
  use ExUnit.Case, async: true

  @moduletag :exclude_in_dev
  @moduletag :compliance

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

      Enum.each(Helpers.numerical_columns(), fn({column, table, uid}) ->

        test "numerical unary function #{function} on input #{column} in a sub-query on #{table}", context do
          context
          |> Helpers.disable_for(MongoDB, match?("abs" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("ceil" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("floor" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("round" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("sqrt" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("trunc" <> _, unquote(function)))
          |> Helpers.assert_consistent_and_not_failing("""
            SELECT
              output
            FROM (
              SELECT
                #{unquote(uid)},
                #{Helpers.on_column(unquote(function), unquote(column))} as output
              FROM #{unquote(table)}
            ) table_alias
            ORDER BY output
          """)
        end

        test "numerical unary function #{function} on input #{column} in query on #{table}", context do
          context
          |> Helpers.disable_for(MongoDB, match?("abs" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("ceil" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("floor" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("round" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("sqrt" <> _, unquote(function)))
          |> Helpers.disable_for(MongoDB, match?("trunc" <> _, unquote(function)))
          |> Helpers.assert_consistent_and_not_failing("""
            SELECT #{Helpers.on_column(unquote(function), unquote(column))} as output
            FROM #{unquote(table)}
            ORDER BY output
          """)
        end
      end)
    end)
  end
end
