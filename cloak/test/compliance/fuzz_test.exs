defmodule Cloak.Compliance.FuzzTest do
  use ComplianceCase, async: true

  alias Cloak.Compliance.QueryGenerator

  @tag :fuzz
  test "running a generated query", context = %{data_sources: [%{tables: tables} | _other_sources]} do
    for _ <- 1..200 do
      query = tables |> Map.values() |> QueryGenerator.generate_ast() |> QueryGenerator.ast_to_sql() |> to_string()
      assert_consistent_and_not_failing(context, query)
    end
  end
end
