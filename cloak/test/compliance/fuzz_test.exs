defmodule Cloak.Compliance.FuzzTest do
  use ComplianceCase, async: true

  alias Cloak.Compliance.QueryGenerator

  @tag :fuzz
  test "running a generated query", context = %{data_sources: [%{tables: tables} | _other_sources]} do
    query = QueryGenerator.generate_ast(tables) |> QueryGenerator.ast_to_sql() |> to_string()
    assert_consistent_and_not_failing(context, query)
  end
end
