defmodule Cloak.Compliance.FuzzTest do
  use ComplianceCase, async: true

  alias Cloak.Compliance.QueryGenerator
  import Cloak.Test.QueryHelpers

  @tag :fuzz
  test "running a generated query", context = %{data_sources: [%{tables: tables} | _other_sources]} do
    for _ <- 1..200 do
      query = tables |> Map.values() |> QueryGenerator.generate_ast() |> QueryGenerator.ast_to_sql() |> to_string()
      assert_consistent_or_failing_nicely(context, query)
    end
  end

  defp assert_consistent_or_failing_nicely(context, query) do
    case assert_query_consistency(query, data_sources: context.data_sources) do
      %{error: error} ->
        if nice_error?(error) do
          error
        else
          raise ExUnit.AssertionError,
            message: "Query execution failed. Query was:\n#{query}.\n\nError:\n#{error}"
        end
      result -> result
    end
  end

  defp nice_error?(error) do
    cond do
      error =~ ~r/`HAVING` clause can not be applied over column/ -> true
      true -> false
    end
  end
end
