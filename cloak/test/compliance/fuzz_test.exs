defmodule Cloak.Compliance.FuzzTest do
  use ComplianceCase, async: true

  alias Cloak.Compliance.QueryGenerator
  import Cloak.Test.QueryHelpers

  @tag :fuzz
  test "running a generated query", context = %{data_sources: [%{tables: tables} | _other_sources]} do
    results = Enum.reduce(1..100, %{}, fn(_, results) ->
      query = tables |> Map.values() |> QueryGenerator.generate_ast() |> QueryGenerator.ast_to_sql() |> to_string()
      IO.inspect(query)
      result = assert_consistent_or_failing_nicely(context, query)
      IO.inspect(result)
      results = Map.update(results, result, 1, & &1 + 1)
    end)

    IO.inspect(results)
  end

  defp assert_consistent_or_failing_nicely(context, query) do
    case assert_query_consistency(query, data_sources: context.data_sources) do
      %{error: error} ->
        if nice_error?(error), do: :error, else: :crash
      %{rows: _} -> :ok
    end
  end

  defp nice_error?(error) do
    cond do
      error =~ ~r/`HAVING` clause can not be applied over column/ -> true
      error =~ ~r/Inequalities on string values are currently not supported/ -> true
      error =~ ~r/must be limited to a finite, nonempty range/ -> true
      error =~ ~r/needs to appear in the `GROUP BY` clause/ -> true
      true -> false
    end
  end
end
