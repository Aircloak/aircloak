defmodule Cloak.Compliance.FuzzTest do
  use ComplianceCase, async: true

  alias Cloak.Compliance.QueryGenerator
  import Cloak.Test.QueryHelpers

  @tag :fuzz
  test "running a generated query", context = %{data_sources: [%{tables: tables} | _other_sources]} do
    results = Enum.reduce(1..100, %{}, fn(_, results) ->
      query = tables |> Map.values() |> QueryGenerator.generate_ast() |> QueryGenerator.ast_to_sql() |> to_string()
      IO.puts(query)
      result = assert_consistent_or_failing_nicely(context, query)
      IO.inspect(result)
      Map.update(results, result, 1, & &1 + 1)
    end)

    IO.inspect(results)
  end

  defp assert_consistent_or_failing_nicely(context, query) do
    case assert_query_consistency(query, data_sources: context.data_sources) do
      %{error: error} -> error_type(error)
      %{rows: _} -> :ok
    end
  end

  defp error_type(error) do
    cond do
      error =~ ~r/`HAVING` clause can not be applied over column/ -> :illegal_having
      error =~ ~r/Inequalities on string values are currently not supported/ -> :string_inequality
      error =~ ~r/must be limited to a finite, nonempty range/ -> :incorrect_range
      error =~ ~r/needs to appear in the `GROUP BY` clause/ -> :missing_group_by
      error =~ ~r/Missing a user id column in the select list of subquery/ -> :subquery_no_uid
      error =~ ~r/There is no user id column in the subquery/ -> :subquery_no_uid
      error =~ ~r/Missing where comparison for uid columns/ -> :join_no_uid
      error =~ ~r/Combining conditions with `OR` is not allowed/ -> :or_used
      true -> raise error
    end
  end
end
