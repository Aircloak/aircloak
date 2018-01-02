defmodule Mix.Tasks.Fuzzer.Run do
  @shortdoc "Runs the fuzzer."
  @usage """
    Usage:

      mix fuzzer.run --queries N

      Use the --queries option to specify how many queries to run.
  """

  @moduledoc "#{@shortdoc}\n\n#{@usage}"

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  alias Cloak.Compliance.QueryGenerator
  import Cloak.Test.QueryHelpers

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    with \
      {options, [], []} <- OptionParser.parse(args, strict: [queries: :integer]),
      {:ok, queries} <- Keyword.fetch(options, :queries)
    do
      do_run(queries)
    else
      _ ->
        IO.puts(@usage)
        Mix.raise("Invalid usage")
    end
  end

  defp do_run(queries) do
    initialize()
    data_sources = [%{tables: tables} | _ ] = data_sources()

    results = Enum.reduce(1..queries, %{}, fn(_, results) ->
      query = tables |> Map.values() |> QueryGenerator.generate_ast() |> QueryGenerator.ast_to_sql() |> to_string()
      IO.puts(query)
      result = assert_consistent_or_failing_nicely(data_sources, query)
      IO.inspect(result)
      Map.update(results, result, 1, & &1 + 1)
    end)

    IO.inspect(results)
  end

  defp assert_consistent_or_failing_nicely(data_sources, query) do
    case assert_query_consistency(query, data_sources: data_sources) do
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
      error =~ ~r/cannot be used in a.*LIKE expression/ -> :mistyped_like
      true -> raise error
    end
  end

  defp data_sources(), do: Compliance.DataSources.all_from_config_initialized("fuzzer")

  defp initialize() do
    Application.ensure_all_started(:cloak)
    Cloak.SapHanaHelpers.delete_test_schemas()
    Cloak.Test.DB.start_link()
  end
end
