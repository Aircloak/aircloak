if Mix.env == :test do
  defmodule Mix.Tasks.Fuzzer.Run do
    @shortdoc "Runs the fuzzer."
    @usage """
      Usage:

        mix fuzzer.run --queries N [--all-out path] [--stats-out path] [--crashes-out path]

        --queries specifies how many queries to run.
        --all-out speciefies where to store a log with all attempted queries, defaults to /tmp/all.txt
        --stats-out specifies where to store a log with number of failures by reason, defaults to /tmp/stats.txt
        --crashes-out specifies where to store a log with unexpected errors, defaults to /tmp/crashes.txt
    """

    @moduledoc """
    Run many randomly-generated queries in compliance mode (asserting that results from all data sources are the same).

    #{@usage}
    """

    # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
    @dialyzer :no_undefined_callbacks

    alias Cloak.Compliance.QueryGenerator
    import Cloak.Test.QueryHelpers

    use Mix.Task


    # -------------------------------------------------------------------
    # Mix task interface
    # -------------------------------------------------------------------

    @option_spec [
      queries: :integer,
      all_out: :string,
      stats_out: :string,
      crashes_out: :string,
      concurrency: :integer,
      timeout: :integer,
    ]

    @impl Mix.Task
    def run(args) do
      with \
        {options, [], []} <- OptionParser.parse(args, strict: @option_spec),
        {:ok, queries} <- Keyword.fetch(options, :queries)
      do
        do_run(queries, options)
      else
        _ ->
          IO.puts(@usage)
          Mix.raise("Invalid usage")
      end
    end


    # -------------------------------------------------------------------
    # Internal functions
    # -------------------------------------------------------------------

    defp do_run(queries, options) do
      initialize()
      data_sources = ComplianceCase.data_sources()
      concurrency = Keyword.get(options, :concurrency, System.schedulers_online())
      timeout = Keyword.get(options, :timeout, :timer.seconds(30))

      results = Task.async_stream(1..queries, fn(_) ->
        IO.write(".")
        run_one_query(data_sources)
      end, max_concurrency: concurrency, timeout: timeout)
      |> Enum.into([])
      IO.puts("\n")

      print_results(results, options)
    end

    defp print_results(results, options) do
      all_path = Keyword.get(options, :all_out, "/tmp/all.txt")
      with_file(all_path, fn(file) ->
        for %{query: query, result: result} <- results do
          IO.puts(file, [query, "\n\n", to_string(result), "\n\n"])
        end
      end)

      stats_path = Keyword.get(options, :stats_out, "/tmp/stats.txt")
      with_file(stats_path, fn(file) ->
        results
        |> Enum.group_by(fn(%{result: result}) -> result end)
        |> Enum.map(fn({result, items}) -> {result, Enum.count(items)} end)
        |> Enum.sort_by(fn({_, count}) -> count end, &Kernel.>/2)
        |> Enum.each(fn({result, count}) -> IO.puts(file, [to_string(result), ": ", to_string(count)]) end)
      end)

      crashes_path = Keyword.get(options, :crashes_out, "/tmp/crashes.txt")
      with_file(crashes_path, fn(file) ->
        for %{query: query, result: :unexpected_error, error: error} <- results do
          IO.puts(file, [query, "\n\n", Exception.format(:error, error)])
        end
      end)
    end

    defp run_one_query(data_sources = [%{tables: tables} | _]) do
      query = tables |> Map.values() |> QueryGenerator.generate_ast() |> QueryGenerator.ast_to_sql() |> to_string()

      try do
        result = assert_consistent_or_failing_nicely(data_sources, query)
        %{query: query, result: result, error: nil}
      rescue
        e -> %{query: query, result: :unexpected_error, error: e}
      end
    end

    defp assert_consistent_or_failing_nicely(data_sources, query) do
      case assert_query_consistency(query, data_sources: data_sources) do
        %{error: error} -> error_type(error)
        %{rows: _} -> :ok
      end
    end

    @lint {Credo.Check.Refactor.CyclomaticComplexity, false}
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

    defp initialize() do
      Application.ensure_all_started(:cloak)
      Cloak.SapHanaHelpers.delete_test_schemas()
      Cloak.Test.DB.start_link()
    end

    defp with_file(name, function) do
      file = File.open!(name, [:write])
      function.(file)
    end
  end
end
