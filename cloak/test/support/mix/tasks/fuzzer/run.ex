defmodule Mix.Tasks.Fuzzer.Run do
  @shortdoc "Runs the fuzzer."
  @usage """
    Usage:

      mix fuzzer.run --queries N

      --queries specifies how many queries to run.
      --all-out speciefies where to store a log with all attempted queries, defaults to /tmp/all.txt
      --stats-out specifies where to store a log with number of failures by reason, defaults to /tmp/stats.txt
      --crashes-out specifies where to store a log with unexpected errors, defaults to /tmp/crashes.txt
      --concurrency specifies how many concurrent queries to run, defaults to System.schedulers_online()
      --timeout specifies the timeout per query in miliseconds, defaults to 30000
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
    timeout: :integer
  ]

  @impl Mix.Task
  def run(args) do
    with {options, [], []} <- OptionParser.parse(args, strict: @option_spec),
         {:ok, queries} <- Keyword.fetch(options, :queries) do
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

  defp do_run(number_of_queries, options) do
    initialize()

    data_sources = [%{tables: tables} | _] = ComplianceCase.data_sources()
    concurrency = Keyword.get(options, :concurrency, System.schedulers_online())
    timeout = Keyword.get(options, :timeout, :timer.seconds(30))

    queries = generate_queries(tables, number_of_queries)

    all_path = Keyword.get(options, :all_out, "/tmp/all.txt")
    crashes_path = Keyword.get(options, :crashes_out, "/tmp/crashes.txt")

    results =
      with_file(all_path, fn all_file ->
        with_file(crashes_path, fn crashes_file ->
          Task.async_stream(
            queries,
            fn query ->
              IO.write(".")
              run_query(query, data_sources)
            end,
            max_concurrency: concurrency,
            timeout: timeout,
            ordered: true,
            on_timeout: :kill_task
          )
          |> Stream.map(&normalize_result/1)
          |> Stream.zip(queries)
          |> Stream.map(fn {result, query} -> {query, result} end)
          |> Enum.map(&print_single_result(all_file, crashes_file, &1))
        end)
      end)

    IO.puts("\n")

    print_stats(results, options)
  end

  defp normalize_result({:ok, result}), do: result
  defp normalize_result({:exit, :timeout}), do: %{result: :timeout, error: nil}

  defp print_single_result(all_file, crashes_file, item = {query, result}) do
    IO.puts(all_file, [query, "\n\n", to_string(result.result), "\n\n"])

    if result.result == :unexpected_error do
      IO.puts(crashes_file, [query, "\n\n", Exception.format(:error, result.error)])
    end

    item
  end

  defp print_stats(results, options) do
    stats_path = Keyword.get(options, :stats_out, "/tmp/stats.txt")

    with_file(stats_path, fn file ->
      results
      |> Enum.group_by(fn {_, %{result: result}} -> result end)
      |> Enum.map(fn {result, items} -> {result, Enum.count(items)} end)
      |> Enum.sort_by(fn {_, count} -> count end, &Kernel.>/2)
      |> Enum.each(fn {result, count} ->
        IO.puts(file, [to_string(result), ": ", to_string(count)])
      end)
    end)
  end

  defp run_query(query, data_sources) do
    result = assert_consistent_or_failing_nicely(data_sources, query)
    %{result: result, error: nil}
  rescue
    e -> %{result: :unexpected_error, error: e}
  end

  defp generate_queries(tables, number_of_queries),
    do:
      tables
      |> Map.values()
      |> QueryGenerator.ast_generator()
      |> Enum.take(number_of_queries)
      |> Enum.map(&QueryGenerator.ast_to_sql/1)

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
      error =~ ~r/Function .* requires arguments of type/ -> :mistyped_function
      error =~ ~r/Function .* is allowed over arguments/ -> :restricted_aggregate
      error =~ ~r/Function .* is not allowed in subqueries/ -> :restricted_aggregate
      error =~ ~r/Table alias .* used more than once/ -> :duplicate_alias
      error =~ ~r/Non-integer constant is not allowed in `GROUP BY`/ -> :non_integer_group_by
      error =~ ~r/`GROUP BY` position .* is out of the range of selected columns./ -> :invalid_group_by_position
      error =~ ~r/Functions .* could cause a database exception/ -> :possible_db_exception
      error =~ ~r/Row splitter functions used in the `WHERE`-clause have/ -> :restricted_row_splitter
      error =~ ~r/String manipulation functions cannot be combined with other transformations/ -> :string_manipulation
      error =~ ~r/Expressions with NOT ILIKE cannot include any functions/ -> :restricted_like
      error =~ ~r/Range expressions cannot include any functions except aggregations and a cast/ -> :restricted_range
      error =~ ~r/Aggregate function .* can not be used in the `GROUP BY` clause/ -> :aggregate_in_group_by
      error =~ ~r/Usage of .* is ambiguous/ -> :ambiguous_identifier
      error =~ ~r/Column .* is ambiguous/ -> :ambiguous_identifier
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
