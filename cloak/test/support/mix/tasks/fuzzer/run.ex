defmodule Mix.Tasks.Fuzzer.Run do
  @shortdoc "Runs the fuzzer."
  @usage """
    Usage:

      mix fuzzer.run --queries N

      --queries specifies how many queries to run.
      --seed add this option instead of --queries to run a query from the specified seed (see output)
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
    timeout: :integer,
    seed: :string
  ]

  @impl Mix.Task
  def run(args) do
    with {options, [], []} <- OptionParser.parse(args, strict: @option_spec) do
      do_run(Enum.into(options, %{}))
    else
      _ -> print_usage!()
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_run(options = %{queries: number_of_queries}) do
    initialize()

    data_sources = [%{tables: tables} | _] = ComplianceCase.data_sources()
    concurrency = Map.get(options, :concurrency, System.schedulers_online())
    timeout = Map.get(options, :timeout, :timer.seconds(30))

    queries = generate_queries(Map.values(tables), number_of_queries)

    all_path = Map.get(options, :all_out, "/tmp/all.txt")
    crashes_path = Map.get(options, :crashes_out, "/tmp/crashes.txt")

    results =
      with_file(all_path, fn all_file ->
        with_file(crashes_path, fn crashes_file ->
          Task.async_stream(
            queries,
            fn {query, _} ->
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

  defp do_run(%{seed: seed}) do
    initialize()
    data_sources = [%{tables: tables} | _] = ComplianceCase.data_sources()
    ast = QueryGenerator.ast_from_seed(seed, Map.values(tables))
    query = QueryGenerator.ast_to_sql(ast)

    case run_query(query, data_sources) do
      %{result: :ok} ->
        IO.puts([query, "\n\n", "Seed: ", inspect(seed), "\n\nok\n\n"])

      result ->
        IO.puts([
          "Query:\n\n",
          query,
          "\n\nMinimized query:\n\n",
          minimize(ast, result, data_sources),
          "\n\nSeed: ",
          inspect(seed),
          "\n\n",
          Exception.format(:error, result.error)
        ])
    end
  end

  defp do_run(_), do: print_usage!()

  defp minimize(ast, result, data_sources) do
    ast
    |> QueryGenerator.minimize(fn ast ->
      other_result = ast |> QueryGenerator.ast_to_sql() |> run_query(data_sources)

      other_result.result == result.result and
        first_line(other_result.error.message) == first_line(result.error.message)
    end)
    |> QueryGenerator.ast_to_sql()
  end

  defp first_line(string), do: string |> String.split("\n") |> hd()

  defp normalize_result({:ok, result}), do: result
  defp normalize_result({:exit, :timeout}), do: %{result: :timeout, error: nil}

  defp print_single_result(all_file, crashes_file, item = {{query, seed}, result}) do
    IO.puts(all_file, [query, "\n\n", "Seed: ", inspect(seed), "\n\n", to_string(result.result), "\n\n"])

    if result.result == :unexpected_error do
      IO.puts(crashes_file, [query, "\n\n", "Seed: ", inspect(seed), "\n\n", Exception.format(:error, result.error)])
    end

    item
  end

  defp print_stats(results, options) do
    stats_path = Map.get(options, :stats_out, "/tmp/stats.txt")

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

  defp generate_queries(tables, number_of_queries) do
    for _ <- 1..number_of_queries do
      {ast, seed} = QueryGenerator.ast_with_seed(tables, _complexity = 100)

      {QueryGenerator.ast_to_sql(ast), seed}
    end
  end

  defp assert_consistent_or_failing_nicely(data_sources, query) do
    case assert_query_consistency(query, data_sources: data_sources) do
      %{error: error} -> raise error
      %{rows: _} -> :ok
    end
  end

  defp initialize() do
    Application.ensure_all_started(:cloak)
    Cloak.SapHanaHelpers.delete_test_schemas()
    Cloak.Test.DB.start_link()
  end

  defp with_file(name, function) do
    file = File.open!(name, [:write, :utf8])
    function.(file)
  end

  defp print_usage!() do
    IO.puts(@usage)
    Mix.raise("Invalid usage")
  end
end
