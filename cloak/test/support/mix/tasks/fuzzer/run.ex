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
            fn {query, seed} ->
              IO.write(".")
              run_query(query, seed, data_sources)
            end,
            max_concurrency: concurrency,
            timeout: timeout,
            ordered: true,
            on_timeout: :kill_task
          )
          |> Stream.zip(queries)
          |> Stream.map(&normalize_result/1)
          |> Enum.map(&print_single_result(all_file, crashes_file, &1))
        end)
      end)

    IO.puts("\n")

    print_stats(results, options)
  end

  defp do_run(%{seed: seed}) do
    initialize()
    data_sources = [%{tables: tables} | _] = ComplianceCase.data_sources()

    seed
    |> QueryGenerator.ast_from_seed(Map.values(tables))
    |> run_query(seed, data_sources)
    |> print_result()
  end

  defp do_run(_), do: print_usage!()

  defp normalize_result({{:ok, result}, _}), do: result

  defp normalize_result({{:exit, :timeout}, {query, seed}}),
    do: %{query: QueryGenerator.ast_to_sql(query), seed: seed, result: :timeout}

  defp print_single_result(all_file, crashes_file, result) do
    print_result(all_file, result)

    if result.result != :ok do
      print_result(crashes_file, result)
    end

    result
  end

  defp print_result(device \\ :stdio, result)

  defp print_result(device, %{result: :error, query: query, seed: seed, minimized: minimized, error: error}) do
    IO.puts(device, [
      "Query:\n\n",
      query,
      "\n\nMinimized query:\n\n",
      minimized,
      "\n\nSeed: ",
      inspect(seed),
      "\n\n",
      error,
      "\n\n"
    ])
  end

  defp print_result(device, %{result: result, query: query, seed: seed}) do
    IO.puts(device, [query, "\n\n", "Seed: ", inspect(seed), "\n\n", inspect(result), "\n\n"])
  end

  defp print_stats(results, options) do
    stats_path = Map.get(options, :stats_out, "/tmp/stats.txt")

    with_file(stats_path, fn file ->
      results
      |> Enum.group_by(fn %{result: result} -> result end)
      |> Enum.map(fn {result, items} -> {result, Enum.count(items)} end)
      |> Enum.sort_by(fn {_, count} -> count end, &Kernel.>/2)
      |> Enum.each(fn {result, count} ->
        IO.puts(file, [to_string(result), ": ", to_string(count)])
      end)
    end)
  end

  defp run_query(ast, seed, data_sources) do
    case do_run_query(ast, data_sources) do
      result = %{result: :ok} -> result
      result -> Map.put(result, :minimized, minimize(ast, result, data_sources))
    end
    |> Map.put(:seed, seed)
  end

  defp minimize(ast, result, data_sources) do
    ast
    |> QueryGenerator.minimize(fn ast ->
      other_result = do_run_query(ast, data_sources)
      other_result.result == result.result and first_line(other_result.error) == first_line(result.error)
    end)
    |> QueryGenerator.ast_to_sql()
  end

  defp first_line(string), do: string |> String.split("\n") |> hd()

  defp do_run_query(ast, data_sources) do
    query = QueryGenerator.ast_to_sql(ast)

    case assert_query_consistency(query, data_sources: data_sources) do
      %{error: error} -> %{query: query, result: :error, error: error}
      %{rows: _} -> %{query: query, result: :ok}
    end
  rescue
    e -> %{query: QueryGenerator.ast_to_sql(ast), result: :error, error: e.message}
  end

  defp generate_queries(tables, number_of_queries) do
    for _ <- 1..number_of_queries do
      QueryGenerator.ast_with_seed(tables, _complexity = 100)
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
