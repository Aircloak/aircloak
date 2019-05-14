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
      --no-minimization turns off failed query minimization
  """

  @moduledoc """
  Run many randomly-generated queries in compliance mode (asserting that results from all data sources are the same).

  #{@usage}
  """

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  alias Cloak.Compliance.QueryGenerator
  import Cloak.Test.QueryHelpers
  alias Cloak.Test.AnalystTableHelpers

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
    seed: :string,
    no_minimization: :boolean
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

    concurrency = Map.get(options, :concurrency, System.schedulers_online())
    timeout = Map.get(options, :timeout, :timer.seconds(30))

    queries = generate_queries(get_tables(), number_of_queries)

    all_path = Map.get(options, :all_out, "/tmp/all.txt")
    crashes_path = Map.get(options, :crashes_out, "/tmp/crashes.txt")

    results =
      with_file(all_path, fn all_file ->
        with_file(crashes_path, fn crashes_file ->
          Task.async_stream(
            queries,
            fn {query, seed} ->
              IO.write(".")
              run_query(query, seed, ComplianceCase.data_sources(), !options[:no_minimization])
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

  defp do_run(options = %{seed: seed}) do
    initialize()
    data_sources = ComplianceCase.data_sources()

    for data_source <- data_sources do
      AnalystTableHelpers.clear_analyst_tables(data_source)
    end

    seed
    |> QueryGenerator.ast_from_seed(get_tables())
    |> run_query(seed, data_sources, !options[:no_minimization])
    |> print_result()
  end

  defp do_run(_), do: print_usage!()

  defp normalize_result({{:ok, result}, _}), do: result

  defp normalize_result({{:exit, :timeout}, {query, seed}}),
    do: %{query: query, seed: seed, result: :timeout}

  defp print_single_result(all_file, crashes_file, result) do
    print_result(all_file, result)

    if result.result != :ok do
      print_result(crashes_file, result)
    end

    result
  end

  defp print_result(device \\ :stdio, result)

  defp print_result(device, result = %{result: :error, query: query, seed: seed, error: error}) do
    IO.puts(device, [
      "\n\n -- QUERY\n\n",
      query(query),
      minimized_query(result),
      "\n\n -- Seed: ",
      inspect(seed),
      "\n\n",
      error
    ])
  end

  defp print_result(device, %{result: result, query: query, seed: seed}) do
    IO.puts(device, [
      "\n\n -- QUERY\n\n",
      query(query),
      "\n\n",
      " -- Seed: ",
      inspect(seed),
      "\n\n",
      inspect(result)
    ])
  end

  defp minimized_query(%{minimized: ast}), do: ["\n\n -- MINIMIZED QUERY\n\n", query(ast)]
  defp minimized_query(_), do: ""

  defp query(ast) do
    case QueryGenerator.extract_analyst_tables(ast) do
      {ast, []} ->
        QueryGenerator.ast_to_sql(ast)

      {ast, analyst_tables} ->
        [
          QueryGenerator.ast_to_sql(ast),
          "\n\n -- Analyst tables:\n\n",
          analyst_tables |> Enum.map(fn {name, ast} -> [" -- ", name, "\n", QueryGenerator.ast_to_sql(ast), "\n"] end)
        ]
    end
  end

  defp print_stats(results, options) do
    stats_path = Map.get(options, :stats_out, "/tmp/stats.txt")

    with_file(stats_path, fn file ->
      results
      |> Enum.group_by(fn %{result: result} -> result end)
      |> Enum.map(fn {result, items} -> {result, Enum.count(items)} end)
      |> Enum.sort_by(fn {_, count} -> count end, &Kernel.>/2)
      |> Enum.each(fn {result, count} ->
        IO.puts([to_string(result), ": ", to_string(count)])
        IO.puts(file, [to_string(result), ": ", to_string(count)])
      end)
    end)
  end

  defp run_query(ast, seed, data_sources, minimize?) do
    case do_run_query(ast, data_sources) do
      result = %{result: :ok} -> result
      result -> if minimize?, do: Map.put(result, :minimized, minimize(ast, result, data_sources)), else: result
    end
    |> Map.put(:seed, seed)
  end

  defp minimize(ast, result, data_sources) do
    QueryGenerator.minimize(ast, fn ast ->
      other_result = do_run_query(ast, data_sources)
      other_result.result == result.result and first_line(other_result.error) == first_line(result.error)
    end)
  end

  defp first_line(string), do: string |> String.split("\n") |> hd()

  defp do_run_query(input_ast, data_sources) do
    {ast, analyst_tables} = QueryGenerator.extract_analyst_tables(input_ast)
    query = QueryGenerator.ast_to_sql(ast)

    try do
      create_analyst_tables!(analyst_tables, data_sources)

      case {assert_query_consistency(query, analyst_id: 1, data_sources: data_sources), analyst_tables} do
        {%{error: error}, _} ->
          %{result: :error, error: error}

        {%{rows: _}, []} ->
          %{result: :ok}

        {%{rows: rows}, _} ->
          no_analyst = input_ast |> QueryGenerator.remove_analyst_tables() |> QueryGenerator.ast_to_sql()

          case assert_query_consistency(no_analyst, data_sources: data_sources) do
            %{error: error} -> %{result: :error, error: "Query failed without analyst tables\n#{error}"}
            %{rows: ^rows} -> %{result: :ok}
            _ -> %{result: :error, error: "Inconsistent results with analyst tables"}
          end
      end
    rescue
      e -> %{result: :error, error: Map.get(e, :message, inspect(e))}
    end
    |> Map.put(:query, input_ast)
  end

  defp create_analyst_tables!(analyst_tables, data_sources) do
    for {name, ast} <- analyst_tables, data_source <- data_sources do
      case AnalystTableHelpers.create_or_update(1, name, QueryGenerator.ast_to_sql(ast), data_source) do
        {:ok, _} -> :ok
        {:error, message} -> raise message
      end
    end
  end

  defp generate_queries(tables, number_of_queries) do
    for _ <- 1..number_of_queries do
      QueryGenerator.ast_with_seed(tables, _complexity = 100)
    end
  end

  defp initialize() do
    anonymizer_config = Application.get_env(:cloak, :anonymizer)

    Application.put_env(
      :cloak,
      :anonymizer,
      anonymizer_config
      |> Keyword.put(:outliers_count, {2, 4, 0.5})
      |> Keyword.put(:low_count_soft_lower_bound, {5, 1})
      |> Keyword.put(:sum_noise_sigma, 1)
      |> Keyword.put(:sum_noise_sigma_scale_params, {1, 0.5})
    )

    Application.ensure_all_started(:cloak)
    Cloak.Test.DB.start_link()
    Cloak.Air.register_air("test_air")
  end

  defp with_file(name, function) do
    file = File.open!(name, [:write, :utf8])
    function.(file)
  end

  defp print_usage!() do
    IO.puts(@usage)
    Mix.raise("Invalid usage")
  end

  defp get_tables() do
    [%{tables: tables} | _] = ComplianceCase.data_sources()
    tables |> Map.values() |> Enum.filter(& &1.user_id)
  end
end
