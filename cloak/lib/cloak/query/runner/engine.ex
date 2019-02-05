defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, Query}
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the SQL query and returns the query result with info messages or the corresponding error."
  @spec run(Cloak.Query.Runner.args()) :: Cloak.Query.Runner.result()
  def run(runner_args) do
    {query_killer_reg, query_killer_unreg} = runner_args.memory_callbacks

    start_time = :erlang.monotonic_time(:milli_seconds)

    query =
      runner_args.statement
      |> parse!(runner_args.state_updater)
      |> compile!(runner_args)
      |> Query.DbEmulator.compile()

    features = Sql.Query.features(query)

    runner_args.feature_updater.(features)
    query_killer_reg.()
    result = query |> run_statement(runner_args.state_updater) |> Query.Result.new(query.column_titles, features)
    query_killer_unreg.()

    runtime = :erlang.monotonic_time(:milli_seconds) - start_time

    query =
      Sql.Query.add_debug_info(query, [
        "Query executed in #{runtime / 1000} seconds."
        | if(query.emulated?, do: ["Query was partially emulated."], else: [])
      ])

    {:ok, result, Sql.Query.info_messages(query)}
  rescue
    e in Cloak.Query.ExecutionError ->
      {:error, e.message}

    e in [Cloak.Sql.CompilationError, Cloak.Sql.Parser.ParseError] ->
      {:error, Cloak.Sql.ErrorFormat.format(runner_args.statement, e)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parse!(statement, state_updater) do
    state_updater.(:parsing)
    Sql.Parser.parse!(statement)
  end

  defp compile!(parsed_query, runner_args) do
    runner_args.state_updater.(:compiling)

    Sql.Compiler.compile!(
      parsed_query,
      runner_args.analyst_id,
      runner_args.data_source,
      runner_args.parameters,
      runner_args.views
    )
  end

  defp display_content_type(:private), do: "personal"
  defp display_content_type(:public), do: "non-personal"

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query, _state_updater) do
    tables =
      query.data_source.tables
      |> Enum.to_list()
      |> Enum.map(fn {name, table} -> [to_string(name), display_content_type(table.content_type)] end)

    views = query.views |> Map.keys() |> Enum.map(&[to_string(&1), "view"])

    Enum.map(tables ++ views, &%{occurrences: 1, row: &1})
  end

  defp run_statement(%Sql.Query{command: :show, show: :columns} = query, _state_updater) do
    [table] = query.selected_tables

    Enum.map(
      table.columns,
      &%{
        occurrences: 1,
        row: [&1.name, to_string(&1.type), isolator_status(query.data_source, table, &1.name), table.keys[&1.name]]
      }
    )
  end

  defp run_statement(%Sql.Query{command: :select} = query, state_updater),
    do: Query.DbEmulator.select(query, state_updater)

  defp isolator_status(_data_source, _view = %{db_name: nil, query: nil}, _column), do: nil

  defp isolator_status(data_source, table, column) do
    case Cloak.DataSource.Isolators.cache_lookup(data_source, table.name, column) do
      {:ok, result} -> to_string(result)
      {:error, status} -> to_string(status)
    end
  end
end
