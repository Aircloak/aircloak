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
    start_time = :erlang.monotonic_time(:milli_seconds)

    %{query: query, command: command, columns: columns, metadata: metadata} =
      runner_args.statement
      |> parse!(runner_args.state_updater)
      |> compile!(runner_args)

    runner_args.metadata_updater.(metadata)

    result =
      command
      |> run_statement(query, runner_args)
      |> Query.Result.new(columns, metadata)

    runtime = :erlang.monotonic_time(:milli_seconds) - start_time

    query =
      Sql.Query.add_debug_info(query, [
        "Query executed in #{runtime / 1000} seconds."
        | if(command == :select and query.emulated?, do: ["Query was partially emulated."], else: [])
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

  defp compile!(%{command: :explain} = parsed_query, runner_args) do
    %{query: query, metadata: metadata} =
      parsed_query
      |> Map.put(:command, :select)
      |> compile!(runner_args)

    %{
      command: :explain,
      query: query,
      columns: ["query plan"],
      metadata: %{metadata | selected_types: ["text"]}
    }
  end

  defp compile!(parsed_query, runner_args) do
    runner_args.state_updater.(:compiling)

    query =
      Sql.Compiler.compile!(
        parsed_query,
        runner_args.analyst_id,
        runner_args.data_source,
        runner_args.parameters,
        runner_args.user_selectables[:views] || %{}
      )
      |> Query.DbEmulator.compile()

    %{
      command: query.command,
      query: query,
      columns: query.column_titles,
      metadata: Sql.Query.metadata(query)
    }
  end

  defp display_content_type(:private), do: "personal"
  defp display_content_type(:public), do: "non-personal"

  defp run_statement(:show, %Sql.Query{show: :tables} = query, %{user_selectables: user_selectables}) do
    tables =
      Cloak.DataSource.tables(query.data_source)
      |> Enum.map(
        &[
          to_string(&1.name),
          display_content_type(&1.content_type),
          Cloak.DataSource.Table.table_comment(&1)
        ]
      )

    analyst_tables =
      Cloak.AnalystTable.cloak_tables(query.analyst_id, query.data_source, query.views)
      |> Enum.map(
        &[
          to_string(&1.name),
          display_content_type(&1.content_type),
          user_selectables
          |> get_in([:analyst_tables, to_string(&1.name)])
          |> selectable_comment()
        ]
      )

    views =
      query.views
      |> Map.keys()
      |> Enum.map(
        &[
          to_string(&1),
          "view",
          user_selectables
          |> get_in([:views, &1])
          |> selectable_comment()
        ]
      )

    Enum.map(tables ++ analyst_tables ++ views, &%{occurrences: 1, row: &1})
  end

  defp run_statement(:show, %Sql.Query{show: :columns} = query, _) do
    [table] = query.selected_tables

    table.columns
    |> Enum.reject(&(&1.access == :hidden))
    |> Enum.map(
      &%{
        occurrences: 1,
        row: [
          &1.name,
          to_string(&1.type),
          isolator_status(query.data_source, table, &1.name),
          table.keys[&1.name],
          Cloak.DataSource.Table.column_comment(table, &1.name)
        ]
      }
    )
  end

  defp run_statement(:explain, query, _),
    do:
      query
      |> Sql.Query.Explainer.explain()
      |> Sql.Query.Explainer.format_explanation()
      |> Enum.map(&%{occurrences: 1, row: [&1]})

  defp run_statement(:select, query, %{state_updater: state_updater}),
    do: Query.DbEmulator.select(query, state_updater)

  defp selectable_comment(%{comment: comment}), do: comment
  defp selectable_comment(_), do: nil

  defp isolator_status(_data_source, %{type: :subquery}, _column), do: nil
  defp isolator_status(_data_source, %{type: :analyst}, _column), do: nil

  defp isolator_status(data_source, %{type: type} = table, column) when type in [:regular, :virtual] do
    case Cloak.DataSource.Isolators.cache_lookup(data_source, table.name, column) do
      {:ok, result} -> to_string(result)
      {:error, status} -> to_string(status)
    end
  end
end
