defmodule Cloak.Query.DbEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """
  require Logger

  alias Cloak.{DataSource, DataSource.Table}
  alias Cloak.Sql.{Query, Expression, Function, Condition, Compiler}
  alias Cloak.Query.{DbEmulator.Selector, Rows, RowSplitters, Aggregator, Runner.ParallelProcessor, Runner.Engine}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Retrieves rows according to the specification in the compiled query. This version ignores query state updates."
  @spec select(Query.t()) :: Enumerable.t()
  def select(query), do: select(query, _state_updater = fn _ -> :ignore end)

  @doc "Retrieves rows according to the specification in the compiled query."
  @spec select(Query.t(), Engine.state_updater()) :: Enumerable.t()
  def select(query, state_updater) do
    query =
      query
      |> Query.set_emulation_flag()
      |> Query.resolve_db_columns()
      |> Compiler.Helpers.apply_top_down(&compile_emulated_subqueries/1)

    state_updater.(:awaiting_data)

    processing_steps =
      Query.Lenses.all_queries()
      |> Lens.filter(&(&1.type == :anonymized))
      |> Lens.to_list(query)
      |> Enum.count()

    state_updater.({:set_processing_steps, processing_steps})

    state_updater = fn acc, state ->
      state_updater.(state)
      acc
    end

    select_rows({:subquery, %{ast: query}}, state_updater)
  end

  # -------------------------------------------------------------------
  # Selection of rows from subparts of an emulated query
  # -------------------------------------------------------------------

  defp select_rows({:subquery, %{ast: %Query{emulated?: true, from: from} = subquery}}, state_updater)
       when not is_binary(from) do
    Query.debug_log(subquery, "Emulating query ...")
    rows = subquery.from |> select_rows(state_updater) |> Selector.pick_db_columns(subquery)
    process_rows([rows], subquery, state_updater)
  end

  defp select_rows({:subquery, %{ast: query}}, state_updater) do
    %Query{query | subquery?: not query.emulated? and query.type != :anonymized, where: Query.offloaded_where(query)}
    |> Query.debug_log("Offloading query ...")
    |> DataSource.select!(fn chunks ->
      chunks
      |> Stream.transform(:first, fn
        chunk, :first -> {[state_updater.(chunk, :ingesting_data)], :not_first}
        chunk, :not_first -> {[chunk], :not_first}
      end)
      |> process_rows(query, state_updater)
    end)
  end

  defp select_rows({:join, join}, state_updater) do
    Logger.debug("Emulating join ...")
    query_id = Keyword.get(Logger.metadata(), :query_id, nil)

    rhs_task =
      Task.async(fn ->
        Logger.metadata(query_id: query_id)
        select_rows(join.rhs, state_updater)
      end)

    lhs_rows = select_rows(join.lhs, state_updater)
    rhs_rows = Task.await(rhs_task, :infinity)
    Selector.join(lhs_rows, rhs_rows, join)
  end

  defp process_rows(chunks, %Query{type: :anonymized} = query, state_updater) do
    Logger.debug("Anonymizing query result ...")

    query = RowSplitters.compile(query)

    chunks
    |> ParallelProcessor.execute(
      concurrency(query),
      &consume_rows(&1, query),
      &Aggregator.merge_groups/2
    )
    |> state_updater.(:processing)
    |> Aggregator.aggregate(query)
    |> state_updater.(:post_processing)
    |> convert_buckets(query)
  end

  defp process_rows(chunks, %Query{emulated?: false} = query, _state_updater) do
    query = RowSplitters.compile(query)

    chunks
    |> Stream.concat()
    |> RowSplitters.split(query)
    |> Rows.filter(query |> Query.emulated_where() |> Condition.to_function())
    |> convert_rows(query)
  end

  defp process_rows(chunks, %Query{emulated?: true} = query, _state_updater) do
    query = RowSplitters.compile(query)

    chunks
    |> Stream.concat()
    |> RowSplitters.split(query)
    |> Selector.select(query)
    |> convert_rows(query)
  end

  defp consume_rows(stream, query) do
    stream
    |> RowSplitters.split(query)
    |> Rows.filter(query |> Query.emulated_where() |> Condition.to_function())
    |> Aggregator.group(query)
  end

  defp concurrency(query), do: query.data_source.concurrency || Application.get_env(:cloak, :concurrency, 0)

  defp convert_buckets(buckets, %Query{subquery?: true}),
    do: Stream.flat_map(buckets, &List.duplicate(&1.row, &1.occurrences))

  defp convert_buckets(buckets, %Query{subquery?: false}), do: buckets

  defp convert_rows(stream, %Query{subquery?: false}),
    do: Enum.map(stream, &%{row: &1, occurrences: 1, unreliable: nil})

  defp convert_rows(stream, %Query{subquery?: true}), do: Enum.to_list(stream)

  # -------------------------------------------------------------------
  # Transformation of joins for the purposes of emulated query selector
  # -------------------------------------------------------------------

  defp compile_emulated_subqueries(%Query{emulated?: true} = query) do
    query
    |> update_in([Query.Lenses.leaf_tables()], &joined_table_to_subquery(&1, query))
    |> Compiler.Optimizer.optimize()
    |> update_in(
      [Lens.root(), Query.Lenses.direct_subqueries() |> Lens.key(:ast)],
      &Query.resolve_db_columns/1
    )
    |> update_in([Query.Lenses.joins()], &compute_columns_to_select/1)
    |> update_in([Query.Lenses.joins()], &update_join_conditions/1)
  end

  defp compile_emulated_subqueries(query), do: query

  defp joined_table_to_subquery(table_name, query) do
    required_columns = required_columns_from_table(query, table_name)
    {:ok, table} = Query.resolve_table(query, table_name)
    query = Compiler.make_select_query(query.data_source, table, required_columns)
    {:subquery, %{ast: query, alias: table_name}}
  end

  defp required_columns_from_table(query, table_name),
    do:
      (query.db_columns ++ get_in(query, [Query.Lenses.join_conditions_terminals()]))
      |> get_in([Query.Lenses.leaf_expressions()])
      |> Enum.filter(&(&1.table != :unknown))
      |> Enum.filter(&(&1.table.name == table_name))
      |> Enum.map(&Expression.unalias/1)
      |> Enum.uniq_by(& &1.name)

  defp compute_columns_to_select(join), do: Map.put(join, :columns, columns_needed_for_join({:join, join}))

  defp update_join_conditions(join),
    do: %{
      join
      | conditions:
          Lens.map(
            Query.Lenses.conditions_terminals(),
            join.conditions,
            &set_column_row_index(&1, join.columns)
          )
    }

  defp columns_needed_for_join({:join, join}),
    do: columns_needed_for_join(join.lhs) ++ columns_needed_for_join(join.rhs)

  defp columns_needed_for_join({:subquery, subquery}) do
    user_id_name =
      case Enum.find_index(subquery.ast.columns, & &1.user_id?) do
        nil -> nil
        index -> Enum.at(subquery.ast.column_titles, index)
      end

    columns =
      subquery.ast.column_titles
      |> Enum.zip(subquery.ast.columns)
      |> Enum.map(fn {alias, column} ->
        Table.column(alias, Function.type(column))
      end)

    table = Table.new(subquery.alias, user_id_name, columns: columns)

    Enum.map(
      columns,
      &%Expression{table: table, name: &1.name, type: &1.type, user_id?: user_id_name == &1.name}
    )
  end

  defp set_column_row_index(%Expression{} = column, columns) do
    case Enum.find_index(columns, &(Expression.id(&1) == Expression.id(column))) do
      # It's not actually a needed column, so ignore for the purpose of positioning
      nil ->
        column

      position ->
        %Expression{column | row_index: position}
    end
  end

  defp set_column_row_index(other, _columns), do: other
end
