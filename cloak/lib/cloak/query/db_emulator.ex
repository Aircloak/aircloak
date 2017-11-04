defmodule Cloak.Query.DbEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """
  require Logger

  alias Cloak.{DataSource, DataSource.Table}
  alias Cloak.Sql.{Query, Expression, Function}
  alias Cloak.Query.{DbEmulator.Selector, DataEngine, DataDecoder}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Retrieves rows according to the specification in the emulated query."
  @spec select(Query.t) :: [Enumerable.t]
  def select(%Query{emulated?: true} = query) do
    query = compile_emulated_joins(query)
    [query.from |> select_rows() |> Selector.pick_db_columns(query)]
  end


  # -------------------------------------------------------------------
  # Selection of rows from subparts of an emulated query
  # -------------------------------------------------------------------

  defp offload_select!(query, rows_processor) do
    DataSource.select!(
      %Query{query | where: DataEngine.offloaded_where(query)},
      fn(rows) ->
        rows
        |> Stream.concat()
        |> DataDecoder.decode(query)
        |> rows_processor.()
      end)
  end

  defp select_rows({:subquery, %{ast: %Query{emulated?: false} = query}}) do
    query
    |> Query.debug_log("Executing sub-query through data source")
    |> offload_select!(&Enum.to_list/1)
  end
  defp select_rows({:subquery, %{ast: %Query{emulated?: true, from: from} = subquery}}) when not is_binary(from) do
    subquery =
      subquery
      |> Query.debug_log("Emulating intermediate sub-query")
      |> compile_emulated_joins()
    subquery.from
    |> select_rows()
    |> Selector.pick_db_columns(subquery)
    |> Selector.select(subquery)
  end
  defp select_rows({:subquery, %{ast: %Query{emulated?: true} = query}}) do
    %Query{query | subquery?: false}
    |> Query.debug_log("Emulating leaf sub-query")
    |> offload_select!(&Selector.select(&1, query))
  end
  defp select_rows({:join, join}) do
    Logger.debug("Emulating join ...")
    query_id = Keyword.get(Logger.metadata(), :query_id, nil)
    rhs_task = Task.async(fn() -> Logger.metadata(query_id: query_id); select_rows(join.rhs) end)
    lhs_rows = select_rows(join.lhs)
    rhs_rows = Task.await(rhs_task, :infinity)
    Selector.join(lhs_rows, rhs_rows, join)
  end


  # -------------------------------------------------------------------
  # Transformation of joins for the purposes of emulated query selector
  # -------------------------------------------------------------------

  defp compile_emulated_joins(%Query{emulated?: true, from: {:join, _}} = query) do
    query
    |> update_in([Query.Lenses.leaf_tables()], &joined_table_to_subquery(&1, query))
    |> update_in([Query.Lenses.joins()], &compute_columns_to_select/1)
    |> update_in([Query.Lenses.joins()], &update_join_conditions/1)
  end
  defp compile_emulated_joins(query), do: query

  defp joined_table_to_subquery(table_name, query) do
    required_columns = required_columns_from_table(query, table_name)
    {:ok, table} = Query.resolve_table(query, table_name)
    query = Cloak.Sql.Compiler.make_select_query(query.data_source, table, required_columns)
    {:subquery, %{ast: query, alias: table_name}}
  end

  defp required_columns_from_table(query, table_name), do:
    (query.db_columns ++ get_in(query, [Query.Lenses.join_conditions_terminals()]))
    |> get_in([Query.Lenses.leaf_expressions()])
    |> Enum.filter(&(&1.table != :unknown))
    |> Enum.filter(&(&1.table.name == table_name))
    |> Enum.uniq_by(&{&1.name, &1.alias})

  defp compute_columns_to_select(join), do:
    Map.put(join, :columns, columns_needed_for_join({:join, join}))

  defp update_join_conditions(join), do:
    %{join | conditions: Lens.map(Query.Lenses.conditions_terminals(), join.conditions,
      &set_column_row_index(&1, join.columns))}

  defp columns_needed_for_join({:join, join}), do:
    columns_needed_for_join(join.lhs) ++ columns_needed_for_join(join.rhs)
  defp columns_needed_for_join({:subquery, subquery}) do
    user_id_name = case Enum.find_index(subquery.ast.columns, &(&1.user_id?)) do
      nil -> nil
      index -> Enum.at(subquery.ast.column_titles, index)
    end
    columns = subquery.ast.column_titles
    |> Enum.zip(subquery.ast.columns)
    |> Enum.map(fn ({alias, column}) ->
      Table.column(alias, Function.type(column))
    end)
    table = Table.new(subquery.alias, user_id_name, columns: columns)
    columns
    |> Enum.map(&%Expression{table: table, name: &1.name, type: &1.type, user_id?: user_id_name == &1.name})
    |> Enum.uniq()
  end

  defp set_column_row_index(%Expression{} = column, columns) do
    case Enum.find_index(columns, &Expression.id(&1) == Expression.id(column)) do
      # It's not actually a needed column, so ignore for the purpose of positioning
      nil -> column
      position -> %Expression{column | row_index: position}
    end
  end
  defp set_column_row_index(other, _columns), do: other
end
