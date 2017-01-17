defmodule Cloak.Query.DbEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """
  require Logger

  alias Cloak.DataSource
  alias Cloak.Sql.{Query, Comparison, Expression, Function}
  alias Cloak.Query.{Rows, DataDecoder, DbEmulator.Selector}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Retrieves rows according to the specification in the emulated query."
  @spec select(Query.t) :: Enumerable.t
  def select(%Query{emulated?: true} = query) do
    query = compile_emulated_joins(query)
    query.from
    |> select_rows()
    |> Selector.pick_db_columns(query)
  end


  # -------------------------------------------------------------------
  # Selection of rows from subparts of an emulated query
  # -------------------------------------------------------------------

  defp select_rows({:subquery, %{ast: %Query{emulated?: false} = query}}) do
    Logger.debug("Loading sub-query through data source ...")
    DataSource.select!(query, fn(rows) ->
      rows
      |> DataDecoder.decode(query)
      |> Rows.filter(Enum.map(query.encoded_where, &Comparison.to_function/1))
      |> Enum.to_list()
    end)
  end
  defp select_rows({:subquery, %{ast: %Query{emulated?: true, from: from} = subquery}}) when not is_binary(from) do
    Logger.debug("Emulating intermediate sub-query ...")
    subquery = compile_emulated_joins(subquery)
    rows = select_rows(subquery.from)
    Logger.debug("Processing rows ...")
    rows
    |> Selector.pick_db_columns(subquery)
    |> Selector.select(subquery)
    |> Enum.to_list()
  end
  defp select_rows({:subquery, %{ast: %Query{emulated?: true} = query}}) do
    Logger.debug("Emulating leaf sub-query ...")
    DataSource.select!(%Query{query | subquery?: false}, fn(rows) ->
      Logger.debug("Processing rows ...")
      rows
      |> DataDecoder.decode(query)
      |> Selector.select(%Query{query | where: query.encoded_where, encoded_where: []})
      |> Enum.to_list()
    end)
  end
  defp select_rows({:join, join}) do
    Logger.debug("Emulating join ...")
    rhs_task = Task.async(fn() -> select_rows(join.rhs) end)
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
    required_columns =
      (query.db_columns ++ get_in(query, [Query.Lenses.join_conditions_terminals()]))
      |> get_in([Query.Lenses.leaf_expressions()])
      |> Enum.filter(& &1.table != :unknown and &1.table.name == table_name)
      |> Enum.uniq_by(&Expression.id/1)
    query = Cloak.Sql.Compiler.make_select_query(query.data_source, table_name, required_columns, subquery?: true)
    {:subquery, %{type: :parsed, ast: query, alias: table_name}}
  end

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
    columns =
        Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
        |> Enum.map(fn ({alias, column}) -> {alias, Function.type(column)} end)
    table = %{
      name: subquery.alias,
      columns: columns,
      user_id: user_id_name,
      decoders: [],
      projection: nil
    }
    Enum.zip(subquery.ast.column_titles, subquery.ast.columns)
    |> Enum.map(fn ({alias, column}) ->
      %Expression{table: table, name: alias, type: Function.type(column), user_id?: user_id_name == alias}
    end)
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
