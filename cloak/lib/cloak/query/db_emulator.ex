defmodule Cloak.Query.DbEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """
  require Logger

  alias Cloak.{DataSource, Query.DbEmulator.Selector}
  alias Cloak.Aql.{Query, Expression, Function}


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

  defp select_rows({:subquery, %{ast: %Query{emulated?: true, from: from} = subquery}}) when not is_binary(from) do
    Logger.debug("Emulating sub-query ...")
    subquery = compile_emulated_joins(subquery)
    rows = select_rows(subquery.from)

    Logger.debug("Processing rows ...")
    rows
    |> Selector.pick_db_columns(subquery)
    |> Selector.select(subquery)
    |> Enum.to_list()
  end
  defp select_rows({:subquery, %{ast: %Query{} = query}}) do
    # either a non-emulated subquery, or a subquery selecting from a single table
    Logger.debug("Loading sub-query through data source ...")
    DataSource.select!(%Query{query | subquery?: false}, fn(rows) ->
      Logger.debug("Processing rows ...")
      rows
      |> Cloak.Query.DataDecoder.decode(query)
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
    from =
      query.from
      |> replace_joined_tables_with_subqueries(query.db_columns, query)
      |> compile_join_conditions_columns()
    %Query{query | from: from}
  end
  defp compile_emulated_joins(query), do: query

  # The DBEmulator modules doesn't know how to select a table directly, so we need
  # to replace any direct references to joined table with the equivalent subquery.
  defp replace_joined_tables_with_subqueries(table_name, columns, parent_query) when is_binary(table_name) do
    query =
      Cloak.Aql.Compiler.make_select_query(
        parent_query.data_source,
        table_name,
        (for %Expression{table: %{name: ^table_name}} = column <- columns, do: column),
        subquery?: true
      )
    {:subquery, %{type: :parsed, ast: query, alias: table_name}}
  end
  defp replace_joined_tables_with_subqueries({:subquery, subquery}, _columns, _parent_query), do: {:subquery, subquery}
  defp replace_joined_tables_with_subqueries({:join, join}, db_columns, parent_query) do
    on_columns = get_in(join.conditions, [Query.Lenses.leaf_expressions()])
    columns = Enum.uniq_by(db_columns ++ on_columns, &Expression.id/1)
    lhs = replace_joined_tables_with_subqueries(join.lhs, columns, parent_query)
    rhs = replace_joined_tables_with_subqueries(join.rhs, columns, parent_query)
    {:join, %{join | lhs: lhs, rhs: rhs}}
  end

  # For emulated joins, we need to set the `row_index` field for the columns in the `ON` clause.
  defp compile_join_conditions_columns({:join, join}) do
    # Because this is an emulated query we only have joining of subqueries, as
    # tables references were previously translated into subqueries.
    columns = joined_columns({:join, join})
    mapper_fun = &set_column_row_index(&1, columns)
    conditions = Lens.map(Query.Lenses.conditions_terminals(), join.conditions, mapper_fun)
    lhs = compile_join_conditions_columns(join.lhs)
    rhs = compile_join_conditions_columns(join.rhs)
    join = Map.put(join, :columns, columns)
    {:join, %{join | conditions: conditions, lhs: lhs, rhs: rhs}}
  end
  defp compile_join_conditions_columns({:subquery, subquery}), do: {:subquery, subquery}

  defp joined_columns({:join, join}) do
    joined_columns(join.lhs) ++ joined_columns(join.rhs)
  end
  defp joined_columns({:subquery, subquery}) do
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
