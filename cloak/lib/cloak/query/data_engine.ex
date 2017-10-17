defmodule Cloak.Query.DataEngine do
  @moduledoc "Retrieval of data from the data source according to the query specification."

  require Logger

  alias Cloak.Sql.{Compiler.Helpers, Condition, Expression, Query, Query.Lenses}
  alias Cloak.Query.DataDecoder


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Retrieves rows from the database, and applies the row processor on them."
  @spec select(Query.t, ((Enumerable.t) -> result)) :: result when result: var
  def select(query, row_processor) do
    if query.emulated? do
      Logger.debug("Emulating query ...")
      row_processor.(Cloak.Query.DbEmulator.select(query))
    else
      Logger.debug("Processing final rows ...")
      Cloak.DataSource.select!(query, fn(rows) -> row_processor.(DataDecoder.decode(rows, query)) end)
    end
  end

  @doc "Determines whether the query needs to be emulated or not."
  @spec needs_emulation?(Query.t) :: boolean
  def needs_emulation?(query), do:
    not query.data_source.driver.supports_query?(query) or
    query |> get_in([Query.Lenses.direct_subqueries()]) |> Enum.any?(&(&1.ast.emulated?)) or
    (query.subquery? and has_emulated_expressions?(query)) or
    has_emulated_join_conditions?(query)

  @doc "Partitions where clauses to the ones which need to be emulated and the ones which don't."
  @spec partitioned_where_clauses(Query.t) :: {Query.where_clause, Query.where_clause}
  def partitioned_where_clauses(query) do
    Condition.partition(query.where,
      fn(condition) ->
        emulated_expression_condition?(condition, query.data_source) or
        (
          query.emulated? and
          (
            multiple_tables_condition?(condition) or
            not is_binary(query.from)
          )
        )
      end
    )
  end

  @doc "Resolves the columns which must be fetched from the database."
  @spec resolve_db_columns(Query.t) :: Query.t
  def resolve_db_columns(%Query{command: :select} = query), do:
    Helpers.apply_bottom_up(query, &resolve_query_db_columns/1)
  def resolve_db_columns(%Query{} = query), do:
    query


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp emulated_expression?(expression, data_source), do:
    DataDecoder.needs_decoding?(expression) or
    (expression.function? and not data_source.driver.supports_function?(expression, data_source))

  defp emulated_expression_condition?(condition, data_source) do
    Query.Lenses.conditions_terminals()
    |> Lens.to_list([condition])
    |> Enum.any?(&emulated_expression?(&1, data_source))
  end

  defp has_emulated_expressions?(query), do:
    Query.Lenses.all_expressions()
    |> Lens.to_list([query.columns, query.group_by, query.having, query.where])
    |> Enum.any?(&emulated_expression?(&1, query.data_source))

  defp has_emulated_join_conditions?(query), do:
    query
    |> Helpers.all_join_conditions()
    |> get_in([Query.Lenses.all_expressions()])
    |> Enum.any?(&emulated_expression?(&1, query.data_source))

  defp multiple_tables_condition?(condition) do
    Query.Lenses.conditions_terminals()
    |> Lens.to_list([condition])
    |> Enum.map(& &1.table)
    |> Enum.uniq()
    |> Enum.count() > 1
  end


  # -------------------------------------------------------------------
  # Calculation of db_columns
  # -------------------------------------------------------------------

  defp resolve_query_db_columns(query), do:
    query
    |> include_required_expressions()
    |> optimize_columns_from_projected_subqueries()

  defp include_required_expressions(query), do:
    Enum.reduce(required_expressions(query), query, &Query.add_db_column(&2, &1))

  defp required_expressions(%Query{command: :select, subquery?: true, emulated?: false} = query) do
    Enum.zip(query.column_titles, query.columns)
    |> Enum.map(fn({column_alias, column}) -> %Expression{column | alias: column_alias} end)
  end
  defp required_expressions(%Query{command: :select} = query) do
    # top-level query -> we're only fetching columns, while other expressions (e.g. function calls)
    # will be resolved in the post-processing phase
    used_columns =
      query
      |> needed_columns()
      |> extract_columns()
      |> Enum.reject(& &1.constant?)

    [Helpers.id_column(query) | used_columns]
  end

  defp needed_columns(query), do:
    [
      query.columns,
      query.group_by,
      query.emulated_where,
      query.having,
      Query.order_by_expressions(query),
    ]

  defp optimize_columns_from_projected_subqueries(%Query{projected?: false} = query), do:
    # We're reducing the amount of selected columns from projected subqueries to only
    # those columns which we in fact need in the outer query (`query`).
    Lens.map(
      Query.Lenses.direct_projected_subqueries(),
      query,
      &%{&1 | ast: optimized_projected_subquery_ast(&1.ast, required_column_names(query, &1.alias))}
    )
  defp optimize_columns_from_projected_subqueries(%Query{projected?: true} = query), do:
    # If this query is projected, then the list was already optimized when the ast for this query
    # has been initially generated, so no need to do anything.
    query

  defp required_column_names(query, subquery_name), do:
    # all db columns of the outer query which are from this projected table, except the user id
    query |> used_columns_from_table(subquery_name) |> Enum.map(& &1.name)

  defp used_columns_from_table(query, table_name) do
    all_terminals = Lens.both(Lenses.terminals(), Lenses.join_conditions_terminals()) |> Lens.to_list(query)
    Lenses.leaf_expressions()
    |> Lens.to_list(all_terminals)
    |> Enum.filter(& &1.table != :unknown and &1.table.name == table_name)
    |> Enum.uniq_by(&Expression.id/1)
  end

  defp optimized_projected_subquery_ast(ast, required_column_names) do
    [user_id | columns] = ast.columns
    [user_id_title | column_titles] = ast.column_titles
    columns = [user_id | Enum.filter(columns, &(&1.alias || &1.name) in required_column_names)]
    titles = [user_id_title | Enum.filter(column_titles, & &1 in required_column_names)]
    %Query{ast | next_row_index: 0, db_columns: [], columns: columns, column_titles: titles}
    |> Query.set_emulation_flag()
    |> resolve_db_columns()
  end

  defp extract_columns(columns), do:
    Query.Lenses.leaf_expressions() |> Lens.to_list(columns)
end
