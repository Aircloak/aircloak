defmodule Cloak.Sql.Compiler.Helpers do
  @moduledoc "Common helper functions used in compilation phases."

  alias Cloak.Sql.{Expression, Query, Parser, Function}
  alias Cloak.DataSource.Table

  @type partial_query :: %Query{}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns one id column of the query."
  @spec id_column(partial_query) :: Expression.t() | nil
  def id_column(%Query{type: :standard}), do: nil

  def id_column(query) do
    query
    |> all_id_columns_from_tables()
    |> case do
      [] ->
        nil

      id_columns ->
        if any_outer_join?(query.from) and length(id_columns) > 1,
          do: %Expression{
            Expression.function("coalesce", id_columns, hd(id_columns).type)
            | alias: "__ac_user_id__",
              user_id?: true,
              row_index: 0
          },
          else: %Expression{hd(id_columns) | row_index: 0}
    end
  end

  @doc "Returns true if any uid column is selected."
  @spec uid_column_selected?(partial_query) :: boolean
  def uid_column_selected?(query), do: Enum.any?(query.columns, & &1.user_id?)

  @doc "Returns all id columns from the query."
  @spec all_id_columns_from_tables(partial_query) :: [Expression.t()]
  def all_id_columns_from_tables(%Query{command: :select, selected_tables: tables}) do
    Enum.flat_map(tables, fn
      %{user_id: user_id} = table when user_id != nil ->
        column = Enum.find(table.columns, &insensitive_equal?(user_id, &1.name))
        [Expression.column(column, table)]

      _ ->
        []
    end)
  end

  @doc "Returns true if a column is GROUPED BY"
  @spec grouped_by?(partial_query, Expression.t()) :: boolean
  def grouped_by?(query, column), do: Expression.member?(query.group_by, column)

  @doc "Returns true if the query has GROUP BY columns that were not selected (ignoring subqueries)"
  @spec non_selected_group_bys(partial_query) :: [Expression.t()]
  def non_selected_group_bys(%Query{columns: columns, group_by: group_bys}),
    do: Enum.filter(group_bys, &(not Expression.member?(columns, &1)))

  @doc "Returns true if the provided expression is aggregated."
  @spec aggregated_column?(Expression.t()) :: boolean
  def aggregated_column?({:distinct, _column}), do: true

  def aggregated_column?(column),
    do: Expression.function?(column) and (Function.aggregator?(column) or Enum.any?(column.args, &aggregated_column?/1))

  @doc "Returns true if the query GROUPS BY columns"
  @spec group_by?(partial_query) :: boolean
  def group_by?(%Query{command: :select, grouping_sets: [_ | _]}), do: true
  def group_by?(_), do: false

  @doc "Returns true if any of the query's bucket columns is aggregated."
  @spec aggregates?(partial_query) :: boolean
  def aggregates?(%Query{command: :select, having: having}) when having != nil, do: true

  def aggregates?(%Query{command: :select} = query),
    do: query |> Query.bucket_columns() |> Enum.any?(&aggregated_column?/1)

  @doc "Creates the default grouping sets from a set of columns."
  @spec default_grouping_sets([Expression.t()]) :: [[non_neg_integer()]]
  def default_grouping_sets([]), do: [[]]
  def default_grouping_sets(columns), do: [Enum.to_list(0..(length(columns) - 1))]

  @doc "Returns all join conditions of the query."
  @spec all_join_conditions(partial_query) :: [Query.where_clause()]
  def all_join_conditions(query),
    do:
      Query.Lenses.join_conditions()
      |> Query.Lenses.conditions()
      |> Lens.to_list(query)

  @doc """
  Updates the query and all its subqueries with the given function. Starts from the most nested subqueries going up.
  """
  @spec apply_bottom_up(q, (q -> q), Query.Lenses.query_filter()) :: q when q: Query.t() | Parser.parsed_query()
  def apply_bottom_up(query, function, opts \\ []), do: update_in(query, [Query.Lenses.all_queries(opts)], function)

  @doc """
  Updates the query and all its subqueries with the given function. Starts from the top-level query going down.
  """
  @spec apply_top_down(q, (q -> q), Query.Lenses.query_filter()) :: q when q: Query.t() | Parser.parsed_query()
  def apply_top_down(query, function, opts \\ []),
    do:
      query
      |> function.()
      |> update_in(
        [Query.Lenses.direct_subqueries(opts) |> Lens.key(:ast)],
        &apply_top_down(&1, function, opts)
      )

  @doc "Runs the given function for its side-effects on the given query and all of its subqueries."
  @spec each_subquery(q, (q -> any)) :: :ok when q: Query.t() | Parser.parsed_query()
  def each_subquery(query, function) do
    Lens.each(Query.Lenses.all_queries(), query, function)
    :ok
  end

  @doc "Returns a column expression from a table."
  @spec column_from_table(Table.t(), String.t()) :: Expression.t()
  def column_from_table(table, name) do
    table.columns
    |> Enum.find(&(&1.name == name))
    |> Expression.column(table)
  end

  @doc "Returns the list of expressions from a query that can contain aggregating clauses."
  @spec aggregator_sources(Query.t()) :: [Expression.t()]
  def aggregator_sources(query), do: query.columns ++ having_columns(query) ++ Query.order_by_expressions(query)

  @doc "Creates a synthetic table from a list of selected columns."
  @spec create_table_from_columns([Expression.t()], String.t(), [Table.option()]) :: Table.t()
  def create_table_from_columns(selected_columns, table_name, opts \\ []) do
    table_columns =
      Enum.map(
        selected_columns,
        &Table.column(
          Expression.title(&1),
          Function.type(&1),
          access:
            cond do
              &1.synthetic? -> :hidden
              unselectable_selected_expression?(&1) -> :unselectable
              true -> :visible
            end
        )
      )

    user_id_column = Enum.find(selected_columns, & &1.user_id?)
    user_id_name = user_id_column && Expression.title(user_id_column)

    keys =
      selected_columns
      |> Enum.filter(&Expression.key?/1)
      |> Enum.map(&{Expression.title(&1), Expression.key_type(&1)})
      |> Enum.into(%{})

    opts = [type: :subquery] |> Keyword.merge(opts) |> Keyword.merge(columns: table_columns, keys: keys)

    Table.new(table_name, user_id_name, opts)
  end

  @doc "Returns a list of unselectable db columns which this expression depends on."
  @spec unselectable_db_columns(Query.t(), Expression.t()) :: [Expression.t()]
  def unselectable_db_columns(query, expression) do
    {query, expression}
    |> resolve_unselectable_db_columns()
    |> Enum.map(&elem(&1, 1))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp unselectable_selected_expression?(expr), do: unselectable_selected_columns(expr) != []

  defp unselectable_selected_columns(expr) do
    Query.Lenses.expression_unselectable_selected_columns()
    |> Lens.to_list(expr)
  end

  defp resolve_unselectable_db_columns({query, expr}) do
    expr
    |> unselectable_selected_columns()
    |> Enum.flat_map(fn column ->
      case Query.resolve_subquery_column(column, query) do
        :database_column -> [{query, column}]
        {column, subquery} -> resolve_unselectable_db_columns({subquery, column})
      end
    end)
  end

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)

  defp any_outer_join?(table) when is_binary(table), do: false
  defp any_outer_join?({:subquery, _}), do: false

  defp any_outer_join?({:join, %{type: type}})
       when type in [:full_outer_join, :left_outer_join, :right_outer_join],
       do: true

  defp any_outer_join?({:join, join}), do: any_outer_join?(join.lhs) || any_outer_join?(join.rhs)

  defp having_columns(query),
    do:
      Query.Lenses.conditions()
      |> Query.Lenses.operands()
      |> Lens.to_list(query.having)
end
