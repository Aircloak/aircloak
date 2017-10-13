defmodule Cloak.Sql.Compiler.Helpers do
  @moduledoc "Common helper functions used in compilation phases."

  alias Cloak.Sql.{Expression, Query}

  @type partial_query :: %Query{}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns one id column of the query."
  @spec id_column(partial_query) :: Expression.t
  def id_column(query) do
    id_columns = all_id_columns_from_tables(query)
    if any_outer_join?(query.from),
      do: %Expression{Expression.function("coalesce", id_columns) | alias: "__ac_coalesce__"},
      else: hd(id_columns)
  end

  @doc "Returns true if any uid column is selected."
  @spec uid_column_selected?(partial_query) :: boolean
  def uid_column_selected?(query), do:
    Enum.any?(query.columns, &(&1.user_id?))

  @doc "Returns all id columns from the query."
  @spec all_id_columns_from_tables(partial_query) :: [Expression.t]
  def all_id_columns_from_tables(%Query{command: :select, selected_tables: tables}) do
    Enum.flat_map(tables, fn
      (%{projection: nil} = table) ->
        user_id = table.user_id
        column = Enum.find(table.columns, &insensitive_equal?(user_id, &1.name))
        [%Expression{table: table, name: user_id, type: column.type, user_id?: true}]
      (_) -> []
    end)
  end

  @doc "Returns true if the provided expression is aggregated."
  @spec aggregated_column?(partial_query, Expression.t) :: boolean
  def aggregated_column?(query, column), do:
    Enum.member?(Enum.map(query.group_by, &Expression.unalias/1), Expression.unalias(column)) or
    (
      column.function? and
      (
        column.aggregate? or
        Enum.any?(column.function_args, &aggregated_column?(query, &1))
      )
    )

  @doc "Returns true if any of the query's bucket columns is aggregated."
  @spec aggregate?(partial_query) :: boolean
  def aggregate?(%Query{command: :select, group_by: [_|_]}), do: true
  def aggregate?(%Query{command: :select} = query), do:
    query |> Query.bucket_columns() |> Enum.any?(&aggregated_column?(query, &1))

  @doc "Returns all join conditions of the query."
  @spec all_join_conditions(partial_query) :: [Query.where_clause]
  def all_join_conditions(query), do:
    Query.Lenses.join_conditions()
    |> Query.Lenses.conditions()
    |> Lens.to_list(query)

  @doc "Modifies the expression to have a globally unique alias."
  @spec set_unique_alias(Expression.t) :: Expression.t
  def set_unique_alias(column), do: %{column | alias: "alias_#{System.unique_integer([:positive])}"}

  @doc """
  Removes columns from new_columns that are duplicated or already present in selected_columns. Returns a modified
  version of query where the appropriate selected columns are used instead of the removed columns.
  """
  @spec drop_redundant_floated_columns(Query.t, [Expression.t], [Expression.t]) :: {Query.t, [Expression.t]}
  def drop_redundant_floated_columns(query, selected_columns, new_columns) do
    selected_ids = Enum.map(selected_columns, &Expression.id/1) |> Enum.uniq()

    {already_selected, new_columns} = Enum.partition(new_columns, &Expression.id(&1) in selected_ids)
    uniq_new = Enum.uniq_by(new_columns, &Expression.id/1)
    duplicated_new = new_columns -- uniq_new
    replacements = selected_columns ++ uniq_new

    query = Enum.reduce(already_selected ++ duplicated_new, query, fn (column, query) ->
      replacement = Enum.find(replacements, &Expression.id(&1) == Expression.id(column))
      Query.Lenses.query_expressions() |> Lens.satisfy(&column == &1) |> Lens.map(query, fn(_) -> replacement end)
    end)

    {query, uniq_new}
  end

  @doc """
  Updates the query and all its subqueries with the given function. Starts from the most nested subqueries going up.
  """
  @spec apply_bottom_up(Query.t, (Query.t -> Query.t)) :: Query.t
  def apply_bottom_up(query, function), do:
    query
    |> update_in([Query.Lenses.direct_subqueries() |> Lens.key(:ast)], &apply_bottom_up(&1, function))
    |> function.()

  @doc """
  Returns an expression that will reference the given aliased expression in the given subquery to be used in the outer
  query. The third argument is the virtual table produced by the subquery.
  """
  @spec reference_aliased(Expression.t, Query.t, DataSource.Table.t | :unknown) :: Expression.t
  def reference_aliased(column, subquery, table \\ :unknown), do:
    %Expression{name: column.alias || find_alias(column, subquery) || column.name, table: table}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)

  defp any_outer_join?(table) when is_binary(table), do: false
  defp any_outer_join?({:subquery, _}), do: false
  defp any_outer_join?({:join, %{type: type}})
    when type in [:full_outer_join, :left_outer_join, :right_outer_join],
    do: true
  defp any_outer_join?({:join, join}),
    do: any_outer_join?(join.lhs) || any_outer_join?(join.rhs)

  defp find_alias(column, query) do
    id = Expression.id(column)
    case Enum.find_index(query.columns, &Expression.id(&1) == id) do
      nil -> nil
      index ->
        true = index < length(query.column_titles)
        Enum.at(query.column_titles, index)
    end
  end
end
