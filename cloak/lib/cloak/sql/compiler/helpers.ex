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
      do: Expression.function("coalesce", id_columns),
      else: hd(id_columns)
  end

  @doc "Returns all id columns from the query."
  @spec all_id_columns_from_tables(partial_query) :: [Expression.t]
  def all_id_columns_from_tables(%Query{command: :select, selected_tables: tables}) do
    Enum.map(tables, fn(table) ->
      user_id = table.user_id
      {_, type} = Enum.find(table.columns, fn ({name, _type}) -> insensitive_equal?(user_id, name) end)
      %Expression{table: table, name: user_id, type: type, user_id?: true}
    end)
  end

  @doc "Returns true if the provided expression is aggregated."
  @spec aggregated_column?(partial_query, Expression.t) :: boolean
  def aggregated_column?(query, column), do:
    Enum.member?(query.group_by, column) or
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
    get_all_join_conditions(query.from)


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

  defp get_all_join_conditions({:join, join}) do
    join.conditions ++ get_all_join_conditions(join.lhs) ++ get_all_join_conditions(join.rhs)
  end
  defp get_all_join_conditions(_), do: []
end
