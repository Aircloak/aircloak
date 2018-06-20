defmodule Cloak.Sql.Compiler.Helpers do
  @moduledoc "Common helper functions used in compilation phases."

  alias Cloak.Sql.{Expression, Query, Parser}

  @type partial_query :: %Query{}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns one id column of the query."
  @spec id_column(partial_query) :: Expression.t()
  def id_column(%Query{type: :standard}), do: nil

  def id_column(query) do
    query
    |> all_id_columns_from_tables()
    |> case do
      [] ->
        nil

      id_columns ->
        id_column =
          if any_outer_join?(query.from),
            do: %Expression{
              Expression.function("coalesce", id_columns, hd(id_columns).type)
              | alias: "__ac_coalesce__",
                user_id?: true
            },
            else: hd(id_columns)

        %Expression{id_column | row_index: 0}
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
        [%Expression{table: table, name: user_id, type: column.type, user_id?: true}]

      _ ->
        []
    end)
  end

  @doc "Returns true if the provided expression is aggregated."
  @spec aggregated_column?(partial_query, Expression.t()) :: boolean
  def aggregated_column?(query, column) do
    normalizer = &(&1 |> Expression.unalias() |> Expression.semantic())

    Enum.member?(Enum.map(query.group_by, normalizer), normalizer.(column)) or
      (column.function? and (column.aggregate? or Enum.any?(column.function_args, &aggregated_column?(query, &1))))
  end

  @doc "Returns true if any of the query's bucket columns is aggregated."
  @spec aggregate?(partial_query) :: boolean
  def aggregate?(%Query{command: :select, group_by: [_ | _]}), do: true
  def aggregate?(%Query{command: :select, having: having}) when having != nil, do: true

  def aggregate?(%Query{command: :select} = query),
    do: query |> Query.bucket_columns() |> Enum.any?(&aggregated_column?(query, &1))

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
  @spec apply_bottom_up(q, (q -> q)) :: q when q: Query.t() | Parser.parsed_query()
  def apply_bottom_up(query, function), do: update_in(query, [Query.Lenses.all_queries()], function)

  @doc """
  Updates the query and all its subqueries with the given function. Starts from the top-level query going down.
  """
  @spec apply_top_down(q, (q -> q)) :: q when q: Query.t() | Parser.parsed_query()
  def apply_top_down(query, function),
    do:
      query
      |> function.()
      |> update_in(
        [Query.Lenses.direct_subqueries() |> Lens.key(:ast)],
        &apply_top_down(&1, function)
      )

  @doc "Runs the given function for its side-effects on the given query and all of its subqueries."
  @spec each_subquery(q, (q -> any)) :: :ok when q: Query.t() | Parser.parsed_query()
  def each_subquery(query, function) do
    Lens.each(Query.Lenses.all_queries(), query, function)
    :ok
  end

  @doc "Returns an error message about a user id missing from the select list for the given query."
  @spec missing_uid_error_message(Query.t(), String.t()) :: String.t()
  def missing_uid_error_message(query, alias) do
    possible_uid_columns =
      all_id_columns_from_tables(query)
      |> Enum.map(&Expression.display_name/1)
      |> case do
        [column] -> "the column #{column}"
        columns -> "one of the columns #{Enum.join(columns, ", ")}"
      end

    "Missing a user id column in the select list of #{"subquery `#{alias}`"}. " <>
      "To fix this error, add #{possible_uid_columns} to the subquery select list."
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)

  defp any_outer_join?(table) when is_binary(table), do: false
  defp any_outer_join?({:subquery, _}), do: false

  defp any_outer_join?({:join, %{type: type}})
       when type in [:full_outer_join, :left_outer_join, :right_outer_join],
       do: true

  defp any_outer_join?({:join, join}), do: any_outer_join?(join.lhs) || any_outer_join?(join.rhs)
end
