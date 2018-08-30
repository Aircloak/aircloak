defmodule Cloak.Sql.Query.Features do
  @moduledoc false

  alias Cloak.Sql.{Function, Expression, Query, Condition}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def features(query) do
    %{
      num_top_level_dimensions: num_top_level_dimensions(query),
      num_top_level_aggregates: num_top_level_aggregates(query),
      num_db_columns: num_db_columns(query),
      num_distinct_db_columns: num_distinct_db_columns(query),
      num_tables: num_tables(query),
      num_distinct_tables: num_distinct_tables(query),
      num_top_level_group_by: num_group_by(query),
      num_subquery_group_by: num_subquery_group_by(query),
      num_group_by: num_group_by(query) + num_subquery_group_by(query),
      top_level_select_functions: top_level_select_functions(query),
      subquery_select_functions: subquery_select_functions(query),
      select_functions: Enum.uniq(top_level_select_functions(query) ++ subquery_select_functions(query)),
      top_level_functions: top_level_functions(query),
      subquery_functions: subquery_functions(query),
      functions: Enum.uniq(top_level_functions(query) ++ subquery_functions(query)),
      expressions: extract_expressions(query),
      top_level_filters: top_level_filters(query),
      subquery_filters: subquery_filters(query),
      filters: Enum.uniq(top_level_filters(query) ++ subquery_filters(query)),
      db_column_types: db_column_types(query),
      selected_types: selected_types(query.columns),
      parameter_types: Enum.map(Query.parameter_types(query), &stringify/1),
      driver: to_string(query.data_source.driver),
      driver_dialect: sql_dialect_name(query.data_source)
    }
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp num_top_level_dimensions(query) do
    query.columns |> Enum.reject(&aggregate?/1) |> Enum.count()
  end

  defp num_top_level_aggregates(query) do
    query.columns |> Enum.filter(&aggregate?/1) |> Enum.count()
  end

  defp aggregate?(%{aggregate?: true}), do: true
  defp aggregate?(%{function?: true, function_args: args}), do: Enum.any?(args, &aggregate?/1)
  defp aggregate?(_), do: false

  defp selected_types(columns), do: columns |> Enum.map(&Function.type/1) |> Enum.map(&stringify/1)

  defp num_db_columns(query), do: query |> db_columns() |> Enum.count()

  defp num_distinct_db_columns(query),
    do: query |> db_columns() |> Enum.uniq_by(&{&1.name, &1.table}) |> Enum.count()

  defp db_column_types(query), do: query |> db_columns() |> Enum.map(& &1.type) |> Enum.map(&stringify/1)

  defp db_columns(query) do
    Query.Lenses.all_queries()
    |> Lens.context(possible_db_columns())
    |> Lens.to_list(Query.resolve_db_columns(query))
    |> Enum.reject(fn {_subquery, column} -> column.constant? end)
    |> Enum.flat_map(fn {subquery, column} ->
      case Query.resolve_subquery_column(column, subquery) do
        :database_column -> [column]
        _ -> []
      end
    end)
  end

  def possible_db_columns() do
    Lens.both(Query.Lenses.filter_clauses(), Lens.keys([:db_columns, :group_by]))
    |> Query.Lenses.leaf_expressions()
  end

  defp num_tables(query), do: query |> tables() |> Enum.count()

  defp num_distinct_tables(query), do: query |> tables() |> Enum.uniq() |> Enum.count()

  defp tables(query) do
    Query.Lenses.all_queries()
    |> Lens.key(:selected_tables)
    |> Lens.all()
    |> Lens.filter(& &1.db_name)
    |> Lens.key(:db_name)
    |> Lens.to_list(query)
  end

  defp num_subquery_group_by(query) do
    query
    |> get_in([Query.Lenses.subqueries()])
    |> Enum.map(&num_group_by/1)
    |> Enum.sum()
  end

  defp num_group_by(%{group_by: clauses}), do: length(clauses)

  defp top_level_select_functions(query), do: extract_functions(query, Lens.key(:columns))

  defp subquery_select_functions(query), do: extract_functions(query, Query.Lenses.subqueries() |> Lens.key(:columns))

  defp top_level_functions(query), do: extract_functions(query, Query.Lenses.analyst_provided_expressions())

  defp subquery_functions(query),
    do: extract_functions(query, Query.Lenses.subqueries() |> Query.Lenses.analyst_provided_expressions())

  defp extract_functions(query, initial_lens) do
    query
    |> get_in([
      initial_lens
      |> Query.Lenses.all_expressions()
      |> Lens.filter(& &1.function?)
    ])
    |> Enum.map(&Function.readable_name(&1.function))
    |> Enum.uniq()
  end

  defp extract_expressions(query) do
    query
    |> get_in([Query.Lenses.analyst_provided_expressions()])
    |> Enum.map(&expression_to_lisp(&1, query))
  end

  defp expression_to_lisp(expression, query) do
    expression
    |> build_expression_tree(query)
    |> expression_tree_to_lisp()
  end

  defp expression_tree_to_lisp(list) when is_list(list),
    do: to_string([?(, list |> Enum.map(&expression_tree_to_lisp/1) |> Enum.intersperse(" "), ?)])

  defp expression_tree_to_lisp(other), do: to_string(other)

  defp build_expression_tree(
         %Expression{function?: true, function: function, function_args: args},
         query
       ),
       do: [Function.readable_name(function) | Enum.map(args, &build_expression_tree(&1, query))]

  defp build_expression_tree(%Expression{constant?: true}, _query), do: :const

  defp build_expression_tree({:distinct, expr}, query), do: [:distinct, build_expression_tree(expr, query)]

  defp build_expression_tree(:*, _query), do: :*

  defp build_expression_tree(other, query) do
    case Query.resolve_subquery_column(other, query) do
      :database_column -> :col
      {column, subquery} -> build_expression_tree(column, subquery)
    end
  end

  defp top_level_filters(query), do: filters(query, Lens.root())
  defp subquery_filters(query), do: filters(query, Query.Lenses.subqueries())

  defp filters(query, initial_lens) do
    initial_lens
    |> Lens.context(
      Query.Lenses.filter_clauses()
      |> Query.Lenses.conditions()
      |> Lens.reject(&Condition.subject(&1).synthetic?)
    )
    |> Lens.to_list(query)
    |> Enum.map(fn {subquery, filter} -> filter_to_tree(filter, subquery) end)
    |> Enum.map(&expression_tree_to_lisp/1)
    |> Enum.uniq()
  end

  defp filter_to_tree({:not, something}, query), do: ["not", filter_to_tree(something, query)]

  defp filter_to_tree({:comparison, column, comparison, comparator}, query),
    do: [Atom.to_string(comparison), build_expression_tree(column, query), build_expression_tree(comparator, query)]

  defp filter_to_tree({:in, column, _set}, query), do: ["in", build_expression_tree(column, query), "set"]

  defp filter_to_tree({:is, column, :null}, query), do: ["is-null", build_expression_tree(column, query)]

  defp filter_to_tree({condition, lhs, rhs}, query),
    do: [Atom.to_string(condition), build_expression_tree(lhs, query), build_expression_tree(rhs, query)]

  defp stringify(string) when is_binary(string), do: string
  defp stringify(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp stringify(function) when is_function(function), do: inspect(function)

  defp sql_dialect_name(data_source) do
    case Cloak.DataSource.sql_dialect_module(data_source) do
      nil ->
        nil

      dialect_module ->
        dialect_module
        |> to_string()
        |> String.split(".")
        |> List.last()
        |> String.downcase()
    end
  end
end
