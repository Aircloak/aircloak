defmodule Cloak.Sql.Query.Features do
  @moduledoc false

  alias Cloak.Sql.{Function, Expression, Query, Condition}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def features(query) do
    %{
      num_top_level_dimensions: length(query.column_titles),
      num_db_columns: num_db_columns(query.columns),
      num_tables: num_tables(query),
      num_distinct_tables: num_distinct_tables(query),
      num_top_level_group_by: num_group_by(query),
      num_subquery_group_by: num_subquery_group_by(query),
      num_group_by: num_group_by(query) + num_subquery_group_by(query),
      functions: extract_functions(query),
      expressions: extract_expressions(query),
      where_conditions: extract_where_conditions(query.where),
      column_types: extract_column_types(query.columns),
      selected_types: selected_types(query.columns),
      parameter_types: Enum.map(Query.parameter_types(query), &stringify/1),
      driver: to_string(query.data_source.driver),
      driver_dialect: sql_dialect_name(query.data_source)
    }
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp selected_types(columns),
    do:
      columns
      |> Enum.map(&Function.type/1)
      |> Enum.map(&stringify/1)

  defp num_db_columns(columns),
    do:
      columns
      |> extract_columns()
      |> Enum.uniq_by(&Expression.semantic/1)
      |> Enum.reject(& &1.constant?)
      |> Enum.count()

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

  defp extract_functions(query),
    do:
      query
      |> get_in([
        Query.Lenses.all_queries()
        |> Query.Lenses.analyst_provided_expressions()
        |> Query.Lenses.all_expressions()
        |> Lens.filter(& &1.function?)
      ])
      |> Enum.map(&Function.readable_name(&1.function))
      |> Enum.uniq()

  defp extract_expressions(query),
    do:
      query
      |> get_in([Query.Lenses.analyst_provided_expressions()])
      |> Enum.map(&build_expression_tree(&1, query))
      |> Enum.map(&expression_tree_to_lisp/1)

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

  defp extract_where_conditions(clause),
    do:
      Query.Lenses.conditions()
      |> Lens.reject(&Condition.subject(&1).synthetic?)
      |> Lens.to_list(clause)
      |> Enum.map(&extract_where_condition/1)
      |> Enum.uniq()

  defp extract_where_condition({:not, {:comparison, _column, :=, _comparator}}), do: "<>"
  defp extract_where_condition({:not, something}), do: "not #{extract_where_condition(something)}"

  defp extract_where_condition({:comparison, _column, comparison, _comparator}), do: Atom.to_string(comparison)

  defp extract_where_condition({:is, _column, :null}), do: "null"

  defp extract_where_condition({condition, _column, _value_or_pattern}), do: Atom.to_string(condition)

  defp extract_column_types(columns),
    do:
      columns
      |> extract_columns()
      |> Enum.flat_map(&extract_column_type/1)
      |> Enum.uniq()
      |> Enum.map(&stringify/1)

  defp extract_column_type(%Expression{constant?: true, type: type}), do: [type]
  defp extract_column_type(%Expression{table: :unknown}), do: []

  defp extract_column_type(%Expression{table: %{columns: columns}, name: name}),
    do:
      columns
      |> Enum.filter(&(&1.name == name))
      |> Enum.map(& &1.type)

  defp extract_columns(columns), do: Enum.flat_map(columns, &extract_column/1)

  defp extract_column({:distinct, value}), do: extract_column(value)
  defp extract_column(%Expression{function?: true, function_args: [:*]}), do: []

  defp extract_column(%Expression{function?: true, function_args: args}), do: extract_columns(args)

  defp extract_column(%Expression{} = column), do: [column]

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
