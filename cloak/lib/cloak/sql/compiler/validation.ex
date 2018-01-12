defmodule Cloak.Sql.Compiler.Validation do
  @moduledoc "Methods for query validation."

  alias Cloak.CyclicGraph
  alias Cloak.Sql.{CompilationError, Expression, Function, Query, Condition}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Query.Lenses


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Checks that the query specification is valid."
  @spec verify_query(Query.t) :: Query.t
  def verify_query(%Query{command: :show} = query), do: query
  def verify_query(%Query{command: :select} = query) do
    verify_duplicate_tables(query)
    verify_aggregated_columns(query)
    verify_group_by_functions(query)
    verify_non_selected_where_splitters(query)
    verify_joins(query)
    verify_where(query)
    verify_having(query)
    verify_limit(query)
    verify_offset(query)
    verify_sample_rate(query)
    query
  end

  @doc "Checks that a function specification is valid."
  @spec verify_function(Function.t, boolean) :: Function.t
  def verify_function(function, subquery?) do
    verify_function_exists(function)
    verify_function_usage(function, subquery?)
    function
  end

  @doc "Checks that the subquery specification is valid."
  @spec verify_subquery(Query.t, String.t) :: Query.t
  def verify_subquery(subquery, alias) do
    verify_subquery_uid(subquery, alias)
    verify_subquery_offset(subquery, alias)
    subquery
  end


  # -------------------------------------------------------------------
  # Duplicate tables
  # -------------------------------------------------------------------

  defp verify_duplicate_tables(query) do
    with [duplicate_table | _] <- duplicate_tables(query) do
      raise CompilationError, message: "Table name `#{duplicate_table}` specified more than once."
    end
  end

  defp duplicate_tables(query), do:
    query.selected_tables
    |> Enum.group_by(&(&1.name))
    |> Enum.reject(&match?({_name, [_]}, &1))
    |> Enum.map(fn({name, _}) -> name end)


  # -------------------------------------------------------------------
  # Columns and expressions
  # -------------------------------------------------------------------

  defp verify_function_exists(function = {_, name, _}) do
    unless Function.exists?(function) do
      case Function.deprecation_info(function) do
        {:error, :not_found} ->
          raise CompilationError, message: "Unknown function `#{Function.readable_name(name)}`."
        {:ok, %{alternative: alternative}} ->
          raise CompilationError, message: "Function `#{Function.readable_name(name)}` has been deprecated. " <>
            "Depending on your use case, consider using `#{Function.readable_name(alternative)}` instead."
      end
    end
  end

  defp verify_function_usage({:function, name, [argument]}, _subquery? = false) when name in ["min", "max", "median"] do
    if Function.type(argument) == :text, do:
      raise CompilationError, message:
        "Function `#{name}` is allowed over arguments of type `text` only in subqueries."
    :ok
  end
  defp verify_function_usage({:function, name, _}, _subquery? = true) do
    if Function.has_attribute?(name, :not_in_subquery), do:
      raise CompilationError, message: "Function `#{name}` is not allowed in subqueries."
    :ok
  end
  defp verify_function_usage(_function, _subquery?), do: :ok

  defp verify_aggregated_columns(query) do
    case invalid_individual_columns(query) do
      [] -> :ok
      [column | _rest] ->
        raise CompilationError, message: "#{aggregated_expression_display(column)} " <>
          "to appear in the `GROUP BY` clause or be used in an aggregate function."
    end
  end

  defp invalid_individual_columns(query), do:
    if Helpers.aggregate?(query),
      do: query |> Query.bucket_columns() |> Enum.filter(&individual_column?(query, &1)),
      else: []

  defp individual_column?(query, column), do:
    not Expression.constant?(column) and not Helpers.aggregated_column?(query, column)

  defp aggregated_expression_display({:function, _function, [arg]}), do:
    "Column `#{arg.name}` needs"
  defp aggregated_expression_display({:function, _function, args}), do:
    "Columns (#{args |> Enum.map(&"`#{&1.name}`") |> Enum.join(", ")}) need"
  defp aggregated_expression_display(%Expression{function: fun, function_args: args}) when fun != nil do
    [column | _] = for %Expression{constant?: false} = column <- args, do: column
    aggregated_expression_display(column)
  end
  defp aggregated_expression_display(%Expression{table: table, name: name}), do:
    "Column `#{name}` from table `#{table.name}` needs"

  defp verify_group_by_functions(query) do
    query.group_by
    |> Enum.filter(&aggregate_expression?/1)
    |> case do
      [] -> :ok
      [expression | _] ->
        [aggregate | _] = aggregate_subexpressions(expression)
        raise CompilationError, message:
          "Aggregate function `#{Function.readable_name(aggregate.function)}` can not be used in the `GROUP BY` clause."
    end
  end

  defp verify_non_selected_where_splitters(query) do
    non_selected_where_splitters =
      MapSet.difference(
        MapSet.new(Query.outermost_where_splitters(query)),
        MapSet.new(Query.outermost_selected_splitters(query))
      )

    if MapSet.size(non_selected_where_splitters) > 0 do
      raise CompilationError, message:
        "Row splitter functions used in the `WHERE`-clause have to be used identically in the `SELECT`-clause first."
    end

    query
  end


  # -------------------------------------------------------------------
  # Joins
  # -------------------------------------------------------------------

  defp verify_joins(%Query{projected?: true} = query), do: query
  defp verify_joins(query) do
    verify_join_types(query)
    verify_join_conditions_scope(query.from, [])
    verify_all_joined_subqueries_have_explicit_uids(query)
    verify_all_uid_columns_are_compared_in_joins(query)
  end

  defp verify_all_joined_subqueries_have_explicit_uids(query) do
    Lens.each(
      Lenses.joined_subqueries(),
      query,
      fn(joined_subquery) ->
        unless Enum.any?(joined_subquery.ast.columns, &(&1.user_id? && &1.visible?)), do:
          raise CompilationError,
            message: "There is no user id column in the subquery `#{joined_subquery.alias}`."
      end
    )
  end

  defp verify_all_uid_columns_are_compared_in_joins(query), do:
    CyclicGraph.with(fn(graph) ->
      query
      |> Helpers.all_id_columns_from_tables()
      |> Enum.each(&CyclicGraph.add_vertex(graph, {&1.table.name, &1.name}))

      conditions =
        Lens.to_list(Query.Lenses.conditions(), query.where) ++
        Helpers.all_join_conditions(query)
      for {:comparison, column1, :=, column2} <- conditions, column1.user_id?, column2.user_id?, column1 != column2 do
        CyclicGraph.connect!(graph, {column1.table.name, column1.name}, {column2.table.name, column2.name})
      end

      with [{{table1, column1}, {table2, column2}} | _] <- CyclicGraph.disconnected_pairs(graph) do
        raise CompilationError,
          message:
            "Missing where comparison for uid columns of tables `#{table1}` and `#{table2}`. " <>
            "You can fix the error by adding `#{table1}.#{column1} = #{table2}.#{column2}` " <>
            "condition to the `WHERE` clause."
      end
    end)

  defp verify_join_conditions_scope({:join, join}, selected_tables) do
    selected_tables = verify_join_conditions_scope(join.lhs, selected_tables)
    selected_tables = verify_join_conditions_scope(join.rhs, selected_tables)

    Lens.each(
      Lenses.conditions_terminals(),
      join.conditions,
      fn
        (%Cloak.Sql.Expression{table: %{name: table_name}, name: column_name}) ->
          verify_scope(selected_tables, table_name, column_name)
        ({:identifier, table_name, {_, column_name}}) ->
          verify_scope(selected_tables, table_name, column_name)
        (_) -> :ok
      end
    )

    verify_where_clauses(join.conditions)
    selected_tables
  end
  defp verify_join_conditions_scope({:subquery, subquery}, selected_tables),
    do: [subquery.alias | selected_tables]
  defp verify_join_conditions_scope(table_name, selected_tables) when is_binary(table_name),
    do: [table_name | selected_tables]

  defp verify_scope(tables_in_scope, table_name, column_name) do
    unless Enum.member?(tables_in_scope, table_name), do:
      raise CompilationError, message: "Column `#{column_name}` of table `#{table_name}` is used out of scope."
  end

  defp verify_join_types(query) do
    Lenses.joins()
    |> Lens.satisfy(& &1.type == :full_outer_join)
    |> Lens.to_list(query)
    |> case do
      [] -> :ok
      _ -> raise CompilationError, message: "FULL OUTER JOINs are not currently allowed."
    end
  end


  # -------------------------------------------------------------------
  # Where, having, limit, offset, sample
  # -------------------------------------------------------------------

  defp verify_where(query), do: verify_where_clauses(query.where)

  defp verify_condition_tree({:or, _, _}), do:
    raise CompilationError, message:
      "Combining conditions with `OR` is not allowed. Note that an `OR` condition may " <>
      "arise when negating an `AND` condition. For example `NOT (x = 1 AND y = 2)` is equivalent to " <>
      "`x <> 1 OR y <> 2`."
  defp verify_condition_tree({:and, lhs, rhs}) do
    verify_condition_tree(lhs)
    verify_condition_tree(rhs)
  end
  defp verify_condition_tree(_), do: :ok

  defp verify_where_clauses(clauses) do
    verify_condition_tree(clauses)

    Lenses.conditions()
    |> Lens.to_list(clauses)
    |> Enum.each(&verify_where_condition/1)

    Lenses.conditions_terminals()
    |> Lens.to_list(clauses)
    |> Enum.filter(&aggregate_expression?/1)
    |> case do
      [] -> :ok
      [column | _rest] ->
        raise CompilationError, message:
          "Expression #{Expression.display_name(column)} is not valid in the `WHERE` clause."
    end
  end

  defp verify_where_condition({:comparison, column_a, comparator, column_b}) do
    verify_where_condition_types(column_a, column_b)
    check_for_string_inequalities(comparator, column_b)
  end
  defp verify_where_condition({verb, column, _}) when verb in [:like, :ilike] do
    if column.type != :text do
      verb = verb |> to_string() |> String.upcase()
      raise CompilationError, message:
        "Column #{Expression.display_name(column)} of type `#{column.type}` cannot be used in a #{verb} expression."
    end
  end
  defp verify_where_condition({:not, condition}), do: verify_where_condition(condition)
  defp verify_where_condition(_), do: :ok

  defp verify_where_condition_types(column_a, column_b) do
    unless comparable?(column_a.type, column_b.type) do
      raise CompilationError, message: "Column #{Expression.display_name(column_a)} of type `#{column_a.type}` and "
        <> "column #{Expression.display_name(column_b)} of type `#{column_b.type}` cannot be compared."
    end
  end

  defp comparable?(:integer, :real), do: true
  defp comparable?(:real, :integer), do: true
  defp comparable?(type1, type2), do: type1 == type2

  defp check_for_string_inequalities(comparator, %Expression{type: :text}) when comparator in [:>, :>=, :<, :<=], do:
    raise CompilationError, message: "Inequalities on string values are currently not supported."
  defp check_for_string_inequalities(_, _), do: :ok

  defp verify_having(query) do
    verify_condition_tree(query.having)

    for condition <- Lens.to_list(Query.Lenses.conditions(), query.having),
        term <- Condition.targets(condition), individual_column?(query, term), do:
      raise CompilationError,
        message: "`HAVING` clause can not be applied over column #{Expression.display_name(term)}."
  end

  defp verify_limit(%Query{order_by: [], limit: amount}) when amount != nil, do:
    raise CompilationError, message: "Using the `LIMIT` clause requires the `ORDER BY` clause to be specified."
  defp verify_limit(_query), do: :ok

  defp verify_offset(%Query{order_by: [], offset: amount}) when amount > 0, do:
    raise CompilationError, message: "Using the `OFFSET` clause requires the `ORDER BY` clause to be specified."
  defp verify_offset(_query), do: :ok

  defp verify_sample_rate(%Query{sample_rate: amount}) when is_integer(amount) and (amount < 0 or amount > 100), do:
    raise CompilationError, message: "The `SAMPLE` clause expects an integer value between 1 and 100."
  defp verify_sample_rate(_query), do: :ok


  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp verify_subquery_uid(subquery, alias) do
    unless Helpers.uid_column_selected?(subquery) do
      possible_uid_columns =
        Helpers.all_id_columns_from_tables(subquery)
        |> Enum.map(&Expression.display_name/1)
        |> case do
          [column] -> "the column #{column}"
          columns -> "one of the columns #{Enum.join(columns, ", ")}"
        end

      raise CompilationError, message:
        "Missing a user id column in the select list of #{"subquery `#{alias}`"}. " <>
        "To fix this error, add #{possible_uid_columns} to the subquery select list."
    end
  end

  defp verify_subquery_offset(%{offset: offset, limit: limit}, alias) when is_nil(limit) and offset > 0, do:
    raise CompilationError, message: "Subquery `#{alias}` has an OFFSET clause without a LIMIT clause."
  defp verify_subquery_offset(subquery, _), do: subquery


  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp aggregate_expression?(expression), do:
    not Enum.empty?(aggregate_subexpressions(expression))

  defp aggregate_subexpressions(expression), do:
    Query.Lenses.all_expressions()
    |> Lens.satisfy(&match?(%{aggregate?: true}, &1))
    |> Lens.to_list(expression)
end
