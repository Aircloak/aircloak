defmodule Cloak.Sql.Compiler.Validation do
  @moduledoc "Methods for query validation."

  alias Cloak.CyclicGraph
  alias Cloak.Sql.{CompilationError, Expression, Function, Query, Condition}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Query.Lenses

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Checks that the query specification respects the SQL standard."
  @spec verify_standard_restrictions(Query.t()) :: Query.t()
  def verify_standard_restrictions(%Query{command: :show} = query), do: query

  def verify_standard_restrictions(%Query{command: :select} = query) do
    Helpers.each_subquery(query, &verify_standard_functions_usage/1)
    Helpers.each_subquery(query, &verify_duplicate_tables/1)
    Helpers.each_subquery(query, &verify_aggregated_columns/1)
    Helpers.each_subquery(query, &verify_aggregators/1)
    Helpers.each_subquery(query, &verify_group_by_functions/1)
    Helpers.each_subquery(query, &verify_standard_joins/1)
    Helpers.each_subquery(query, &verify_where/1)
    Helpers.each_subquery(query, &verify_having/1)
    Helpers.each_subquery(query, &verify_limit/1)
    Helpers.each_subquery(query, &verify_offset/1)
    Helpers.each_subquery(query, &verify_in/1)
    Helpers.each_subquery(query, &verify_division_by_zero/1)
    query
  end

  @doc "Checks that the query specification respects the anonymization restrictions."
  @spec verify_anonymization_restrictions(Query.t()) :: Query.t()
  def verify_anonymization_restrictions(%Query{command: :show} = query), do: query

  def verify_anonymization_restrictions(%Query{command: :select} = query) do
    verify_user_id_usage(query, nil)
    Helpers.each_subquery(query, &verify_anonymization_functions_usage/1)
    Helpers.each_subquery(query, &verify_anonymization_joins/1)
    Helpers.each_subquery(query, &verify_sample_rate/1)
    query
  end

  # -------------------------------------------------------------------
  # Duplicate tables
  # -------------------------------------------------------------------

  defp verify_duplicate_tables(query) do
    with [duplicate_table | _] <- duplicate_tables(query) do
      raise CompilationError, message: "Table name `#{duplicate_table}` specified more than once."
    end
  end

  defp duplicate_tables(query),
    do:
      query.selected_tables
      |> Enum.group_by(& &1.name)
      |> Enum.reject(&match?({_name, [_]}, &1))
      |> Enum.map(fn {name, _} -> name end)

  # -------------------------------------------------------------------
  # Columns and expressions
  # -------------------------------------------------------------------

  defp verify_standard_functions_usage(query) do
    Lenses.query_expressions()
    |> Lens.filter(& &1.function?)
    |> Lens.to_list(query)
    |> Enum.map(&verify_standard_function_usage/1)
  end

  defp verify_standard_function_usage(%Expression{function: name, function_args: args} = expression) do
    if not Function.aggregator?(name) and match?([{:distinct, _}], args),
      do:
        raise(
          CompilationError,
          source_location: expression.source_location,
          message: "`DISTINCT` specified in non-aggregating function `#{Function.readable_name(name)}`."
        )

    :ok
  end

  defp verify_anonymization_functions_usage(query) do
    Lenses.query_expressions()
    |> Lens.filter(& &1.function?)
    |> Lens.to_list(query)
    |> Enum.map(&verify_anonymization_function_usage(&1, query))
  end

  defp verify_anonymization_function_usage(%Expression{function: name} = expression, query) do
    if query.type == :anonymized and name in ~w(min max median) and
         expression.function_args |> Enum.at(0) |> Function.type() == :text do
      raise(
        CompilationError,
        source_location: expression.source_location,
        message: """
        Aggregator `#{name}` is not allowed over arguments of type `text` in anonymizing contexts.
        For more information see the "Text operations" subsection of the "Restrictions" section in the user guides.
        """
      )
    end

    if Function.has_attribute?(name, {:not_in, query.type}) do
      raise(
        CompilationError,
        source_location: expression.source_location,
        message: "Function `#{Function.readable_name(name)}` is not allowed in `#{query.type}` subqueries."
      )
    end

    if Function.internal?(name) do
      raise(
        CompilationError,
        source_location: expression.source_location,
        message: "Function `#{Function.readable_name(name)}` can only be used internally."
      )
    end

    :ok
  end

  defp verify_aggregated_columns(query) do
    case invalid_individual_columns(query) do
      [] ->
        :ok

      [column = %{source_location: location} | _rest] ->
        raise CompilationError,
          source_location: location,
          message:
            "Column #{Expression.display_name(column)} needs " <>
              "to appear in the `GROUP BY` clause or be used in an aggregate function."
    end
  end

  defp invalid_individual_columns(query) do
    if Helpers.aggregates?(query) or Helpers.group_by?(query) do
      query
      |> Query.bucket_columns()
      |> Enum.reject(& &1.synthetic?)
      |> Enum.flat_map(&invalid_columns_in_aggregate(query, &1))
    else
      []
    end
  end

  defp valid_expression_in_aggregate?(query, column) do
    cond do
      Expression.constant?(column) ->
        true

      Expression.member?(query.group_by, column) ->
        true

      column.function? ->
        column.aggregate? or Enum.all?(column.function_args, &valid_expression_in_aggregate?(query, &1))

      true ->
        false
    end
  end

  defp invalid_columns_in_aggregate(_query, :*), do: []
  defp invalid_columns_in_aggregate(query, {:distinct, column}), do: invalid_columns_in_aggregate(query, column)

  defp invalid_columns_in_aggregate(query, expression) do
    cond do
      valid_expression_in_aggregate?(query, expression) -> []
      expression.function? -> Enum.flat_map(expression.function_args, &invalid_columns_in_aggregate(query, &1))
      true -> [expression]
    end
  end

  defp verify_aggregators(query) do
    query
    |> Query.bucket_columns()
    |> Enum.flat_map(&aggregate_subexpressions/1)
    |> Enum.filter(&(&1 |> aggregate_subexpressions() |> Enum.count() > 1))
    |> case do
      [] ->
        :ok

      [column = %{source_location: location} | _rest] ->
        raise CompilationError,
          source_location: location,
          message: "Expression `#{Expression.display(column)}` recursively calls multiple aggregators."
    end
  end

  defp verify_group_by_functions(query) do
    query.group_by
    |> Enum.filter(&aggregate_expression?/1)
    |> case do
      [] ->
        :ok

      [expression | _] ->
        [aggregate | _] = aggregate_subexpressions(expression)

        raise CompilationError,
          source_location: aggregate.source_location,
          message:
            "Aggregate function `#{Function.readable_name(aggregate.function)}` can not be used in the `GROUP BY` clause."
    end
  end

  # -------------------------------------------------------------------
  # Joins
  # -------------------------------------------------------------------

  defp verify_standard_joins(query) do
    verify_join_types(query)
    verify_join_conditions_scope(query.from, [])
  end

  defp verify_anonymization_joins(query) do
    verify_all_tables_are_connected_in_joins(query)
  end

  defp verify_all_tables_are_connected_in_joins(%Query{type: :standard}), do: :ok

  defp verify_all_tables_are_connected_in_joins(query),
    do:
      CyclicGraph.with(fn graph ->
        Enum.each(query.selected_tables, &CyclicGraph.add_vertex(graph, &1.name))
        conditions = Lens.to_list(Query.Lenses.conditions(), query.where) ++ Helpers.all_join_conditions(query)

        for {:comparison, column1, :=, column2} <- conditions,
            matching_key_types?(column1, column2),
            column1 != column2 do
          CyclicGraph.connect!(graph, column1.table.name, column2.table.name)
        end

        with [{table1, table2} | _] <- CyclicGraph.disconnected_pairs(graph) do
          raise CompilationError,
            message:
              "There is no connection path using key match filters between the tables `#{table1}` and `#{table2}`. " <>
                "Connect the two tables by adding `table1.key1 = table2.key2` conditions " <>
                "to the `WHERE` or `ON` clauses in the query."
        end
      end)

  defp matching_key_types?(column1, column2) do
    case {Expression.key_type(column1), Expression.key_type(column2)} do
      {nil, nil} -> false
      {type, type} -> true
      {_type1, _type2} -> false
    end
  end

  defp verify_join_conditions_scope({:join, join}, selected_tables) do
    selected_tables = verify_join_conditions_scope(join.lhs, selected_tables)
    selected_tables = verify_join_conditions_scope(join.rhs, selected_tables)

    Lens.each(Lenses.conditions_terminals(), join.conditions, fn
      %Cloak.Sql.Expression{
        table: %{name: table_name},
        name: column_name,
        source_location: location
      } ->
        verify_scope(selected_tables, table_name, column_name, location)

      {:identifier, table_name, {_, column_name}, location} ->
        verify_scope(selected_tables, table_name, column_name, location)

      _ ->
        :ok
    end)

    verify_where_clauses(join.conditions)
    selected_tables
  end

  defp verify_join_conditions_scope({:subquery, subquery}, selected_tables), do: [subquery.alias | selected_tables]

  defp verify_join_conditions_scope(table_name, selected_tables) when is_binary(table_name),
    do: [table_name | selected_tables]

  defp verify_scope(tables_in_scope, table_name, column_name, location) do
    unless Enum.member?(tables_in_scope, table_name),
      do:
        raise(
          CompilationError,
          source_location: location,
          message: "Column `#{column_name}` of table `#{table_name}` is used out of scope."
        )
  end

  defp verify_join_types(query) do
    Lenses.joins()
    |> Lens.filter(&(&1.type == :full_outer_join))
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

  defp verify_condition_tree({:or, _, _}),
    do:
      raise(
        CompilationError,
        message:
          "Combining conditions with `OR` is not allowed. Note that an `OR` condition may " <>
            "arise when negating an `AND` condition. For example `NOT (x = 1 AND y = 2)` is equivalent to " <>
            "`x <> 1 OR y <> 2`."
      )

  defp verify_condition_tree({:and, lhs, rhs}) do
    verify_condition_tree(lhs)
    verify_condition_tree(rhs)
  end

  defp verify_condition_tree(_), do: :ok

  defp verify_where_clauses(clauses) do
    verify_condition_tree(clauses)
    verify_conditions(clauses)

    Lenses.conditions_terminals()
    |> Lens.to_list(clauses)
    |> Enum.filter(&aggregate_expression?/1)
    |> case do
      [] ->
        :ok

      [column | _rest] ->
        raise CompilationError,
          source_location: column.source_location,
          message: "Expression #{Expression.display_name(column)} is not valid in the `WHERE` clause."
    end
  end

  defp verify_conditions(clauses),
    do:
      Lenses.conditions()
      |> Lens.to_list(clauses)
      |> Enum.each(&verify_condition/1)

  defp verify_condition({:comparison, column_a, comparator, column_b}) do
    verify_where_condition_types(column_a, column_b)
    check_for_string_inequalities(comparator, column_b)
  end

  defp verify_condition({verb, column, _}) when verb in [:like, :ilike] do
    if column.type != :text do
      verb = verb |> to_string() |> String.upcase()

      raise CompilationError,
        source_location: column.source_location,
        message:
          "Column #{Expression.display_name(column)} of type `#{column.type}` cannot be used in a #{verb} expression."
    end
  end

  defp verify_condition({:not, condition}), do: verify_condition(condition)
  defp verify_condition(_), do: :ok

  defp verify_where_condition_types(column_a, column_b) do
    unless comparable?(column_a.type, column_b.type) do
      raise CompilationError,
        source_location: column_a.source_location,
        message:
          "Column #{Expression.display_name(column_a)} of type `#{column_a.type}` and " <>
            "column #{Expression.display_name(column_b)} of type `#{column_b.type}` cannot be compared."
    end
  end

  defp comparable?(:integer, :real), do: true
  defp comparable?(:real, :integer), do: true
  defp comparable?(type1, type2), do: type1 == type2

  defp check_for_string_inequalities(comparator, %Expression{
         type: :text,
         source_location: location
       })
       when comparator in [:>, :>=, :<, :<=],
       do:
         raise(
           CompilationError,
           source_location: location,
           message: "Inequalities on string values are currently not supported."
         )

  defp check_for_string_inequalities(_, _), do: :ok

  defp verify_having(query) do
    verify_condition_tree(query.having)
    verify_conditions(query.having)

    for condition <- Lens.to_list(Query.Lenses.conditions(), query.having),
        term <- Condition.targets(condition),
        not valid_expression_in_aggregate?(query, term),
        do:
          raise(
            CompilationError,
            source_location: term.source_location,
            message: "`HAVING` clause can not be applied over column #{Expression.display_name(term)}."
          )
  end

  defp verify_limit(%Query{order_by: [], limit: amount}) when amount != nil,
    do:
      raise(
        CompilationError,
        message: "Using the `LIMIT` clause requires the `ORDER BY` clause to be specified."
      )

  defp verify_limit(_query), do: :ok

  defp verify_offset(%Query{order_by: [], offset: amount}) when amount > 0,
    do:
      raise(
        CompilationError,
        message: "Using the `OFFSET` clause requires the `ORDER BY` clause to be specified."
      )

  defp verify_offset(%Query{offset: offset, limit: nil, type: :restricted}) when offset > 0,
    do:
      raise(
        CompilationError,
        message: "`OFFSET` clause requires a `LIMIT` clause in `restricted` subqueries."
      )

  defp verify_offset(_query), do: :ok

  defp verify_sample_rate(%Query{sample_rate: amount})
       when is_integer(amount) and (amount < 0 or amount > 100),
       do:
         raise(
           CompilationError,
           message: "The `SAMPLE` clause expects an integer value between 1 and 100."
         )

  defp verify_sample_rate(_query), do: :ok

  # -------------------------------------------------------------------
  # IN verification
  # -------------------------------------------------------------------

  defp verify_in(query) do
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.filter(&Condition.in?/1)
    |> Lens.to_list(query)
    |> Enum.each(&do_verify_in/1)
  end

  defp do_verify_in(in_condition) do
    verify_in_rhs_constant(in_condition)
    verify_in_types(in_condition)
  end

  defp verify_in_rhs_constant({:in, _, items}) do
    items
    |> Enum.find(&(not Expression.constant?(&1)))
    |> case do
      nil ->
        :ok

      item ->
        raise CompilationError,
          message: "Only constants are allowed on the right-hand side of the `IN` operator.",
          source_location: item.source_location
    end
  end

  defp verify_in_types({:in, lhs, items}) do
    items
    |> Enum.find(&(&1.type != lhs.type))
    |> case do
      nil ->
        :ok

      item ->
        raise CompilationError,
          message: "Expected a constant of type `#{lhs.type}`, got `#{item.type}`.",
          source_location: item.source_location
    end
  end

  # -------------------------------------------------------------------
  # UserId usage
  # -------------------------------------------------------------------

  defp verify_user_id_usage(query, alias) do
    unless valid_user_id?(query), do: raise(CompilationError, missing_uid_error_message(query, alias))

    Lens.each(
      Lenses.direct_subqueries(),
      query,
      &verify_user_id_usage(&1.ast, &1.alias)
    )
  end

  defp valid_user_id?(%Query{type: :restricted} = query), do: Helpers.uid_column_selected?(query)

  defp valid_user_id?(%Query{type: :anonymized} = query),
    do: not Enum.empty?(Helpers.all_id_columns_from_tables(query))

  defp valid_user_id?(%Query{type: :standard}), do: true

  defp missing_uid_error_message(query, alias) do
    suggested_fix_message =
      Helpers.all_id_columns_from_tables(query)
      |> Enum.map(&Expression.display_name/1)
      |> case do
        [] -> "a table or subquery containing a `user id` key column to the `FROM` clause"
        [column] -> "the column #{column} to the `SELECT` clause"
        columns -> "one of the columns #{Enum.join(columns, ", ")} to the `SELECT` clause"
      end

    error_location =
      case alias do
        nil -> "tables referenced by the top-level query"
        _ -> "select list of subquery `#{alias}`"
      end

    "Missing a `user id` key column in the #{error_location}. To fix this error, add #{suggested_fix_message}."
  end

  # -------------------------------------------------------------------
  # Division by zero
  # -------------------------------------------------------------------

  defp verify_division_by_zero(query) do
    Lens.each(Lenses.query_expressions(), query, fn
      %{function: "/", function_args: [_, divisor]} ->
        if Expression.constant?(divisor) and Expression.const_value(divisor) == 0 do
          raise CompilationError, message: "Division by zero.", source_location: divisor.source_location
        end

      _ ->
        :ok
    end)
  end

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp aggregate_expression?(expression), do: not Enum.empty?(aggregate_subexpressions(expression))

  defp aggregate_subexpressions(expression),
    do:
      Query.Lenses.all_expressions()
      |> Lens.filter(&match?(%{aggregate?: true}, &1))
      |> Lens.to_list(expression)
end
