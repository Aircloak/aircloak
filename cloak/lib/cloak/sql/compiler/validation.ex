defmodule Cloak.Sql.Compiler.Validation do
  @moduledoc "Methods for query validation."

  alias Cloak.CyclicGraph
  alias Cloak.Sql.{CompilationError, Expression, Function, Query, Condition, Function}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Query.Lenses
  alias Cloak.DataSource.Shadows

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Checks that the query specification respects the SQL standard."
  @spec verify_standard_restrictions(Query.t()) :: Query.t()
  def verify_standard_restrictions(%Query{command: :show} = query), do: query

  def verify_standard_restrictions(query) do
    Helpers.each_subquery(query, &verify_standard_functions_usage/1)
    Helpers.each_subquery(query, &verify_duplicate_tables/1)
    Helpers.each_subquery(query, &verify_aggregated_columns/1)
    Helpers.each_subquery(query, &verify_aggregators/1)
    Helpers.each_subquery(query, &verify_group_by_constants/1)
    Helpers.each_subquery(query, &verify_group_by_functions/1)
    Helpers.each_subquery(query, &verify_grouping_sets_uniqueness/1)
    Helpers.each_subquery(query, &verify_grouping_id_usage/1)
    Helpers.each_subquery(query, &verify_grouping_id_args/1)
    Helpers.each_subquery(query, &verify_standard_joins/1)
    Helpers.each_subquery(query, &verify_where/1)
    Helpers.each_subquery(query, &verify_having/1)
    Helpers.each_subquery(query, &verify_limit/1)
    Helpers.each_subquery(query, &verify_offset/1)
    Helpers.each_subquery(query, &verify_in/1)
    Helpers.each_subquery(query, &verify_division_by_zero/1)
    Helpers.each_subquery(query, &verify_constants/1)
    Helpers.each_subquery(query, &verify_union_columns/1)
    query
  end

  @doc "Checks that the query specification respects the anonymization restrictions."
  @spec verify_anonymization_restrictions(Query.t(), Keyword.t()) :: Query.t()
  def verify_anonymization_restrictions(query, opts \\ [])

  def verify_anonymization_restrictions(%Query{command: :show} = query, _opts), do: query

  def verify_anonymization_restrictions(query, opts) do
    verify_user_id_usage(query, nil)
    Helpers.each_subquery(query, &verify_unions_usage/1)
    Helpers.each_subquery(query, &verify_anonymization_functions_usage/1)
    Helpers.each_subquery(query, &verify_conditions_usage/1)
    Helpers.each_subquery(query, &verify_or_usage/1)
    Helpers.each_subquery(query, &verify_case_usage/1)
    Helpers.each_subquery(query, &verify_anonymization_joins/1)
    Helpers.each_subquery(query, &verify_grouping_sets_uid/1)
    Helpers.each_subquery(query, &verify_unselectable_columns_filtering/1)

    if Keyword.get(opts, :analyst_table_compilation?, false) do
      Lens.each(Lenses.subqueries(), query, &verify_unselectable_columns_selection/1)
    else
      Helpers.each_subquery(query, &verify_unselectable_columns_selection/1)
    end

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
    |> Lens.filter(&Expression.function?/1)
    |> Lens.to_list(query)
    |> Enum.map(&verify_standard_function_usage/1)
  end

  defp verify_standard_function_usage(%Expression{kind: :function, name: name, args: args} = expression) do
    if match?([{:distinct, _}], args) and name not in ~w(count count_noise),
      do:
        raise(
          CompilationError,
          source_location: expression.source_location,
          message: "Only the `count` and `count_noise` functions support the `DISTINCT` modifier."
        )

    if name == "case", do: verify_case_arguments(expression.source_location, args)

    :ok
  end

  defp verify_case_arguments(source_location, args) do
    args
    |> Enum.take_every(2)
    |> Enum.reverse()
    |> Enum.drop(1)
    |> Enum.reject(&(&1.type == :boolean))
    |> case do
      [arg | _] ->
        raise(
          CompilationError,
          source_location: arg.source_location,
          message: "`case` expression requires a `boolean` argument for the test condition."
        )

      _ ->
        :ok
    end

    args
    |> Function.case_branches()
    |> Enum.map(& &1.type)
    |> Enum.uniq()
    |> Enum.reject(&is_nil/1)
    |> case do
      types when length(types) > 1 ->
        raise(
          CompilationError,
          source_location: source_location,
          message: "`case` expression requires that all branches return the same type."
        )

      _ ->
        :ok
    end
  end

  defp verify_anonymization_functions_usage(query) do
    Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.to_list(query)
    |> Enum.map(&verify_anonymization_function_usage(&1, query))
  end

  defp verify_anonymization_function_usage(%Expression{kind: :function, name: name} = expression, query) do
    if query.type == :anonymized and name in ~w(min max) and
         expression.args |> Enum.at(0) |> Function.type() == :text do
      raise(
        CompilationError,
        source_location: expression.source_location,
        message: """
        Aggregator `#{name}` is not allowed over arguments of type `text` in anonymizing contexts.
        For more information see the "Text operations" subsection of the "Restrictions" section in the user guides.
        """
      )
    end

    if query.type != :standard and Function.unsafe?(name) do
      raise(
        CompilationError,
        source_location: expression.source_location,
        message: "Function `#{Function.readable_name(name)}` can only be used in non-anonymizing queries."
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
            "Column `#{Expression.display(column)}` needs " <>
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

      Expression.function?(column) ->
        Function.aggregator?(column) or Enum.all?(column.args, &valid_expression_in_aggregate?(query, &1))

      true ->
        false
    end
  end

  defp invalid_columns_in_aggregate(_query, :*), do: []
  defp invalid_columns_in_aggregate(query, {:distinct, column}), do: invalid_columns_in_aggregate(query, column)

  defp invalid_columns_in_aggregate(query, expression) do
    cond do
      valid_expression_in_aggregate?(query, expression) -> []
      Expression.function?(expression) -> Enum.flat_map(expression.args, &invalid_columns_in_aggregate(query, &1))
      true -> [expression]
    end
  end

  defp verify_aggregators(query) do
    Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&Function.aggregator?/1)
    |> Lens.to_list(query)
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

  defp verify_constants(query) do
    Lenses.query_expressions()
    |> Lens.filter(&Expression.constant?/1)
    |> Lens.reject(&(&1.value == nil))
    |> Lens.to_list(query)
    |> Enum.map(&verify_constant/1)
  end

  defp verify_constant(%Expression{value: value, type: type} = expression) when type in [:integer, :real] do
    if abs(value) > :math.pow(10, Cloak.Math.numeric_max_scale()) do
      raise CompilationError,
        source_location: expression.source_location,
        message:
          "Constant expression is out of valid range: numeric values have to be inside the interval " <>
            "[-10^#{Cloak.Math.numeric_max_scale()}, 10^#{Cloak.Math.numeric_max_scale()}]."
    end
  end

  defp verify_constant(%Expression{value: value, type: type} = expression) when type in [:date, :datetime] do
    if value.year < Cloak.Time.year_lower_bound() or value.year > Cloak.Time.year_upper_bound() do
      raise CompilationError,
        source_location: expression.source_location,
        message:
          "Constant expression is out of valid range: date values have to be inside the interval " <>
            "[`#{Cloak.Time.year_lower_bound()}-01-01`, `#{Cloak.Time.year_upper_bound()}-12-31`]."
    end
  end

  @interval_constant_max_year 100
  defp verify_constant(%Expression{value: value, type: :interval} = expression) do
    if abs(Timex.Duration.to_days(value)) > @interval_constant_max_year * 365.25 do
      raise CompilationError,
        source_location: expression.source_location,
        message:
          "Constant expression is out of valid range: interval values have to be less than " <>
            "`#{@interval_constant_max_year}` years."
    end
  end

  defp verify_constant(_expression), do: :ok

  # -------------------------------------------------------------------
  # Unselectable columns
  # -------------------------------------------------------------------

  defp verify_unselectable_columns_selection(query) do
    do_verify_unselectable_columns(
      Lenses.unselectable_selected_columns(),
      query
    )
  end

  defp verify_unselectable_columns_filtering(query) do
    do_verify_unselectable_columns(
      Lenses.unselectable_filtering_columns(),
      query
    )
  end

  defp do_verify_unselectable_columns(lens, query) do
    lens
    |> Lens.to_list(query)
    |> case do
      [column | _] ->
        column_display = unselectable_column_display(column)
        source_column_display = unselectable_db_column(query, column) |> unselectable_column_display()

        message =
          if column_display == source_column_display do
            "Column #{column_display} cannot appear in this query context as it has been classified as unselectable by your system administrator."
          else
            "Column #{column_display} cannot appear in this query context as it depends on" <>
              " column #{source_column_display}, which has been classified as unselectable by your system administrator."
          end <>
            " Please consult the section on unselectable columns in the documentation."

        raise CompilationError,
          source_location: column.source_location,
          message: message

      _ ->
        :ok
    end
  end

  defp unselectable_column_display(%{name: name, table: table}),
    do: "`#{name}` from table `#{table.db_name || table.name}`"

  defp unselectable_db_column(query, column) do
    case Helpers.unselectable_db_columns(query, column) do
      [source_column | _] ->
        source_column

      _ ->
        column
    end
  end

  # -------------------------------------------------------------------
  # GROUP BY
  # -------------------------------------------------------------------

  defp verify_group_by_constants(query) do
    query.group_by
    |> Enum.filter(&Expression.constant?/1)
    |> case do
      [] ->
        :ok

      [expression | _] ->
        raise CompilationError,
          source_location: expression.source_location,
          message: "Constant expression `#{Expression.display(expression)}` can not be used in the `GROUP BY` clause."
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
            "Aggregate function `#{Function.readable_name(aggregate.name)}` can not be used in the `GROUP BY` clause."
    end
  end

  defp verify_grouping_sets_uniqueness(query) do
    expanded_grouping_sets =
      Enum.map(query.grouping_sets, fn grouping_set ->
        Enum.map(grouping_set, &(query.group_by |> Enum.at(&1) |> Expression.semantic()))
      end)

    case expanded_grouping_sets -- Enum.uniq(expanded_grouping_sets) do
      [] ->
        :ok

      _ ->
        raise CompilationError,
          message: "Duplicated grouping sets used in the `GROUP BY` clause."
    end
  end

  defp verify_grouping_sets_uid(%Query{type: :restricted} = query) do
    Enum.map(query.grouping_sets, fn grouping_set ->
      unless Enum.any?(grouping_set, &(query.group_by |> Enum.at(&1) |> Expression.key_type() == :user_id)) do
        raise CompilationError,
          message: "Grouping set in restricted query doesn't contain a user id."
      end
    end)
  end

  defp verify_grouping_sets_uid(_query), do: :ok

  defp verify_grouping_id_usage(query) do
    Lens.keys([:where, :group_by])
    |> Lenses.all_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name == "grouping_id"))
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [column | _] ->
        raise CompilationError,
          source_location: column.source_location,
          message: "Function `grouping_id` can not be used in the `WHERE` or `GROUP BY` clauses."
    end
  end

  defp verify_grouping_id_args(query) do
    Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name == "grouping_id"))
    |> Lens.to_list(query)
    |> Enum.each(fn %Expression{args: args} ->
      Enum.each(args, fn column ->
        unless Expression.member?(query.group_by, column) do
          raise CompilationError,
            source_location: column.source_location,
            message:
              "Arguments to function `grouping_id` have to be from the list of expressions used in the `GROUP BY` clause."
        end
      end)
    end)
  end

  # -------------------------------------------------------------------
  # Joins
  # -------------------------------------------------------------------

  defp verify_standard_joins(%Query{command: :union}), do: :ok

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

        for %Expression{kind: :function, name: "=", args: [column1, column2]} <- conditions,
            matching_key_types?(column1, column2),
            column1 != column2 do
          CyclicGraph.connect!(graph, column1.table.name, column2.table.name)
        end

        with [{table1, table2} | _] <- CyclicGraph.disconnected_pairs(graph) do
          raise CompilationError, message: unconnected_tables_error_message(query.selected_tables, table1, table2)
        end
      end)

  defp matching_key_types?(column1, column2) do
    case {Expression.key_type(column1), Expression.key_type(column2)} do
      {nil, nil} -> false
      {type, type} -> true
      {_type1, _type2} -> false
    end
  end

  defp unconnected_tables_error_message(selected_tables, table_name1, table_name2) do
    table1 = Enum.find(selected_tables, &(&1.name == table_name1))
    table2 = Enum.find(selected_tables, &(&1.name == table_name2))

    "The tables `#{table_name1}` and `#{table_name2}` are not joined using key match filters. " <>
      "Connect the two tables by adding `table1.key1 = table2.key2` conditions " <>
      "to the `WHERE` or `ON` clauses in the query. #{table_join_hint(table1)} #{table_join_hint(table2)}"
  end

  defp table_join_hint(table) do
    keys_hint =
      table.keys
      |> Enum.map(fn {column, type} -> "column `#{column}` of type `#{type}`" end)
      |> case do
        [] ->
          "has no keys through which it can be joined, either because the table has not been configured to allow " <>
            "joining with other tables with personal data, or in the case of a sub query, " <>
            "because you did not select the necessary key columns"

        [key_info] ->
          "has to be joined through #{key_info}"

        keys_info ->
          "can be joined through one of the following keys: #{Aircloak.OxfordComma.join(keys_info, "or")}"
      end

    "Table `#{table.name}` #{keys_hint}."
  end

  defp verify_join_conditions_scope({:join, join}, selected_tables) do
    selected_tables = verify_join_conditions_scope(join.lhs, selected_tables)
    selected_tables = verify_join_conditions_scope(join.rhs, selected_tables)

    Lens.each(Lenses.conditions_terminals(), join.condition, fn
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

    verify_where_clauses(join.condition)
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
  # Where, having, limit, offset
  # -------------------------------------------------------------------

  defp verify_where(query), do: verify_where_clauses(query.where)

  defp verify_where_clauses(clauses) do
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
          message: "Expression `#{Expression.display(column)}` is not valid in the `WHERE` clause."
    end
  end

  defp verify_conditions(clauses),
    do:
      Lenses.conditions()
      |> Lens.to_list(clauses)
      |> Enum.each(&verify_condition/1)

  defp verify_condition(%Expression{
         kind: :function,
         name: verb,
         args: [_subject, %Expression{type: type, source_location: location}]
       })
       when verb in ~w(< > <= >=) and type in [:text, :boolean] do
    raise(
      CompilationError,
      source_location: location,
      message: "Inequalities on `#{type}` values are currently not supported."
    )
  end

  defp verify_condition(%Expression{kind: :function, name: verb, args: [column, _pattern]})
       when verb in ~w(like ilike) do
    if column.type != :text do
      raise CompilationError,
        source_location: column.source_location,
        message:
          "Column `#{Expression.display(column)}` of type `#{column.type}` " <>
            "cannot be used in a #{String.upcase(verb)} expression."
    end
  end

  defp verify_condition(%Expression{
         kind: :function,
         name: "=",
         args: [%Expression{type: type, source_location: location}, %Expression{value: true}]
       })
       when type not in [:boolean, nil] do
    raise CompilationError,
      source_location: location,
      message: "Row filtering clauses have to be boolean expressions."
  end

  defp verify_condition(%Expression{kind: :function, name: "not", args: [condition]}), do: verify_condition(condition)
  defp verify_condition(_), do: :ok

  defp verify_having(query) do
    verify_conditions(query.having)

    for condition <- Lens.to_list(Query.Lenses.conditions(), query.having),
        term <- Condition.targets(condition),
        not valid_expression_in_aggregate?(query, term) do
      raise CompilationError,
        source_location: term.source_location,
        message:
          "Expression `#{Expression.display(term)}` has to appear in the `GROUP BY` clause " <>
            "or be used in an aggregate function."
    end
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

  # -------------------------------------------------------------------
  # IN verification
  # -------------------------------------------------------------------

  defp verify_in(query) do
    Query.Lenses.filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.filter(&Condition.in?/1)
    |> Lens.to_list(query)
    |> Enum.each(&do_verify_in/1)
  end

  defp do_verify_in(in_condition) do
    verify_in_rhs_constant(in_condition)
    verify_in_types(in_condition)
  end

  defp verify_in_rhs_constant(%Expression{kind: :function, name: "in", args: [_lhs | items]}) do
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

  defp verify_in_types(%Expression{kind: :function, name: "in", args: [lhs | items]}) do
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
  # User ID usage verification
  # -------------------------------------------------------------------

  defp verify_user_id_usage(query, alias) do
    unless valid_user_id?(query), do: raise(CompilationError, missing_uid_error_message(query, alias))

    verify_user_id_references(query)

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
      |> Enum.map(&"`#{&1.name}`")
      |> case do
        [] -> "join in one of the following tables: #{user_id_tables_hint(query.data_source.tables)}"
        [column] -> "add the column #{column} to the `SELECT` clause"
        columns -> "add one of the columns #{Aircloak.OxfordComma.join(columns, "or")} to the `SELECT` clause"
      end

    error_location =
      case alias do
        nil -> "tables selected by the top-level query"
        _ -> "select list of subquery `#{alias}`"
      end

    "Missing a `user id` key column in the #{error_location}. To fix this error, #{suggested_fix_message}."
  end

  defp user_id_tables_hint(tables) do
    user_id_tables =
      tables
      |> Enum.filter(fn {_name, table} -> table.user_id != nil end)
      |> Enum.map(fn {name, _table} -> "`#{name}`" end)

    if Enum.count(user_id_tables) > 5 do
      "#{user_id_tables |> Enum.take(5) |> Enum.join(", ")}, ... "
    else
      Aircloak.OxfordComma.join(user_id_tables, "or")
    end
  end

  defp verify_user_id_references(%Query{type: :anonymized} = query) do
    Lens.multiple([
      Lens.keys([:columns, :group_by]) |> Lens.all(),
      Lens.key(:order_by) |> Lens.all() |> Lens.at(0)
    ])
    |> Lens.filter(& &1.user_id?)
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [uid_expression | _rest] ->
        raise CompilationError,
          message:
            "Directly selecting or grouping on the user id column in an anonymizing query is not allowed, as it " <>
              "would lead to a fully censored result.\nAnonymizing queries can reference user id columns either " <>
              "by filtering on them, aggregating them, or processing them so that they have a chance to create " <>
              "anonymized buckets.",
          source_location: uid_expression.source_location
    end
  end

  defp verify_user_id_references(_query), do: :ok

  # -------------------------------------------------------------------
  # Division by zero
  # -------------------------------------------------------------------

  defp verify_division_by_zero(query) do
    Lens.each(Lenses.query_expressions(), query, fn
      %{kind: :function, name: "/", args: [_, divisor]} ->
        if Expression.constant?(divisor) and Expression.const_value(divisor) == 0 do
          raise CompilationError, message: "Division by zero.", source_location: divisor.source_location
        end

      _ ->
        :ok
    end)
  end

  # -------------------------------------------------------------------
  # Conditions usage verification
  # -------------------------------------------------------------------

  defp verify_conditions_usage(%Query{type: :standard}), do: :ok

  defp verify_conditions_usage(query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Function.condition?/1)
    |> Lens.to_list(query |> drop_case_conditions() |> drop_filtering_conditions())
    |> case do
      [] ->
        :ok

      [condition | _rest] ->
        raise CompilationError,
          message: "Illegal usage of a condition in an anonymizing or restricted query.",
          source_location: condition.source_location
    end
  end

  # -------------------------------------------------------------------
  # OR usage verification
  # -------------------------------------------------------------------

  defp verify_or_usage(%Query{type: :standard}), do: :ok

  defp verify_or_usage(query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name == "or"))
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [condition | _rest] ->
        raise CompilationError,
          message:
            "Combining boolean expressions with `OR` is not allowed in anonymizing queries. " <>
              "Note that an `OR` expression may arise when negating an `AND` expression. " <>
              "For example `NOT (x = 1 AND y = 2)` is equivalent to `x <> 1 OR y <> 2`.",
          source_location: condition.source_location
    end
  end

  # -------------------------------------------------------------------
  # CASE usage verification
  # -------------------------------------------------------------------

  defp verify_case_usage(%Query{type: :standard}), do: :ok

  defp verify_case_usage(%Query{type: :anonymized} = query) do
    verify_case_usage_in_filtering_clauses(query)
    verify_post_processing_of_case_expressions(query)
    verify_count_distinct_case_expressions(query)
    verify_aggregated_case_expressions_values(query)
    verify_bucketed_case_expressions_values(query)
    verify_case_expressions_conditions(query)
  end

  defp verify_case_usage(%Query{type: :restricted} = query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name == "case"))
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [case_expression | _rest] ->
        raise CompilationError,
          message: "`case` expressions can not be used in restricted queries.",
          source_location: case_expression.source_location
    end
  end

  defp verify_post_processing_of_case_expressions(%Query{type: :anonymized} = query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.reject(&Function.aggregator?/1)
    |> Lens.filter(fn expression ->
      Enum.any?(expression.args, &match?(%Expression{kind: :function, name: "case"}, &1))
    end)
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [post_processing_expression | _rest] ->
        raise CompilationError,
          message: "Post-processing a `case` expression in an anonymizing query is not allowed.",
          source_location: post_processing_expression.source_location
    end
  end

  defp verify_case_usage_in_filtering_clauses(%Query{type: :anonymized} = query) do
    Query.Lenses.pre_anonymization_filter_clauses()
    |> Query.Lenses.all_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name == "case"))
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [case_expression | _rest] ->
        raise CompilationError,
          message: "`case` expressions can not be used in filtering clauses in an anonymizing query.",
          source_location: case_expression.source_location
    end
  end

  defp verify_count_distinct_case_expressions(%Query{type: :anonymized} = query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name == "count"))
    |> Lens.key(:args)
    |> Lens.all()
    |> Lens.filter(&match?({:distinct, _}, &1))
    |> Lens.at(1)
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name == "case"))
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [invalid_value | _rest] ->
        raise CompilationError,
          message: "Counting the distinct values of a `case` expression is not allowed.",
          source_location: invalid_value.source_location
    end
  end

  defp verify_aggregated_case_expressions_values(%Query{type: :anonymized} = query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&Function.aggregator?/1)
    |> Lens.key(:args)
    |> Lens.all()
    |> Query.Lenses.case_then_else_clauses()
    |> Lens.reject(&(Expression.constant?(&1) and &1.value in [1, nil]))
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [invalid_value | _rest] ->
        raise CompilationError,
          message: "Aggregated `case` expressions can only return the constants `1` or `NULL`.",
          source_location: invalid_value.source_location
    end
  end

  defp verify_bucketed_case_expressions_values(%Query{type: :anonymized} = query) do
    Query.Lenses.query_expressions()
    |> Query.Lenses.case_then_else_clauses()
    |> Lens.reject(&Expression.constant?/1)
    |> Lens.to_list(query)
    |> case do
      [] ->
        :ok

      [invalid_value | _rest] ->
        raise CompilationError,
          message: "`case` expressions in anonymizing queries can only return constant values.",
          source_location: invalid_value.source_location
    end
  end

  defp verify_case_expressions_conditions(%Query{type: :anonymized} = query) do
    when_clauses = Query.Lenses.query_expressions() |> Query.Lenses.case_when_clauses() |> Lens.to_list(query)

    when_clauses
    |> Enum.reject(&valid_when_clause?/1)
    |> case do
      [] ->
        :ok

      [invalid_when_clause | _rest] ->
        raise CompilationError,
          message:
            "`when` clauses from `case` expressions in anonymizing queries can only use a simple " <>
              "equality condition of the form `column = constant`.",
          source_location: invalid_when_clause.source_location
    end

    unless Application.get_env(:cloak, :allow_any_value_in_when_clauses) == true do
      when_clauses
      |> Enum.map(&Shadows.check_condition_safety(&1, query))
      |> Enum.reject(&(&1 == :ok))
      |> case do
        [] ->
          :ok

        [{:error, unsafe_target_constant} | _rest] ->
          raise CompilationError,
            message: """
            The target constants used in `when` clauses from `case` expressions in anonymizing queries have to
            be in the list of frequent values for that column, unless the Insights Cloak is explicitly configured
            to allow any value. For more information, see the "Configuring the system" section of the user guides.
            """,
            source_location: unsafe_target_constant.source_location
      end
    end
  end

  defp valid_when_clause?(%Expression{
         kind: :function,
         name: "=",
         args: [%Expression{kind: :column}, %Expression{kind: :constant}]
       }),
       do: true

  defp valid_when_clause?(_), do: false

  # -------------------------------------------------------------------
  # Unions
  # -------------------------------------------------------------------

  defp verify_unions_usage(%Query{command: :union, from: {:union, {:subquery, lhs}, {:subquery, rhs}}}) do
    if lhs.ast.type == :restricted or rhs.ast.type == :restricted do
      raise CompilationError, message: "Unions over restricted queries are forbidden."
    end
  end

  defp verify_unions_usage(%Query{}), do: :ok

  defp verify_union_columns(%Query{command: :union, from: {:union, {:subquery, lhs}, {:subquery, rhs}}}) do
    if Enum.count(lhs.ast.columns) != Enum.count(rhs.ast.columns) do
      raise CompilationError, message: "Queries in a `union` must have the same number of columns."
    end

    Enum.zip(lhs.ast.columns, rhs.ast.columns)
    |> Enum.filter(fn {lhs_column, rhs_column} -> lhs_column.type != rhs_column.type end)
    |> case do
      [] ->
        :ok

      [{lhs_column, _rhs_column} | _] ->
        raise CompilationError,
          message: "Queries in a `union` must have identical column types.",
          source_location: lhs_column.source_location
    end
  end

  defp verify_union_columns(%Query{}), do: :ok

  # -------------------------------------------------------------------
  # Helpers
  # -------------------------------------------------------------------

  defp aggregate_expression?(expression), do: not Enum.empty?(aggregate_subexpressions(expression))

  defp aggregate_subexpressions(expression),
    do:
      Query.Lenses.all_expressions()
      |> Lens.filter(&Function.aggregator?/1)
      |> Lens.to_list(expression)

  defp drop_case_conditions(query) do
    Query.Lenses.query_expressions()
    |> Query.Lenses.case_when_clauses()
    |> Query.Lenses.conditions()
    |> Lens.map(query, &drop_condition/1)
  end

  defp drop_filtering_conditions(query) do
    Query.Lenses.filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.map(query, &drop_condition/1)
  end

  # We can't remove the condition completely, as we still want to verify its arguments later,
  # so we change the function name to an invalid value, making it not a condition anymore.
  defp drop_condition(%Expression{kind: :function, name: "not", args: [expression]}), do: drop_condition(expression)
  defp drop_condition(%Expression{kind: :function} = expression), do: %Expression{expression | name: "__dropped"}
end
