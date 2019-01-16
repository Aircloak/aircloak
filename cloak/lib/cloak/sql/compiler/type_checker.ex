defmodule Cloak.Sql.Compiler.TypeChecker do
  @moduledoc """
  Provides functions to check whether the expressions used in a query are valid according to our anonymization rules.
  This includes checks to determine if the number of restricted functions used (like functions that truncate values
  and could be used to create boolean logic circumventing other checks) exceed allowed thresholds, as well
  as checks to validate that columns used in certain filter conditions haven't been altered.
  """

  alias Cloak.Sql.{CompilationError, Condition, Expression, Query, Range}
  alias Cloak.Sql.Compiler.TypeChecker.{Access, Type}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.DataSource.{Isolators, Shadows}

  @max_allowed_restricted_functions 5

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec validate_allowed_usage_of_math_and_functions(Query.t()) :: Query.t()
  def validate_allowed_usage_of_math_and_functions(query) do
    each_anonymized_subquery(query, &verify_negative_conditions!/1)

    Helpers.each_subquery(query, fn subquery ->
      unless subquery.type == :standard do
        verify_usage_of_potentially_crashing_functions(subquery)
        verify_allowed_usage_of_math(subquery)
        verify_lhs_of_in_is_clear(subquery)
        verify_not_equals_is_clear(subquery)
        verify_lhs_of_not_like_is_clear(subquery)
        verify_string_based_conditions_are_clear(subquery)
        verify_string_based_expressions_are_clear(subquery)
        verify_ranges_are_clear(subquery)
        verify_isolator_conditions_are_clear(subquery)
        verify_raw_inequalities(subquery)
      end
    end)

    query
  end

  # -------------------------------------------------------------------
  # Top-level checks
  # -------------------------------------------------------------------

  defp verify_usage_of_potentially_crashing_functions(%Query{columns: columns} = query),
    do:
      Query.Lenses.db_filter_clauses()
      |> Query.Lenses.conditions()
      |> Query.Lenses.operands()
      |> Lens.to_list(query)
      |> Enum.concat(columns)
      |> Enum.each(fn column ->
        type = Type.establish_type(column, query)

        if potentially_crashing_function?(type) do
          raise CompilationError,
            source_location: column.source_location,
            message: """
            Functions are not allowed to be used in ways that could cause a database exception.
            This situation arises when a column or constant value is divided (`/`)
            by an expression that both contains a database column as well as a constant value
            (for example `age / (age - 20)`), or if the square root is taken of an expression that
            contains a database column as well as a constant value (for example `sqrt(age - 20)`).
            Please note that the system will also classify certain uses of math as potentially being
            a constant, such as `div(age, age)`.
            """
        end
      end)

  def verify_allowed_usage_of_math(query),
    do:
      Query.Lenses.analyst_provided_expressions()
      |> Lens.to_list(query)
      |> Enum.each(fn expression ->
        type = Type.establish_type(expression, query)

        if restricted_transformations_count(type) > @max_allowed_restricted_functions do
          raise CompilationError,
            source_location: expression.source_location,
            message: """
            Queries containing expressions with a high number of functions that are used in combination
            with constant values are prohibited. For further information about when this condition is
            triggered, please check the "Restrictions" section of the user guides.
            """
        end
      end)

  @allowed_in_functions ~w(
    lower upper substring trim ltrim rtrim btrim hour minute second year quarter month day weekday
  )
  defp verify_lhs_of_in_is_clear(query),
    do:
      verify_conditions(query, &Condition.in?/1, fn {:in, lhs, _} ->
        unless Type.establish_type(lhs, query) |> Type.clear_column?(&(&1 in @allowed_in_functions)) do
          raise CompilationError,
            source_location: lhs.source_location,
            message: """
            Only #{function_list(@allowed_in_functions)} can be used in the left-hand side of an IN operator.
            For more information see the "Restrictions" section of the user guides.
            """
        end
      end)

  @allowed_not_equals_functions ~w(
    lower upper substring trim ltrim rtrim btrim hour minute second year quarter month day weekday
  )
  defp verify_not_equals_is_clear(query),
    do:
      verify_conditions(query, &Condition.not_equals?/1, fn {:comparison, lhs, :<>, rhs} ->
        rhs_type = Type.establish_type(rhs, query)
        lhs_type = Type.establish_type(lhs, query)

        if rhs_type.constant? do
          unless Type.establish_type(lhs, query)
                 |> Type.clear_column?(&(&1 in @allowed_not_equals_functions)),
                 do:
                   raise(
                     CompilationError,
                     source_location: lhs.source_location,
                     message: """
                     Only #{function_list(@allowed_not_equals_functions)} can be used in the arguments of an <> operator.
                     For more information see the "Restrictions" section of the user guides.
                     """
                   )
        else
          {error, location} =
            cond do
              not Type.clear_column?(lhs_type) -> {true, lhs.source_location}
              not Type.clear_column?(rhs_type) -> {true, rhs.source_location}
              true -> {false, nil}
            end

          if error,
            do:
              raise(
                CompilationError,
                source_location: location,
                message: """
                No functions or mathematical operations are allowed when comparing two database columns with `<>`.
                For more information see the "Restrictions" section of the user guides.
                """
              )
        end
      end)

  defp verify_string_based_conditions_are_clear(query),
    do:
      verify_conditions(query, &(Condition.equals?(&1) or Condition.not_equals?(&1)), fn {:comparison, lhs, _, rhs} ->
        lhs_type = Type.establish_type(lhs, query)
        rhs_type = Type.establish_type(rhs, query)

        if Type.string_manipulation?(lhs_type) and not Type.clear_column?(rhs_type) and not rhs_type.constant?,
          do:
            raise(
              CompilationError,
              source_location: lhs.source_location,
              message: """
              Results of string manipulation functions can only be compared to constants.
              For more information see the "Text operations" subsection of the "Restrictions" section
              in the user guides.
              """
            )
      end)

  defp verify_string_based_expressions_are_clear(query),
    do:
      Query.Lenses.analyst_provided_expressions()
      |> Lens.to_list(query)
      |> Enum.each(fn expression ->
        if expression |> Type.establish_type(query) |> Type.unclear_string_manipulation?(),
          do:
            raise(
              CompilationError,
              source_location: expression.source_location,
              message: """
              String manipulation functions cannot be combined with other transformations.
              For more information see the "Text operations" subsection of the "Restrictions" section
              in the user guides.
              """
            )
      end)

  @allowed_like_functions []
  defp verify_lhs_of_not_like_is_clear(query),
    do:
      verify_conditions(query, &Condition.not_like?/1, fn {:not, {kind, lhs, _}} ->
        unless Type.establish_type(lhs, query) |> Type.clear_column?(&(&1 in @allowed_like_functions)) do
          raise CompilationError,
            source_location: lhs.source_location,
            message: """
            Expressions with NOT #{like_kind_name(kind)} cannot include any functions except aggregators and a cast.
            For more information see the "Restrictions" section of the user guides.
            """
        end
      end)

  defp like_kind_name(:like), do: "LIKE"
  defp like_kind_name(:ilike), do: "ILIKE"

  @allowed_range_functions ~w(hour minute second year quarter month day weekday)
  defp verify_ranges_are_clear(query),
    do:
      query
      |> Range.find_ranges()
      |> Enum.each(fn %Range{column: column, interval: interval} ->
        unless clear_range_lhs?(column, query, interval) do
          raise CompilationError,
            source_location: column.source_location,
            message: """
            Only #{function_list(@allowed_range_functions)} can be used in range expressions.
            For more information see the "Ranges" and "Implicit ranges" subsections of the "Restrictions"
            section in the user guides.
            """
        end
      end)

  defp clear_range_lhs?(%Expression{aggregate?: true, function_args: [lhs]}, query, interval),
    do: clear_range_lhs?(lhs, query, interval)

  defp clear_range_lhs?(lhs, query, :implicit),
    do: not (Type.establish_type(lhs, query) |> Type.unclear_implicit_range?())

  defp clear_range_lhs?(lhs, query, _),
    do: Type.establish_type(lhs, query) |> Type.clear_column?(&(&1 in @allowed_range_functions))

  defp function_list(function_names), do: function_names |> Enum.map(&"`#{&1}`") |> Aircloak.OxfordComma.join()

  # -------------------------------------------------------------------
  # Isolators
  # -------------------------------------------------------------------

  defp verify_isolator_conditions_are_clear(query) do
    query
    |> Access.potential_unclear_isolator_usages()
    |> Stream.filter(&includes_isolating_column?(&1, query))
    |> Enum.take(1)
    |> case do
      [] ->
        :ok

      [clause] ->
        [offending_column | _] = isolating_columns(clause, query)

        raise CompilationError,
          source_location: offending_column.source_location,
          message: """
          The column #{Expression.short_name(offending_column)} is isolating and cannot be used in this condition.
          For more information see the "Restrictions" section of the user guides.
          """
    end
  end

  defp includes_isolating_column?(clause, query) do
    not Enum.empty?(isolating_columns(clause, query))
  end

  defp isolating_columns(condition_or_expression, query) do
    Query.Lenses.leaf_expressions()
    |> Lens.to_list(condition_or_expression)
    |> Enum.flat_map(&columns_with_queries(&1, query))
    |> Enum.filter(fn {column, subquery} ->
      Isolators.isolates_users?(query.data_source, resolve_table_alias(column.table.name, subquery), column.name)
    end)
    |> Enum.map(&elem(&1, 0))
  end

  defp columns_with_queries(%{constant?: true}, _query), do: []

  defp columns_with_queries(column, query) do
    case Query.resolve_subquery_column(column, query) do
      :database_column ->
        [{column, query}]

      {column, subquery} ->
        column |> get_in([Query.Lenses.leaf_expressions()]) |> Enum.flat_map(&columns_with_queries(&1, subquery))
    end
  end

  defp resolve_table_alias(table, query) do
    case Map.fetch(query.table_aliases, table) do
      :error -> table
      {:ok, actual} -> actual.name
    end
  end

  # -------------------------------------------------------------------
  # Negative conditions
  # -------------------------------------------------------------------

  defp verify_negative_conditions!(query) do
    negative_conditions = query |> Access.negative_conditions() |> Enum.to_list()

    # We only verify negative conditions if there are enough of them. As a result, a potential cache bug won't
    # crash the queries with too few conditions to raise an error. Similarly, in such situations we don't need to wait
    # for cache to be primed.
    if length(negative_conditions) > Query.max_rare_negative_conditions(query),
      do: verify_negative_conditions!(query, negative_conditions)
  end

  defp verify_negative_conditions!(query, negative_conditions) do
    negative_conditions
    |> Stream.reject(&safe_negative_condition?/1)
    |> Stream.drop(Query.max_rare_negative_conditions(query))
    |> Enum.take(1)
    |> case do
      [] -> :ok
      [{_subquery, condition}] -> raise_negative_condition_error(query, condition)
    end
  end

  defp safe_negative_condition?({query, condition}) do
    case Shadows.safe?(condition, query) do
      {:ok, result} ->
        result

      {:error, :multiple_columns} ->
        raise CompilationError,
          source_location: Condition.subject(condition).source_location,
          message: "Negative conditions can only involve one database column."
    end
  end

  defp raise_negative_condition_error(query, offending_condition) do
    raise CompilationError,
      source_location: Condition.subject(offending_condition).source_location,
      message: """
      #{
        case Query.max_rare_negative_conditions(query) do
          0 -> "No negative conditions "
          1 -> "At most 1 negative condition "
          count -> "At most #{count} negative conditions "
        end
      }
      matching rare values are allowed.
      For further information see the "Number of conditions" subsection of the "Restrictions" section
      in the user guides.
      """
  end

  # -------------------------------------------------------------------
  # Inequalities
  # -------------------------------------------------------------------

  defp verify_raw_inequalities(query) do
    verify_conditions(query, &Condition.inequality?/1, fn condition ->
      sides = Condition.targets(condition)

      case {Enum.any?(sides, &Expression.constant?/1),
            Enum.find(sides, &(not Type.establish_type(&1, query).raw_column?))} do
        {true, _} ->
          :ok

        {_, nil} ->
          :ok

        {_, invalid} ->
          raise(
            CompilationError,
            source_location: invalid.source_location,
            message: "Only unmodified columns can be used in inequalities without constants."
          )
      end
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp potentially_crashing_function?(type),
    do:
      Enum.any?(type.history_of_restricted_transformations, fn
        {:potentially_crashing_function, _} -> true
        _ -> false
      end)

  defp restricted_transformations_count(type),
    do:
      type.history_of_restricted_transformations
      |> Enum.filter(fn
        {:restricted_function, _} -> true
        _ -> false
      end)
      |> Enum.count()

  defp verify_conditions(query, predicate, action),
    do: query |> Access.conditions(predicate) |> Enum.each(action)

  defp each_anonymized_subquery(query, function), do: Lens.each(Access.anonymized_queries(), query, function)
end
