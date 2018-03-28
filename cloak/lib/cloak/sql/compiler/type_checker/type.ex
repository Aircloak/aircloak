defmodule Cloak.Sql.Compiler.TypeChecker.Type do
  @moduledoc false

  alias Cloak.Sql.Compiler.TypeChecker.Type
  alias Cloak.Sql.{Expression, Function, Query}

  @type function_name :: String.t()
  @type restricted_transformation :: {:restricted_function | :potentially_crashing_function, function_name}

  @type t :: %__MODULE__{
          # The names of functions that have been applied to a column or an expression
          applied_functions: [String.t()],

          # Whether the expressions is a constant. As soon as a constant expression
          # interacts with a non-constant expression through math or function application
          # it ceases to be constant.
          constant?: boolean,

          # True if the expression is a column from the database without any processing. It might be a column in a
          # subquery that has been simply selected into the outer query.
          raw_column?: boolean,

          # True if any of the expressions it has come in contact with through functions
          # were constant.
          constant_involved?: boolean,

          # We keep track of the restricted transformations an expression has undergone in order
          # to later produce an explanation outlining the steps that led to a query being rejected.
          history_of_restricted_transformations: [restricted_transformation],

          # Keep track of the columns that have been involved in order to be able to produce better
          # explanations of why queries were rejected.
          history_of_columns_involved: [Expression.t()]
        }

  defstruct constant?: false,
            constant_involved?: false,
            raw_column?: false,
            applied_functions: [],
            history_of_restricted_transformations: [],
            history_of_columns_involved: []

  @math_operations_before_considered_constant 2

  @allowed_clear_casts 1
  @allowed_clear_funs 0
  @allowed_clear_string_manipulations 1

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns the Type struct for the given column in the context of the given query."
  @spec establish_type(Expression.t(), Query.t()) :: t
  def establish_type(column, query)
  def establish_type(:null, _query), do: constant()
  def establish_type({:distinct, column}, query), do: establish_type(column, query)
  def establish_type(:*, _query), do: column(:*)
  def establish_type(%Expression{constant?: true}, _query), do: constant()

  def establish_type(%Expression{function: nil} = column, query), do: expand_from_subquery(column, query)

  def establish_type(function = %Expression{function?: true}, query), do: type_for_function(function, query)

  @doc "Returns true if an implicit range function is used in combination with other functions"
  @spec unclear_implicit_range?(t) :: boolean
  def unclear_implicit_range?(type) do
    implicit_range_count = transformation_count(type, &Function.implicit_range?/1)

    has_forbidden_transformation =
      has_transformation?(type, fn function ->
        not Function.implicit_range?(function) and not Function.aggregator?(function) and not Function.cast?(function)
      end)

    implicit_range_count > 1 or (implicit_range_count == 1 and has_forbidden_transformation)
  end

  @doc """
  Returns true if the expression with the given type is a column from the database without any processing other than
  one cast, false otherwise. Functions in allowed_functions are ignored.
  """
  @spec clear_column?(t, [String.t()]) :: boolean
  def clear_column?(type, allowed_functions \\ []) do
    transforms =
      transformation_count(type, fn function ->
        not Function.cast?(function) and not Function.aggregator?(function) and not (function in allowed_functions)
      end)

    casts = transformation_count(type, &Function.cast?/1)

    transforms <= @allowed_clear_funs and casts <= @allowed_clear_casts
  end

  @doc "Returns true if the expression with the given type contains a string manipulation function, false otherwise."
  @spec string_manipulation?(t) :: boolean
  def string_manipulation?(type), do: transformation_count(type, &Function.string_manipulation_function?/1) > 0

  @doc """
  Returns true if the expression with the given type contains a string manipulation function together with other
  transformations, false otherwise. Aggregators and a single cast are ignored.
  """
  def unclear_string_manipulation?(type) do
    casts = transformation_count(type, &Function.cast?/1)

    transforms = transformation_count(type, &(not Function.aggregator?(&1) and not Function.cast?(&1)))

    string_manipulation?(type) and (casts > @allowed_clear_casts or transforms > @allowed_clear_string_manipulations)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp expand_from_subquery(column, query) do
    case Query.resolve_subquery_column(column, query) do
      :database_column -> column(column)
      {column, subquery} -> establish_type(column, subquery)
    end
  end

  defp constant(), do: %Type{constant?: true, constant_involved?: true}

  defp column(expression),
    do: %Type{
      raw_column?: true,
      constant?: false,
      history_of_columns_involved: [expression]
    }

  defp type_for_function(%Expression{function: name, function_args: args}, query) do
    child_types = args |> Enum.map(&establish_type(&1, query))
    # Prune constants, they don't interest us further
    if Enum.all?(child_types, & &1.constant?) do
      constant()
    else
      applied_functions = [name | Enum.flat_map(child_types, & &1.applied_functions)]

      %Type{
        applied_functions: applied_functions,
        constant_involved?:
          any_touched_by_constant?(child_types) ||
            math_operations_count(applied_functions) >= @math_operations_before_considered_constant,
        history_of_columns_involved: combined_columns_involved(child_types)
      }
      |> extend_history_of_restricted_transformations(name, child_types)
    end
  end

  defp has_transformation?(type, predicate), do: transformation_count(type, predicate) > 0

  defp transformation_count(type, predicate), do: Enum.count(type.applied_functions, predicate)

  defp any_touched_by_constant?(types), do: Enum.any?(types, & &1.constant_involved?)

  defp math_operations_count(applied_functions),
    do:
      applied_functions
      |> Enum.filter(&Function.math_function?(&1))
      |> Enum.count()

  defp combined_columns_involved(child_types),
    do:
      child_types
      |> Enum.flat_map(& &1.history_of_columns_involved)
      |> Enum.uniq()

  defp extend_history_of_restricted_transformations(
         %Type{constant_involved?: constant_involved?} = type,
         name,
         child_types
       ) do
    full_history =
      child_types
      |> Enum.flat_map(& &1.history_of_restricted_transformations)
      |> prepend_if({:restricted_function, name}, restricted_function?(constant_involved?, name))
      |> prepend_if(
        {:potentially_crashing_function, name},
        performs_potentially_crashing_function?(name, child_types)
      )

    %Type{type | history_of_restricted_transformations: full_history}
  end

  defp prepend_if(existing_values, _value, false), do: existing_values
  defp prepend_if(existing_values, value, true), do: [value | existing_values]

  defp restricted_function?(_constant_involved? = false, _name), do: false

  defp restricted_function?(_constant_involved? = true, name),
    do: Function.restricted_function?(name) or Function.math_function?(name)

  defp performs_potentially_crashing_function?("/", [_, child_type]),
    # This allows division by a pure constant, but not by a column influenced by a constant
    do: child_type.constant_involved? && not child_type.constant?

  defp performs_potentially_crashing_function?("sqrt", [child_type]),
    # This allows usage of square root on a pure constant, but not by a column influenced by a constant
    do: child_type.constant_involved? && not child_type.constant?

  defp performs_potentially_crashing_function?(_other, _child_type), do: false
end
