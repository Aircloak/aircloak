defmodule Cloak.Sql.Compiler.TypeChecker.Type do
  @moduledoc false

  alias Cloak.Sql.Compiler.TypeChecker.Type
  alias Cloak.Sql.{Expression, Function, Query}

  @type function_name :: String.t
  @type restricted_transformation :: {:restricted_function | :potentially_crashing_function, function_name}

  @type t :: %__MODULE__{
    # The names of functions that have been applied to a column or an expression
    applied_functions: [String.t],

    # Whether the expressions is a constant. As soon as a constant expression
    # interacts with a non-constant expression through math or function application
    # it ceases to be constant.
    constant?: boolean,

    # True if the expression is a column from the database without any processing. It might be a column in a subquery
    # that has been simply selected into the outer query.
    raw_column?: boolean,

    # True if the expression is a column from the database without any processing other than casts.
    cast_raw_column?: boolean,

    # True if the expression is an implicit range function applied to a cast_raw_column? argument.
    raw_implicit_range?: boolean,

    # True if any of the expressions it has come in contact with through functions
    # were constant.
    constant_involved?: boolean,

    # True if this expression includes a string manipulation function.
    string_manipulation?: boolean,

    # True if this expression includes a string manipulation function and any other function except for a single cast.
    unclear_string_manipulation?: boolean,

    # We keep track of the restricted transformations an expression has undergone in order
    # to later produce an explanation outlining the steps that led to a query being rejected.
    history_of_restricted_transformations: [restricted_transformation],

    # Keep track of the columns that have been involved in order to be able to produce better
    # explanations of why queries were rejected.
    history_of_columns_involved: [Expression.t]
  }

  defstruct [
    constant?: false, constant_involved?: false, raw_column?: false,
    cast_raw_column?: false, raw_implicit_range?: false,
    string_manipulation?: false, unclear_string_manipulation?: false,
    applied_functions: [], history_of_restricted_transformations: [], history_of_columns_involved: [],
  ]

  @math_operations_before_considered_constant 2


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def establish_type(column, query)
  def establish_type(:null, _query), do: constant()
  def establish_type({:distinct, column}, query), do: establish_type(column, query)
  def establish_type(:*, _query), do: column(:*)
  def establish_type(%Expression{constant?: true}, _query), do: constant()
  def establish_type(%Expression{function: nil} = column, query), do: expand_from_subquery(column, query)
  def establish_type(function = %Expression{function?: true}, query), do: type_for_function(function, query)


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

  defp column(expression), do:
    %Type{
      raw_column?: true,
      cast_raw_column?: true,
      constant?: false,
      history_of_columns_involved: [expression],
    }

  defp type_for_function(function = %Expression{function: name, function_args: args}, query) do
    child_types = args |> Enum.map(&(establish_type(&1, query)))
    # Prune constants, they don't interest us further
    if Enum.all?(child_types, &(&1.constant?)) do
      constant()
    else
      applied_functions = [name | Enum.flat_map(child_types, & &1.applied_functions)]
      %Type{
        applied_functions: applied_functions,
        cast_raw_column?: Function.cast?(function) and match?([%{raw_column?: true}], child_types),
        raw_implicit_range?: Function.has_attribute?(function, :implicit_range) and
          Enum.all?(child_types, &(&1.cast_raw_column? || &1.constant?)),
        constant_involved?: any_touched_by_constant?(child_types) ||
          math_operations_count(applied_functions) >= @math_operations_before_considered_constant,
        string_manipulation?: Function.has_attribute?(function, :string_manipulation) or
          Enum.any?(child_types, & &1.string_manipulation?),
        unclear_string_manipulation?: unclear_string_manipulation?(function, child_types),
        history_of_columns_involved: combined_columns_involved(child_types),
      }
      |> extend_history_of_restricted_transformations(name, child_types)
    end
  end

  defp unclear_string_manipulation?(function, child_types) do
    unclear_argument = Enum.any?(child_types, & &1.unclear_string_manipulation?)
    string_fun_of_unclear_expression =
      Function.string_manipulation_function?(function) and Enum.any?(child_types, &unclear_modification?/1)
    unclear_transform_of_string_fun_result =
      not Function.aggregator?(function) and Enum.any?(child_types, & &1.string_manipulation?)

    unclear_argument or string_fun_of_unclear_expression or unclear_transform_of_string_fun_result
  end

  @allowed_clear_casts 1
  @allowed_clear_funs 0
  defp unclear_modification?(%Type{constant?: true}), do: false
  defp unclear_modification?(type) do
    casts = transformation_count(type, &Function.cast?/1)
    transformations = transformation_count(type, & not Function.aggregator?(&1) and not Function.cast?(&1))

    (casts > @allowed_clear_casts) or (transformations > @allowed_clear_funs)
  end

  defp transformation_count(type, predicate), do:
    Enum.count(type.applied_functions, predicate)

  defp any_touched_by_constant?(types), do: Enum.any?(types, &(&1.constant_involved?))

  defp math_operations_count(applied_functions), do:
    applied_functions
    |> Enum.filter(& Function.math_function?(&1))
    |> Enum.count()

  defp combined_columns_involved(child_types), do:
    child_types
    |> Enum.flat_map(& &1.history_of_columns_involved)
    |> Enum.uniq()

  defp extend_history_of_restricted_transformations(%Type{constant_involved?: constant_involved?} = type,
      name, child_types) do
    full_history = child_types
    |> Enum.flat_map(& &1.history_of_restricted_transformations)
    |> prepend_if({:restricted_function, name}, restricted_function?(constant_involved?, name))
    |> prepend_if({:potentially_crashing_function, name}, performs_potentially_crashing_function?(name, child_types))
    %Type{type | history_of_restricted_transformations: full_history}
  end

  defp prepend_if(existing_values, _value, false), do: existing_values
  defp prepend_if(existing_values, value, true), do: [value | existing_values]

  defp restricted_function?(_constant_involved? = false, _name), do: false
  defp restricted_function?(_constant_involved? = true, name), do:
    Function.restricted_function?(name) or Function.math_function?(name)

  defp performs_potentially_crashing_function?("/", [_, child_type]), do:
    # This allows division by a pure constant, but not by a column influenced by a constant
    child_type.constant_involved? && not child_type.constant?
  defp performs_potentially_crashing_function?("sqrt", [child_type]), do:
    # This allows usage of square root on a pure constant, but not by a column influenced by a constant
    child_type.constant_involved? && not child_type.constant?
  defp performs_potentially_crashing_function?(_other, _child_type), do: false
end
