defmodule Cloak.Sql.Compiler.TypeChecker.Type do
  @moduledoc false

  alias Cloak.Sql.Expression

  @type function_name :: String.t
  @type dangerous_transformation :: {:dangerous_function | :potentially_crashing_function, function_name}

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

    # We keep track of the dangerous transformations an expression has undergone in order
    # to later produce an explanation outlining the steps that led to a query being rejected.
    history_of_dangerous_transformations: [dangerous_transformation],

    # Keep track of the columns that have been involved in order to be able to produce better
    # explanations of why queries were rejected.
    history_of_columns_involved: [Expression.t]
  }

  defstruct [
    constant?: false, constant_involved?: false, raw_column?: false,
    cast_raw_column?: false, raw_implicit_range?: false, applied_functions: [],
    history_of_dangerous_transformations: [], history_of_columns_involved: [],
  ]
end
