defmodule Cloak.Sql.TypeChecker.Type do
  @moduledoc false

  alias Cloak.Sql.Expression

  @type function_name :: String.t
  @type offense_type :: {
      :dangerously_discontinuous
    | :dangerous_math
    | :datetime_processing
    | :potentially_crashing_function,
    function_name
  }
  @type offense :: {Expression.t, [offense_type]}

  @type t :: %__MODULE__{
    # Whether the expressions is a constant. As soon as a constant expression
    # interacts with a non-constant expression through math or function application
    # it ceases to be constant.
    constant?: boolean,

    # True if the expression is a column from the database without any processing. It might be a column in a subquery
    # that has been simply selected into the outer query.
    raw_column?: boolean,

    # True if any of the expressions it has come in contact with through functions
    # were constant.
    constant_involved?: boolean,

    # Whether the expression represents a date, time or datetime value, or one of
    # the expressions in this expressions past represented such a value.
    datetime_involved?: boolean,

    # If a function like year, month, etc has been used on the value, or the value
    # has in some other way been manipulated, like having been cast.
    is_result_of_datetime_processing?: boolean,

    # sqrt and / are functions which are illdefined for certain values. sqrt of negative values,
    # or division by 0. When these functions occur with values that have been manipulated
    # using constants (to potentially construct a failure condition), we mark them as
    # potentially crashing.
    is_result_of_potentially_crashing_function?: boolean,

    # True if the expression has been processed by a discontinuous function and the
    # parameters of the function call were such that the computation is classified
    # as potentially dangerous (i.e. an attack vector).
    # Taints all other later expressions, hence, if a single expression in a function
    # application is dangerously discontinuous, then the result of the function is
    # dangerously discontinous too.
    dangerously_discontinuous?: boolean,

    # Whether the expression has had dangerous math performed on it or not.
    # Math is considered dangerous if any of the expressions in the math application
    # have previously been touched by a constant.
    seen_dangerous_math?: boolean,

    # We keep track of the dangerous transformations each column has undergone in order
    # to later produce a narrative to the analyst explaining what particular steps led to
    # a query expression being rejected.
    narrative_breadcrumbs: [offense],
  }

  defstruct [
    constant?: false, constant_involved?: false, datetime_involved?: false,
    is_result_of_datetime_processing?: false, is_result_of_potentially_crashing_function?: false,
    dangerously_discontinuous?: false, seen_dangerous_math?: false, narrative_breadcrumbs: [], raw_column?: false
  ]
end
