defmodule Cloak.Aql.TypeChecker do
  @moduledoc """
  Provides functions to check whether selected columns, or expressions
  used in WHERE-clause inequalities have undergone dangerous transformations.

  What transformations are considered dangerous differs based on where
  an expression is to be used. WHERE-clause inequalities have stricter rules
  applied to them than selected columns.

  In particular this module considers whether a column has seen math
  or has had discontinuous functions applied to it. We currently only
  consider math and discontinuous functions dangerous if the inputs
  to a function have been tainted by an analyst-provided constant.
  Hence for example `abs(<pure-column>)` is ok, whereas
  `abs(<pure-column> + <constant>)` is not.
  Likewise `<pure-column> + <pure-column>` is ok, whereas
  `<pure-column> + (<pure-column> + <constant>)` is not.

  Note also that `<constant> MATH <constant>` and `DISCONTINUOUS_FUNCTION(<constant>)`
  are transformed to `<constant>`, and hence not considered as applications of math
  or discontinuous functions.
  """

  alias Cloak.Aql.{Expression, Query}

  defmodule Type do
    @moduledoc false

    @type function_name :: String.t
    @type offense :: {Expression.t, [{:dangerously_discontinuous | :dangerous_math, function_name}]}

    @type t :: %__MODULE__{
      # Whether the expressions is a constant. As soon as a constant expression
      # interacts with a non-constant expression through math or function application
      # it ceases to be constant.
      constant?: boolean,

      # True if any of the expressions it has come in contact with through functions
      # were constant.
      constant_involved?: boolean,

      # If a function like year, month, etc has been used on the value.
      been_through_datetime_function?: boolean,

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
      constant?: false, dangerously_discontinuous?: false,
      seen_dangerous_math?: false, constant_involved?: false,
      narrative_breadcrumbs: [], been_through_datetime_function?: false,
    ]
  end


  # -------------------------------------------------------------------
  # API and rules
  # -------------------------------------------------------------------

  @discontinuous_math_functions ~w(% abs ceil ceiling div floor mod round trunc sqrt)
  @discontinuous_string_functions ~w(btrim left ltrim right rtrim substring)
  @continuous_math_functions ~w(+ - * / ^ pow)
  @datetime_functions ~w(year month day hour minute second weekday)

  @doc "Returns true if an expression of this type is safe to be reported. False otherwise"
  @spec ok_for_display?(Type.t) :: boolean
  def ok_for_display?(type), do:
    not (type.dangerously_discontinuous? and type.seen_dangerous_math?)

  @doc "Returns true if an expression of this type is safe to be used in a WHERE-inequality. False otherwise"
  def ok_for_where_inequality?(type), do:
    not (type.dangerously_discontinuous? or type.seen_dangerous_math?)

  @doc """
  Produces a type characteristic for an expression by resolving function applications and references
  back to the underlying table columns and constants contributing to the expression.
  The type of the expression itself is not of interest. Rather the class of transformations
  that have been applied to the expression is what can make it safe or unsafe in a query.
  """
  @spec type(Expression.t, Query.t) :: Type.t
  def type(column, query), do: construct_type(column, query)


  # -------------------------------------------------------------------
  # Function classification
  # -------------------------------------------------------------------

  defp dangerously_discontinuous?({:bucket, _}, _future, child_types), do:
    any_touched_by_constant?(child_types)
  defp dangerously_discontinuous?(name, _future, child_types)
      when name in @discontinuous_math_functions, do:
    any_touched_by_constant?(child_types)
  defp dangerously_discontinuous?("/", _future, [_, child_type]), do:
    # This allows division by a pure constant, but not by a column influenced by a constant
    child_type.constant_involved? && not child_type.constant?
  defp dangerously_discontinuous?({:cast, _}, _future, child_types), do: any_touched_by_constant?(child_types)
  defp dangerously_discontinuous?(name, future, child_types)
      when name in @discontinuous_string_functions, do:
    any_touched_by_constant?(child_types) and later_turned_into_a_number?(future)
  defp dangerously_discontinuous?(_name, _future, _child_types), do: false

  defp performs_dangerous_math?(name, _future, child_types) when name in @continuous_math_functions, do:
    any_touched_by_constant?(child_types)
  defp performs_dangerous_math?(_, _future, _child_types), do: false

  defp performs_datetime_function?(name), do: name in @datetime_functions


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp later_turned_into_a_number?(future), do:
    Enum.any?(["length", {:cast, :integer}, {:cast, :real}, {:cast, :boolean}], &(Enum.member?(future, &1)))

  defp any_touched_by_constant?(types), do: Enum.any?(types, &(&1.constant_involved?))

  defp constant(), do: %Type{constant?: true, constant_involved?: true}

  defp column(expression), do: %Type{constant?: false, narrative_breadcrumbs: [{expression, []}]}

  defp construct_type(column, query, future \\ [])
  defp construct_type({:distinct, column}, query, future), do:
    construct_type(column, query, ["distinct" | future])
  defp construct_type(:*, _query, _future), do: column(:*)
  defp construct_type(%Expression{constant?: true}, _query, _future), do: constant()
  defp construct_type(%Expression{function: nil} = column, query, future), do:
    expand_from_subquery(column, query, future)
  defp construct_type(%Expression{function: name, function_args: args}, query, future), do:
    type_for_function(name, args, query, future)
  defp construct_type({:function, name, args}, query, future), do:
    type_for_function(name, args, query, future)

  defp type_for_function(name, args, query, future) do
    child_types = args |> Enum.map(&(construct_type(&1, query, [name | future])))
    # Prune constants, they don't interest us further
    if Enum.all?(child_types, &(&1.constant?)) do
      constant()
    else
      # This constructs a history of what has happened to a column in order to be able to later
      # produce a narrative for an analyst of the type: "column a underwent discontinuous functions
      # X prior to math Y, which is considered an offensive action".
      updated_narrative_breadcrumbs = child_types
      |> Enum.flat_map(&(&1.narrative_breadcrumbs))
      |> Enum.uniq()
      |> Enum.map(fn({expression, breadcrumbs}) ->
        breadcrumbs = if dangerously_discontinuous?(name, future, child_types) do
          [{:dangerously_discontinuous, name} | breadcrumbs] |> Enum.uniq()
        else
          breadcrumbs
        end
        breadcrumbs = if performs_dangerous_math?(name, future, child_types) do
          [{:dangerous_math, name} | breadcrumbs] |> Enum.uniq()
        else
          breadcrumbs
        end
        {expression, breadcrumbs}
      end)
      %Type{
        constant_involved?: any_touched_by_constant?(child_types),
        seen_dangerous_math?: performs_dangerous_math?(name, future, child_types) ||
          Enum.any?(child_types, &(&1.seen_dangerous_math?)),
        dangerously_discontinuous?: dangerously_discontinuous?(name, future, child_types) ||
          Enum.any?(child_types, &(&1.dangerously_discontinuous?)),
        narrative_breadcrumbs: updated_narrative_breadcrumbs,
        been_through_datetime_function?: performs_datetime_function?(name) ||
          Enum.any?(child_types, &(&1.been_through_datetime_function?)),
      }
    end
  end

  def expand_from_subquery(column, query, future) do
    %Expression{name: column_name, table: %{name: table_name}} = column
    Lens.to_list(Query.Lenses.direct_subqueries(), query)
    |> Enum.find(&(&1.alias == table_name))
    |> case do
      nil -> column(column)
      %{ast: subquery} ->
        column_index = Enum.find_index(subquery.column_titles, &(&1 == column_name))
        column = Enum.at(subquery.columns, column_index)
        construct_type(column, subquery, future)
    end
  end
end
