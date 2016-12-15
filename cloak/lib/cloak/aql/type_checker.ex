defmodule Cloak.Aql.TypeChecker do
  @moduledoc """
  Allows a user to verify whether a column that is to be selected, or
  used as part of a WHERE-clause inequality has seen illegal transformations or not.

  The transformations that are considered illegal differ based on where
  a value is to be used. More specifically the rules differ between values
  used in WHERE-clauses and those that are selected for reporting.

  In particular this module considers whether a column has seen math
  or has had discontinuous functions applied to it. We currently only
  consider math and discontinuous functions dangerous if the input values
  to the function have been tainted by an analyst provided constant.
  Hence for example `abs(<pure-column>)` is ok, whereas
  `abs(<pure-column> + <constant>)` is not.
  Likewise `<pure-column> + <pure-column>` is ok, whereas
  `<pure-column> + (<pure-column> + <constant>)` is not.

  Note also that `<constant> MATH <constant>` is transformed to `<constant>`, and
  hence not considered as application of math.
  """

  alias Cloak.Aql.{Column, Compiler}

  defmodule Type do
    @moduledoc false

    @type t :: %__MODULE__{
      # Whether the value is a constant at this point?
      # A constant that has interacted with a non-constant column (for example
      # through math) is no longer a constant.
      constant?: boolean,

      # True if any of the values it has come in contact with through functions
      # were constant.
      touched_by_constant?: boolean,

      # True if the value has been processed by a discontinuous function and the
      # parameters of the function call were such that the computation is classified
      # as potentially dangerous (i.e. an attack vector).
      # Taints all other values, hence, if a single value in a function application
      # is discontinuous, then the result of the function is discontinous too.
      # Strings are handled differently here, and are only considered dangerously
      # discontinuous if the result of the discontinuous function is later turned
      # into a number
      dangerously_discontinuous?: boolean,

      # Whether there has been math performed on the value, and a value that has
      # been influenced by a constant was part of the math too.
      seen_dangerous_math?: boolean,
    }

    defstruct [
      constant?: false, dangerously_discontinuous?: false,
      seen_dangerous_math?: false, touched_by_constant?: false,
    ]
  end


  # -------------------------------------------------------------------
  # API and rules
  # -------------------------------------------------------------------

  @discontinuous_math_functions ~w(% abs ceil ceiling div floor mod round trunc sqrt)
  @discontinuous_string_functions ~w(btrim left ltrim right rtrim substring)
  @math_functions ~w(+ - * / ^ pow div)

  @doc "Returns true if a value is safe to be reported. False otherwise"
  @spec ok_for_display?(Type.t) :: boolean
  def ok_for_display?(type), do:
    not (type.dangerously_discontinuous? and type.seen_dangerous_math?)

  @doc "Returns true if a value is safe to be used in a WHERE-inequality. False otherwise"
  def ok_for_where_inquality?(type), do:
    not (type.dangerously_discontinuous? or type.seen_dangerous_math?)

  @doc """
  Produces a type characteristic for column by resolving it down to a table column.
  The type of the the underlying value is not of interest in this case, but rather
  the class of transformations that have been performed on the value.
  """
  @spec type(Column.t, Query.t) :: Type.t
  def type(column, query), do: construct_type_hierarchy(column, query)


  # -------------------------------------------------------------------
  # Function classification
  # -------------------------------------------------------------------

  defp dangerously_discontinuous?({:bucket, _}, _future, child_types), do:
    touched_by_constant?(child_types)
  defp dangerously_discontinuous?(name, _future, child_types)
      when name in @discontinuous_math_functions, do:
    touched_by_constant?(child_types)
  defp dangerously_discontinuous?("/", _future, [_, child_type]), do: child_type.touched_by_constant?
  defp dangerously_discontinuous?({:cast, _}, _future, _child_types), do: true
  defp dangerously_discontinuous?(name, future, child_types)
      when name in @discontinuous_string_functions, do:
    touched_by_constant?(child_types) and later_turned_into_a_number?(future)
  defp dangerously_discontinuous?(_name, _future, _child_types), do: false

  defp performs_dangerous_math?(name, _future, child_types) when name in @math_functions, do:
    touched_by_constant?(child_types)
  defp performs_dangerous_math?(_, _future, _child_types), do: false


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp later_turned_into_a_number?(future), do:
    Enum.any?(["length", {:cast, :integer}, {:cast, :real}, {:cast, :boolean}], &(Enum.member?(future, &1)))

  defp is_constant?(types), do: Enum.all?(types, &(&1.constant?))

  defp touched_by_constant?(types), do: Enum.any?(types, &(&1.touched_by_constant?))

  def seen_dangerous_math?(types), do: Enum.any?(types, &(&1.seen_dangerous_math?))

  def seen_dangerous_discontinuity?(types), do: Enum.any?(types, &(&1.dangerously_discontinuous?))

  defp constant(), do: %Type{constant?: true, touched_by_constant?: true}

  defp value(), do: %Type{constant?: false}

  defp construct_type_hierarchy(column, query, future \\ [])
  defp construct_type_hierarchy({:distinct, column}, query, future), do:
    construct_type_hierarchy(column, query, ["distinct" | future])
  defp construct_type_hierarchy(:*, _query, _future), do: value()
  defp construct_type_hierarchy(%Column{constant?: true}, _query, _future), do: constant()
  defp construct_type_hierarchy(%Column{db_function: nil} = column, query, future), do:
    expand_from_subquery(column, query, future)
  defp construct_type_hierarchy(%Column{db_function: name, db_function_args: args}, query, future), do:
    type_for_function(name, args, query, future)
  defp construct_type_hierarchy({:function, name, args}, query, future), do:
    type_for_function(name, args, query, future)

  defp type_for_function(name, args, query, future) do
    child_types = args |> Enum.map(&(construct_type_hierarchy(&1, query, [name | future])))
    # Prune constants, they don't interest us further
    if is_constant?(child_types) do
      constant()
    else
      %Type{
        touched_by_constant?: touched_by_constant?(child_types),
        seen_dangerous_math?:
          performs_dangerous_math?(name, future, child_types) || seen_dangerous_math?(child_types),
        dangerously_discontinuous?:
          dangerously_discontinuous?(name, future, child_types) || seen_dangerous_discontinuity?(child_types),
      }
    end
  end

  def expand_from_subquery(column, query, future) do
    %Column{name: column_name, table: %{name: table_name}} = column
    Lens.to_list(Compiler.direct_subqueries(), query)
    |> Enum.find(&(&1.alias == table_name))
    |> case do
      nil -> value()
      %{ast: subquery} ->
        column_index = Enum.find_index(subquery.column_titles, &(&1 == column_name))
        column = Enum.at(subquery.columns, column_index)
        construct_type_hierarchy(column, subquery, future)
    end
  end
end
