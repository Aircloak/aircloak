defmodule Cloak.Aql.Query.Lenses do
  @moduledoc "Lenses for traversing queries"

  alias Cloak.Aql.{Expression, Function, Query}

  use Lens.Macros


  # -------------------------------------------------------------------
  # Lenses into Query
  # -------------------------------------------------------------------

  @doc "Lens focusing all terminal elements in a query, including intermediate function invocations."
  deflens terminals(), do:
    Lens.multiple([
      Lens.keys([:columns, :group_by, :db_columns, :property, :aggregators]),
      Lens.key(:order_by) |> Lens.all() |> Lens.at(0),
      Lens.key(:ranges) |> Lens.all() |> Lens.key(:column),
      filters_operands(),
    ])
    |> terminal_elements()

  @doc "Lens focusing all terminal elements in a list of conditions."
  deflens conditions_terminals(), do:
    Lens.all()
    |> operands()
    |> terminal_elements()

  @doc "Lens focusing all column elements in the query (subqueries are not included)."
  deflens query_expressions(), do:
    terminals() |> expressions()

  @doc "Lens focusing leaf (non-functions) expressions in a list of expressions."
  deflens leaf_expressions(), do:
    Lens.both(terminal_elements(), conditions_terminals())
    |> expressions()
    |> do_leaf_expressions()

  @doc "Lens focusing all expressions in a list of expressions."
  deflens all_expressions(), do:
    Lens.both(terminal_elements(), conditions_terminals())
    |> expressions()

  @doc "Lens focusing on expressions with the same id as the given expression."
  deflens expressions_like(other_expression), do:
    Lens.satisfy(expressions(), &(Expression.id(&1) == Expression.id(other_expression)))

  @doc "Lens focusing on invocations of row splitting functions"
  deflens splitter_functions(), do:
    terminal_elements()
    |> Lens.satisfy(&match?(%Expression{function?: true}, &1))
    |> Lens.satisfy(&Function.has_attribute?(&1.name, :row_splitter))

  @doc "Lens focusing on raw (uncompiled) casts of parameters."
  deflens raw_parameter_casts(), do:
    terminals()
    |> Lens.satisfy(&match?({:function, {:cast, _type}, [{:parameter, _index}]}, &1))

  @doc "Lens focusing on invocations of the bucket function"
  deflens buckets(), do: terminal_elements() |> Lens.satisfy(&Function.bucket?/1)

  @doc "Lens focusing on a query's immediate subqueries"
  deflens direct_subqueries(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&match?({:subquery, _}, &1))
    |> Lens.at(1)

  @doc "Lens focusing on all condition-clauses in a query."
  deflens condition_columns(), do:
    Lens.both(do_order_condition_columns(), do_match_condition_columns())
    |> Lens.satisfy(&(not &1.constant?))

  @doc "Lens focusing on query's immediate projected subqueries"
  deflens direct_projected_subqueries(), do:
    Lens.satisfy(direct_subqueries(), &(&1.ast.projected?))

  @doc "Lens focusing on all inequality condition-clauses in a query."
  deflens order_condition_columns(), do:
    do_order_condition_columns()
    |> Lens.satisfy(&(not &1.constant?))

  @doc "Lens focusing on all match condition-clauses in a query."
  deflens match_condition_columns(), do:
    do_match_condition_columns()
    |> Lens.satisfy(&(not &1.constant?))

  @doc "Lens focusing on the tables selected from the database. Does not include subqueries."
  deflens leaf_tables(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&is_binary/1)

  @doc "Lens focusing on all join elements of a query."
  deflens joins(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&match?({:join, _}, &1))
    |> Lens.at(1)

  @doc "Lens focusing on all terminal elements in all join conditions of a query."
  deflens join_conditions_terminals(), do:
    joins()
    |> Lens.key(:conditions)
    |> conditions_terminals()

  @doc "Lens focusing on all sources in a query where condition operands can be found"
  def sources_of_operands(), do: sources_of_operands_except([])

  @doc """
  Lens focusing on all sources in a query where condition operands can be found
  with the exception of in HAVING clauses. If HAVING clauses should also be
  included, use `sources_of_operands` instead
  """
  def sources_of_operands_except(operands_to_exclude), do:
    Lens.multiple([
      Lens.keys([:where, :emulated_where, :lcf_check_conditions, :having] -- operands_to_exclude),
      join_conditions()
    ])

  @doc """
  Lens focusing on match conditions where where multiple column expressions
  are compared. An example being A = B. Returns the full expression
  rather than the sub-components of the match condition individually.

  Order conditions like for exampel > and < are excluded.
  """
  deflens comparisons(), do:
    Lens.match(fn
      {:comparison, _, _check, _} -> Lens.root()
      {like, _, _} when like in ~w(like ilike)a -> Lens.root()
      {:is, _, _} -> Lens.root()
      {:in, _, _} -> Lens.root()
      {:not, _} -> Lens.at(1) |> comparisons()
      elements when is_list(elements) -> Lens.all() |> comparisons()
      _ -> Lens.empty()
    end)

  @doc "Returns a list of lenses focusing on sets of join conditions of the given query."
  @spec join_condition_lenses(Query.t) :: [Lens.t]
  def join_condition_lenses(query), do: do_join_condition_lenses(query.from, Lens.key(:from))


  # -------------------------------------------------------------------
  # Internal lenses
  # -------------------------------------------------------------------

  defp do_join_condition_lenses({:join, %{lhs: lhs, rhs: rhs}}, path) do
    base = path |> Lens.at(1)
    [base |> Lens.key(:conditions)] ++
      do_join_condition_lenses(lhs, base |> Lens.key(:lhs)) ++
      do_join_condition_lenses(rhs, base |> Lens.key(:rhs))
  end
  defp do_join_condition_lenses(_, _), do: []

  defp filters_operands(), do:
    sources_of_operands()
    |> Lens.all()
    |> operands()

  defp join_conditions(), do:
    joins()
    |> Lens.key(:conditions)

  deflensp do_order_condition_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a ->
        Lens.indices([1, 3]) |> do_order_condition_columns()
      elements when is_list(elements) -> Lens.all() |> do_order_condition_columns()
      %Expression{} -> Lens.root()
      _ -> Lens.empty()
    end)

  deflensp do_match_condition_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a -> Lens.empty()
      {:comparison, _, _check, _} -> Lens.indices([1, 3]) |> do_match_condition_columns()
      {like, _, _} when like in ~w(like ilike)a -> Lens.indices([1, 2]) |> do_match_condition_columns()
      {:is, _, _} -> Lens.index(1) |> do_match_condition_columns()
      {:in, _, _} -> Lens.indices([1, 2]) |> do_match_condition_columns()
      {:not, _} -> Lens.index(1) |> do_match_condition_columns()
      elements when is_list(elements) -> Lens.all() |> do_match_condition_columns()
      %Expression{} -> Lens.root()
      _ -> Lens.empty()
    end)

  deflensp operands(), do:
    Lens.match(fn
      {:not, _} -> Lens.at(1) |> operands()
      {:comparison, _lhs, _comparator, _rhs} -> Lens.indices([1, 3])
      {op, _, _} when op in [:in, :like, :ilike, :is] -> Lens.both(Lens.at(1), Lens.at(2))
      _ -> Lens.empty()
    end)

  deflensp terminal_elements(), do:
    Lens.match(fn
      {:function, "count", :*} -> Lens.empty()
      {:function, "count_noise", :*} -> Lens.empty()
      {:function, _, _} -> Lens.both(Lens.at(2) |> terminal_elements(), Lens.root())
      {:distinct, _} -> Lens.both(Lens.at(1) |> terminal_elements(), Lens.root())
      {_, :as, _} -> Lens.at(0)
      elements when is_list(elements) -> Lens.all() |> terminal_elements()
      %Expression{function?: true} -> Lens.both(Lens.key(:function_args) |> terminal_elements, Lens.root())
      _ -> Lens.root
    end)

  deflensp expressions(), do:
    Lens.satisfy(Lens.root(), &match?(%Expression{}, &1))

  deflensp do_leaf_expressions(), do:
    Lens.match(fn
      %Expression{function: nil} -> Lens.root()
      _function_expression ->
        Lens.key(:function_args)
        |> Lens.all()
        |> terminal_elements()
        |> expressions()
        |> do_leaf_expressions()
    end)

  deflensp join_elements(), do:
    Lens.match(fn
      {:join, _} ->
        Lens.seq_both(
          Lens.root(),
          Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> join_elements()
        )
      {:subquery, _} -> Lens.root()
      nil -> Lens.empty()
      table when is_binary(table) -> Lens.root()
    end)
end
