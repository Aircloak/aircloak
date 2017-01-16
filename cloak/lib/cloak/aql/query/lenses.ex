defmodule Cloak.Aql.Query.Lenses do
  @moduledoc "Lenses for traversing queries"

  alias Cloak.Aql.{Expression, Function}

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

  @doc "Lens focusing on expressions with the same id as the given expression."
  deflens expressions_like(other_expression), do:
    Lens.satisfy(expressions(), &(Expression.id(&1) == Expression.id(other_expression)))

  @doc "Lens focusing on invocations of row splitting functions"
  deflens splitter_functions(), do:
    terminal_elements()
    |> Lens.satisfy(&match?(%Expression{function?: true}, &1))
    |> Lens.satisfy(&Function.row_splitting_function?(&1.name))

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

  @doc "Lens focusing on all inequality condition-clauses in a query."
  deflens inequality_condition_columns(), do:
    do_inequality_columns()
    |> Lens.satisfy(&(not &1.constant?))

  @doc "Lens focusing on all equality condition-clauses in a query."
  deflens equality_condition_columns(), do:
    do_equality_columns()
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
  def sources_of_operands(), do:
    Lens.multiple([sources_of_operands_except_having(), Lens.key(:having)])

  @doc """
  Lens focusing on all sources in a query where condition operands can be found
  with the exception of in HAVING clauses. If HAVING clauses should also be
  included, use `sources_of_operands` instead
  """
  def sources_of_operands_except_having(), do:
    Lens.multiple([
      Lens.keys([:where, :encoded_where, :lcf_check_conditions]),
      join_conditions()
    ])


  # -------------------------------------------------------------------
  # Internal lenses
  # -------------------------------------------------------------------

  defp filters_operands(), do:
    sources_of_operands()
    |> Lens.all()
    |> operands()

  defp join_conditions(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&match?({:join, _}, &1))
    |> Lens.at(1)
    |> Lens.key(:conditions)

  deflensp do_inequality_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a ->
        Lens.indices([1, 3]) |> do_inequality_columns()
      elements when is_list(elements) -> Lens.all() |> do_inequality_columns()
      %Expression{} -> Lens.root()
      _ -> Lens.empty()
    end)

  deflensp do_equality_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a -> Lens.empty()
      {:comparison, _, _check, _} -> Lens.indices([1, 3]) |> do_equality_columns()
      {like, _, _} when like in ~w(like ilike)a -> Lens.indices([1, 2]) |> do_equality_columns()
      {:is, _, _} -> Lens.index(1) |> do_equality_columns()
      {:in, _, _} -> Lens.indices([1, 2]) |> do_equality_columns()
      {:not, _} -> Lens.index(1) |> do_equality_columns()
      elements when is_list(elements) -> Lens.all() |> do_equality_columns()
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
