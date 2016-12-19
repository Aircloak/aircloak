defmodule Cloak.Aql.Query.Lenses do
  @moduledoc "Lenses for traversing queries"

  alias Cloak.Aql.{Column, Function}

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
      filter_parents(),
    ])
    |> terminal_elements()

  @doc "Lens focusing all terminal elements in a list of conditions."
  deflens conditions_terminals(), do:
    Lens.all()
    |> conditions_parents()
    |> terminal_elements()

  @doc "Lens focusing on invocations of row splitting functions"
  deflens splitter_functions(), do: terminal_elements() |> Lens.satisfy(&Function.row_splitting_function?/1)

  @doc "Lens focusing on invocations of the bucket function"
  deflens buckets(), do: terminal_elements() |> Lens.satisfy(&Function.bucket?/1)

  @doc "Lens focusing on a query's immediate subqueries"
  deflens direct_subqueries(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&match?({:subquery, _}, &1))
    |> Lens.at(1)

  @doc """
  Lens focusing on all queries found in a query. Also includes the top-level query itself.
  More concretely, given a query containing a subquery, the main query as well as the subquery
  will be given focus.
  """
  deflens queries(), do:
    Lens.both(direct_subqueries(), Lens.root())

  @doc "Lens focusing on all WHERE-clause inequalities in a query."
  deflens where_inequality_columns(), do:
    Lens.keys([:where, :lcf_check_conditions])
    |> do_where_inequality_columns()
    |> Lens.satisfy(&(not &1.constant?))

  @doc "Lens focusing on the tables selected from the database. Does not include subqueries."
  deflens leaf_tables(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&is_binary/1)

  # -------------------------------------------------------------------
  # Internal lenses
  # -------------------------------------------------------------------

  deflensp filter_parents(), do:
    Lens.multiple([
      Lens.keys([:where, :having, :encoded_where, :lcf_check_conditions]),
      join_conditions()
    ])
    |> Lens.all()
    |> conditions_parents()

  deflensp join_conditions(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&match?({:join, _}, &1))
    |> Lens.at(1)
    |> Lens.key(:conditions)

  deflensp do_where_inequality_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a ->
        Lens.indices([1, 3]) |> do_where_inequality_columns()
      elements when is_list(elements) -> Lens.all() |> do_where_inequality_columns()
      %Column{} -> Lens.root()
      _ -> Lens.empty()
    end)

  deflensp conditions_parents(), do:
    Lens.match(fn
      {:not, _} -> Lens.at(1) |> conditions_parents()
      {:comparison, _lhs, _comparator, _rhs} -> Lens.indices([1, 3])
      {op, _, _} when op in [:in, :like, :ilike, :is] -> Lens.both(Lens.at(1), Lens.at(2))
    end)

  deflensp terminal_elements(), do:
    Lens.match(fn
      {:function, "count", :*} -> Lens.empty()
      {:function, "count_noise", :*} -> Lens.empty()
      {:function, _, _} -> Lens.both(Lens.at(2) |> terminal_elements(), Lens.root())
      {:distinct, _} -> Lens.both(Lens.at(1) |> terminal_elements(), Lens.root())
      {_, :as, _} -> Lens.at(0)
      elements when is_list(elements) -> Lens.all() |> terminal_elements()
      _ -> Lens.root
    end)

  deflensp join_elements(), do:
    Lens.match(fn
      {:join, _} ->
        Lens.seq_both(
          Lens.root(),
          Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> join_elements()
        )
      {:subquery, _} -> Lens.root()
      table when is_binary(table) -> Lens.root()
    end)
end
