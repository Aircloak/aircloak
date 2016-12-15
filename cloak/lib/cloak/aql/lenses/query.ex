defmodule Cloak.Aql.Lenses.Query do
  @moduledoc "Lenses for traversing queries"

  alias Cloak.Aql.{Column, Function}

  use Lens.Macros


  # -------------------------------------------------------------------
  # Lenses into Query
  # -------------------------------------------------------------------

  @doc "Lens focusing on all terminal elements of a query, including intermediate function invocations."
  deflens terminal_elements() do
    Lens.match(fn
      {:function, "count", :*} -> Lens.empty()
      {:function, "count_noise", :*} -> Lens.empty()
      {:function, _, _} -> Lens.both(Lens.at(2) |> terminal_elements(), Lens.root())
      {:distinct, _} -> Lens.both(Lens.at(1) |> terminal_elements(), Lens.root())
      {_, :as, _} -> Lens.at(0)
      elements when is_list(elements) -> Lens.all() |> terminal_elements()
      _ -> Lens.root
    end)
  end

  @doc "Lens focusing on terminal elements that are part of WHERE-clauses"
  deflens where_terminal_elements() do
    Lens.match(fn
      {:not, _} -> Lens.at(1) |> where_terminal_elements()
      {:comparison, _lhs, _comparator, _rhs} -> Lens.indices([1, 3]) |> terminal_elements()
      {op, _, _} when op in [:in, :like, :ilike, :is] -> Lens.both(Lens.at(1), Lens.at(2)) |> terminal_elements()
    end)
  end

  @doc "Lens focusing on invocations of row splitting functions"
  deflens splitter_functions(), do: terminal_elements() |> Lens.satisfy(&Function.row_splitting_function?/1)

  @doc "Lens focusing on invocations of the bucket function"
  deflens buckets(), do: terminal_elements() |> Lens.satisfy(&Function.bucket?/1)

  @doc "Lens focusing on a query's immediate subqueries"
  deflens direct_subqueries(), do: Lens.key(:from) |> do_direct_subqueries()

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
  deflens leaf_tables, do: Lens.key(:from) |> do_leaf_tables()


  # -------------------------------------------------------------------
  # Internal lenses
  # -------------------------------------------------------------------

  deflensp do_where_inequality_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a ->
        Lens.indices([1, 3]) |> do_where_inequality_columns()
      elements when is_list(elements) -> Lens.all() |> do_where_inequality_columns()
      %Column{} -> Lens.root()
      _ -> Lens.empty()
    end)

  deflensp do_leaf_tables(), do:
    Lens.match(fn
      {:join, _} -> Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> do_leaf_tables()
      {:subquery, _} -> Lens.empty()
      table when is_binary(table) -> Lens.root()
    end)

  deflensp do_direct_subqueries(), do:
    Lens.match(fn
      {:join, _} -> Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> do_direct_subqueries()
      {:subquery, _} -> Lens.at(1)
      _ -> Lens.empty()
    end)
end
