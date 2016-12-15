defmodule Cloak.Aql.Lenses.Query do
  @moduledoc "Lenses for traversing queries"

  alias Cloak.Aql.{Column, Function}

  use Lens.Macros


  # -------------------------------------------------------------------
  # Lenses into Query
  # -------------------------------------------------------------------

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

  deflens where_terminal_elements() do
    Lens.match(fn
      {:not, _} -> Lens.at(1) |> where_terminal_elements()
      {:comparison, _lhs, _comparator, _rhs} -> Lens.both(Lens.at(1), Lens.at(3)) |> terminal_elements()
      {op, _, _} when op in [:in, :like, :ilike, :is] -> Lens.both(Lens.at(1), Lens.at(2)) |> terminal_elements()
    end)
  end

  deflens splitter_functions(), do: terminal_elements() |> Lens.satisfy(&Function.row_splitting_function?/1)

  deflens buckets(), do: terminal_elements() |> Lens.satisfy(&Function.bucket?/1)

  deflens direct_subqueries(), do: Lens.key(:from) |> do_direct_subqueries()

  deflens do_direct_subqueries() do
    Lens.match(fn
      {:join, _} -> Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> do_direct_subqueries()
      {:subquery, _} -> Lens.at(1)
      _ -> Lens.empty()
    end)
  end

  deflens where_inequality_columns(), do:
    Lens.keys([:where, :lcf_check_conditions])
    |> do_where_inequality_columns()
    |> Lens.satisfy(&(not &1.constant?))

  deflens queries(), do:
    Lens.both(direct_subqueries(), Lens.root())

  deflens do_where_inequality_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a ->
        Lens.indices([1, 3]) |> do_where_inequality_columns()
      elements when is_list(elements) -> Lens.all() |> do_where_inequality_columns()
      %Column{} -> Lens.root()
      _ -> Lens.empty()
    end)

  deflens leaf_tables, do: Lens.key(:from) |> do_leaf_tables()

  deflens do_leaf_tables() do
    Lens.match(fn
      {:join, _} -> Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> do_leaf_tables()
      {:subquery, _} -> Lens.empty()
      table when is_binary(table) -> Lens.root()
    end)
  end
end
