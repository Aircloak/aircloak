defmodule Cloak.Sql.Query.Lenses do
  @moduledoc "Lenses for traversing queries"

  alias Cloak.Sql.{Expression, Function, Query, Condition}

  use Lens.Macros


  # -------------------------------------------------------------------
  # Lenses into Query
  # -------------------------------------------------------------------

  @doc "Lens focusing on all terminal elements in a query, including intermediate function invocations."
  deflens terminals(), do:
    Lens.multiple([
      Lens.keys([:columns, :group_by, :db_columns, :property, :aggregators]),
      Lens.key(:noise_layers) |> Lens.all() |> Lens.key(:expressions),
      Lens.key(:order_by) |> Lens.all() |> Lens.at(0),
      filters_operands(),
    ])
    |> terminal_elements()

  @doc "Lens focusing on all outermost analyst provided elements in the top-level query."
  deflens analyst_provided_expressions(), do:
    Lens.multiple([
      Lens.keys([:columns, :group_by]),
      Lens.key(:order_by) |> Lens.all() |> Lens.at(0),
      filters_operands(),
    ])

  @doc "Lens focusing on all terminal elements in a list of conditions."
  deflens conditions_terminals(), do: conditions() |> operands() |> terminal_elements()

  @doc "Lens focusing on all column elements in the query (subqueries are not included)."
  deflens query_expressions(), do: terminals() |> expressions()

  @doc "Lens focusing on all instances of the given expression."
  deflens expression_instances(expression), do:
    Lens.satisfy(query_expressions(), &(&1 == expression))

  @doc "Lens focusing on leaf (non-functions) expressions in a list of expressions."
  deflens leaf_expressions(), do: all_expressions() |> do_leaf_expressions()

  @doc "Lens focusing on all expressions in a list of expressions."
  deflens all_expressions(), do:
    Lens.both(terminal_elements(), conditions_terminals())
    |> expressions()

  @doc "Lens focusing on function expressions in the query that are sent to the database (subqueries are not included)."
  deflens db_needed_functions(), do:
    Lens.match(fn
      %Query{subquery?: true} -> query_expressions()
      %Query{subquery?: false} -> db_filter_clauses() |> conditions_terminals() |> expressions()
    end)
    |> Lens.satisfy(& &1.function?)

  @doc "Lens focusing on raw (uncompiled) casts of parameters."
  deflens raw_parameter_casts(), do:
    terminals()
    |> Lens.satisfy(&match?({:function, {:cast, _type}, [{:parameter, _index}]}, &1))

  @doc "Lens focusing on invocations of the bucket function"
  deflens buckets(), do: terminals() |> Lens.satisfy(&Function.bucket?/1)

  @doc "Lens focusing on all noise layers of subqueries of the query."
  deflens subquery_noise_layers(), do:
    direct_subqueries() |> Lens.key(:ast) |> Lens.key(:noise_layers) |> Lens.all()

  @doc "Lens focusing on all subqueries of a query."
  deflens subqueries(), do:
    Lens.match(fn(_) -> direct_subqueries() |> Lens.both(Lens.key(:ast) |> subqueries(), Lens.root()) end)

  @doc "Lens focusing on a query's immediate subqueries"
  deflens direct_subqueries(), do:
    Lens.key(:from)
    |> join_elements()
    |> Lens.satisfy(&match?({:subquery, _}, &1))
    |> Lens.at(1)

  @doc "Lens focusing on all inequality condition-clauses in a query."
  deflens order_condition_columns(), do:
    Lens.match(fn
      {:comparison, _, check, _} when check in ~w(> >= < <=)a ->
        Lens.indices([1, 3]) |> order_condition_columns()
      elements when is_list(elements) -> Lens.all() |> order_condition_columns()
      %Expression{} -> Lens.root()
      _ -> Lens.empty()
    end)

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

  @doc "Lens focusing on all subqueries which are part of a join."
  deflens joined_subqueries(), do:
    Lens.key(:from)
    |> Lens.satisfy(&match?({:join, _}, &1))
    |> join_elements()
    |> Lens.satisfy(&match?({:subquery, _}, &1))
    |> Lens.at(1)

  @doc "Lens focusing on all terminal elements in all join conditions of a query."
  deflens join_conditions_terminals(), do:
    join_conditions() |> conditions_terminals()

  @doc "Lens focusing on all sources in a query where conditions can be found."
  deflens filter_clauses(), do:
    Lens.multiple([
      Lens.keys([:where, :having]),
      join_conditions()
    ])

  @doc "Lens focusing on all sources in a query where database conditions can be found."
  deflens db_filter_clauses(), do:
    Lens.multiple([
      Lens.match(fn
        %Query{subquery?: true} -> Lens.keys([:where, :having])
        %Query{subquery?: false} -> Lens.key(:where)
        _ -> Lens.empty()
      end),
      join_conditions()
    ])

  @doc "Returns a list of lenses focusing on sets of join conditions of the given query."
  @spec join_condition_lenses(Query.t) :: [Lens.t]
  def join_condition_lenses(query), do: do_join_condition_lenses(query.from, Lens.key(:from))

  @doc "Lens focusing on the operands for a condition."
  deflens operands(), do:
    Lens.match(fn
      {:not, _} -> Lens.at(1) |> operands()
      {:comparison, _lhs, _comparator, _rhs} -> Lens.indices([1, 3])
      {:is, _, :null} -> Lens.at(1)
      {op, _, _} when op in [:like, :ilike] -> Lens.both(Lens.at(1), Lens.at(2))
      {:in, _, _} -> Lens.both(Lens.at(1), Lens.at(2) |> Lens.all())
      _ -> Lens.empty()
    end)

  @doc "Lens focusing on individual conditions."
  deflens conditions(), do:
    Lens.match(fn
      {:or, _, _} -> Lens.both(Lens.at(1), Lens.at(2)) |> conditions()
      {:and, _, _} -> Lens.both(Lens.at(1), Lens.at(2)) |> conditions()
      list when is_list(list) -> Lens.all() |> conditions()
      nil -> Lens.empty()
      _ -> Lens.root()
    end)

  @doc "Lens focusing on all conditions in joins."
  deflens join_conditions(), do: joins() |> Lens.key(:conditions)

  @doc "Lens focusing selected leaf tables in the parser AST."
  deflens ast_tables(), do:
    Lens.key(:from)
    |> ast_tables_recursive()

  @doc "Lens focusing on outermost selected splitters in a compiled query."
  deflens outermost_selected_splitters(), do:
    Lens.key(:columns)
    |> Lens.all()
    |> outermost_splitters_in_expression()

  @doc "Lens focusing on outermost where splitters in a compiled query."
  deflens outermost_where_splitters(), do:
    Lens.key(:where)
    |> conditions()
    |> operands()
    |> outermost_splitters_in_expression()

  @doc "Returns a list of lenses focusing on all subqueries of the given query."
  @spec subquery_lenses(Query.t) :: [Lens.t]
  def subquery_lenses(query), do: [Lens.root() | do_subquery_lenses(Lens.key(:from), query.from)]

  @doc "Lens focusing on all like/ilike/not like/not ilike where clauses in the query conditions."
  @spec like_clauses() :: Lens.t
  def like_clauses(), do:
    filter_clauses()
    |> conditions()
    |> Lens.satisfy(& Condition.like?(&1) or Condition.not_like?(&1))

  @doc "Lens focusing on all like/ilike/not like/not ilike patterns in the query conditions."
  @spec like_patterns() :: Lens.t
  def like_patterns(), do:
    like_clauses()
    |> Lens.match(fn
      {:not, {_kind, _lhs, _rhs}} -> Lens.at(1) |> Lens.at(2)
      {_kind, _lhs, _rhs} -> Lens.at(2)
    end)
    |> Lens.key(:value)


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

  defp do_subquery_lenses(base_lens, {:subquery, %{ast: subquery}}) do
    current_lens = base_lens |> Lens.at(1) |> Lens.key(:ast)
    [current_lens | do_subquery_lenses(current_lens |> Lens.key(:from), subquery.from)]
  end
  defp do_subquery_lenses(base_lens, {:join, %{lhs: lhs, rhs: rhs}}) do
    lhs_lens = base_lens |> Lens.at(1) |> Lens.key(:lhs)
    rhs_lens = base_lens |> Lens.at(1) |> Lens.key(:rhs)
    do_subquery_lenses(lhs_lens, lhs) ++ do_subquery_lenses(rhs_lens, rhs)
  end
  defp do_subquery_lenses(_base_lens, _from), do: []

  defp filters_operands(), do: filter_clauses() |> conditions() |> operands()

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

  defp do_leaf_expressions(lens), do: lens |> Lens.satisfy(&match?(%Expression{function?: false}, &1))

  deflensp join_elements(), do:
    Lens.match(fn
      {:join, _} ->
        Lens.seq_both(
          Lens.root(),
          Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> join_elements()
        )
      {:subquery, _} -> Lens.root()
      nil -> Lens.empty()
      _other -> Lens.root()
    end)

  deflensp ast_tables_recursive(), do:
    Lens.match(fn
      {:join, _} ->
        Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> ast_tables_recursive()
      {:subquery, _} -> Lens.empty()
      {_quoted, _table} -> Lens.root()
      {_identifier, :as, _alias} -> Lens.root()
    end)

  deflensp outermost_splitters_in_expression(), do:
    Lens.match(
      fn
        %Expression{function?: true} = function ->
          if Expression.row_splitter?(function) do
            Lens.root()
          else
            Lens.key(:function_args) |> Lens.all() |> outermost_splitters_in_expression()
          end

        _other ->
          Lens.empty()
      end
    )
end
