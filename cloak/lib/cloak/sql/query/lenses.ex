defmodule Cloak.Sql.Query.Lenses do
  @moduledoc "Lenses for traversing queries"

  alias Cloak.Sql.{Expression, Function, Query, Compiler}

  use Lens.Macros

  @type query_filter :: [analyst_tables?: boolean]

  # -------------------------------------------------------------------
  # Lenses into Query
  # -------------------------------------------------------------------

  @doc "Lens focusing on all terminal elements in a query, including intermediate function invocations."
  deflens terminals(), do: query_fields() |> bottom_up_elements()

  @doc "Lens focusing on all terminal elements in a list of conditions."
  deflens conditions_terminals(), do: conditions() |> operands() |> bottom_up_elements()

  @doc "Lens focusing on all column elements in the query (subqueries are not included)."
  deflens query_expressions(), do: terminals() |> expressions()

  @doc "Lens focusing top-down on all column elements in the query (subqueries are not included)."
  deflens top_down_query_expressions(), do: query_fields() |> top_down_elements() |> expressions()

  @doc "Lens focusing on leaf (non-functions) expressions in a list of expressions."
  deflens leaf_expressions(), do: all_expressions() |> do_leaf_expressions()

  @doc "Lens focusing on all expressions in a list of expressions."
  deflens all_expressions(), do: bottom_up_elements() |> expressions()

  @doc "Lens focusing on function expressions in the query that are sent to the database (subqueries are not included)."
  deflens db_needed_functions() do
    Lens.match(fn
      %Query{type: :anonymized} -> Lens.key(:where) |> bottom_up_elements() |> expressions()
      _ -> query_expressions()
    end)
    |> Lens.filter(&Expression.function?/1)
  end

  @doc "Lens focusing on raw (uncompiled) casts of parameters."
  deflens raw_parameter_casts() do
    terminals()
    |> Lens.filter(&match?({:function, {:cast, _type}, [{:parameter, _index}], _}, &1))
  end

  @doc "Lens focusing on invocations of the bucket function"
  deflens buckets(), do: terminals() |> Lens.filter(&Function.bucket?/1)

  @doc "Lens focusing on all noise layers of subqueries of the query."
  @spec subquery_noise_layers(query_filter) :: Lens.t()
  def subquery_noise_layers(opts \\ []),
    do: direct_subqueries(opts) |> Lens.key(:ast) |> Lens.key(:noise_layers) |> Lens.all()

  @doc "Lens focusing on all queries (subqueries and top-level) of a query."
  @spec all_queries(query_filter) :: Lens.t()
  def all_queries(opts \\ []), do: Lens.both(subqueries(opts), Lens.root())

  @doc "Lens focusing on all subqueries (ignoring the top-level) of a query."
  @spec subqueries(query_filter) :: Lens.t()
  def subqueries(opts \\ []), do: Lens.recur(direct_subqueries(opts) |> Lens.key(:ast))

  @doc "Lens focusing on a query's immediate subqueries"
  @spec direct_subqueries(query_filter) :: Lens.t()
  def direct_subqueries(opts \\ []) do
    Lens.key(:from)
    |> join_elements()
    |> Lens.filter(&match?({:subquery, _}, &1))
    |> Lens.at(1)
    |> Lens.filter(fn subquery ->
      Keyword.get(opts, :analyst_tables?, true) or is_nil(subquery.ast.analyst_table)
    end)
  end

  @doc "Lens focusing on a query's immediate subqueries which represent analyst tables."
  deflens analyst_tables_subqueries() do
    Lens.key(:from)
    |> join_elements()
    |> Lens.filter(&match?({:subquery, _}, &1))
    |> Lens.filter(fn {:subquery, subquery} -> subquery.ast.analyst_table != nil end)
  end

  @doc "Lens focusing on the tables selected from the database. Does not include subqueries."
  deflens leaf_tables() do
    Lens.key(:from)
    |> join_elements()
    |> Lens.filter(&is_binary/1)
  end

  @doc "Lens focusing on all join elements of a query."
  deflens joins() do
    Lens.key(:from)
    |> join_elements()
    |> Lens.filter(&match?({:join, _}, &1))
    |> Lens.at(1)
  end

  @doc "Lens focusing on all subqueries which are part of a join."
  deflens joined_subqueries() do
    Lens.key(:from)
    |> Lens.filter(&match?({:join, _}, &1))
    |> join_elements()
    |> Lens.filter(&match?({:subquery, _}, &1))
    |> Lens.at(1)
  end

  @doc "Lens focusing on all terminal elements in all join conditions of a query."
  deflens join_conditions_terminals() do
    join_conditions() |> conditions_terminals()
  end

  @doc "Lens focusing on all sources in a query where row filters can be found."
  deflens filter_clauses() do
    Lens.both(
      Lens.keys([:where, :having]),
      join_conditions()
    )
  end

  @doc "Lens focusing on all sources in a query where row filters on sensitive data can be found."
  deflens pre_anonymization_filter_clauses() do
    Lens.both(
      Lens.match(fn
        %Query{type: :restricted} -> Lens.keys([:where, :having])
        %Query{type: :anonymized} -> Lens.key(:where)
        _ -> Lens.empty()
      end),
      join_conditions()
    )
  end

  @doc "Returns a list of lenses focusing on sets of join conditions of the given query."
  @spec join_condition_lenses(Query.t()) :: [Lens.t()]
  def join_condition_lenses(query), do: do_join_condition_lenses(query.from, Lens.key(:from))

  @doc "Lens focusing on the operands for a condition."
  deflens operands() do
    Lens.match(fn
      %Expression{kind: :function, name: "not"} ->
        Lens.key(:args) |> Lens.at(0) |> operands()

      %Expression{kind: :function, name: name} ->
        if Function.condition?(name), do: Lens.key(:args) |> Lens.all(), else: Lens.root()

      _ ->
        Lens.root()
    end)
  end

  @doc "Lens focusing on individual conditions."
  deflens conditions() do
    Lens.match(fn
      %Expression{kind: :function, name: name} when name in ~w(and or) ->
        Lens.key(:args) |> conditions()

      list when is_list(list) ->
        Lens.all() |> conditions()

      nil ->
        Lens.empty()

      _ ->
        Lens.root()
    end)
  end

  @doc "Lens focusing on the individual conditions which are 'AND'-ed together."
  deflens and_conditions() do
    Lens.match(fn
      %Expression{kind: :function, name: "and"} ->
        Lens.key(:args) |> Lens.all() |> conditions()

      %Expression{kind: :function, name: "or"} ->
        Lens.empty()

      list when is_list(list) ->
        Lens.all() |> conditions()

      nil ->
        Lens.empty()

      _ ->
        Lens.root()
    end)
  end

  @doc "Lens focusing on all conditions in joins."
  deflens join_conditions(), do: joins() |> Lens.key(:condition)

  @doc "Lens focusing selected leaf tables in the parser AST."
  deflens ast_tables(), do: Lens.key(:from) |> ast_tables_recursive()

  @doc "Returns a list of lenses focusing on all subqueries of the given query."
  @spec subquery_lenses(Query.t()) :: [Lens.t()]
  def subquery_lenses(query), do: [Lens.root() | do_subquery_lenses(Lens.key(:from), query.from)]

  @doc "Lens focusing on the `WHEN` clauses in `CASE` statements."
  deflens case_when_clauses() do
    Lens.match(fn
      %Expression{kind: :function, name: "case", args: args} ->
        branch_count = args |> length() |> div(2)
        when_branches = for i <- 0..(branch_count - 1), do: 2 * i
        Lens.key(:args) |> Lens.indices(when_branches)

      _ ->
        Lens.empty()
    end)
  end

  @doc "Lens focusing on the `THEN` and `ELSE` clauses in `CASE` statements."
  deflens case_then_else_clauses() do
    Lens.match(fn
      %Expression{kind: :function, name: "case", args: args} ->
        branch_count = args |> length() |> div(2)
        then_branches = for i <- 0..(branch_count - 1), do: 2 * i + 1
        Lens.key(:args) |> Lens.indices(then_branches ++ [branch_count * 2])

      _ ->
        Lens.empty()
    end)
  end

  @doc "Lens focusing on all expressions in a query used to form the groups for aggregation and anonymization."
  deflens group_expressions() do
    Lens.match(fn
      %Query{command: :select, type: :anonymized, group_by: []} = query ->
        if not Compiler.Helpers.aggregates?(query) do
          # Group by is not provided, and no selected expression is an aggregation function ->
          #   we're grouping on all selected columns + non selected order by expressions.
          Lens.both(
            Lens.key(:columns) |> Lens.all(),
            Lens.key(:order_by) |> Lens.all() |> Lens.at(0)
          )
        else
          Lens.empty()
        end

      _ ->
        Lens.key(:group_by) |> Lens.all()
    end)
  end

  @doc "Lens focusing on all selected columns of a query which are marked unselectable."
  deflens unselectable_selected_columns() do
    Lens.match(fn
      %{type: :anonymized} ->
        Lens.multiple([
          Lens.keys?([:columns, :group_by]) |> Lens.all(),
          Lens.key?(:order_by) |> Lens.all() |> Lens.at(0)
        ])

      %{type: :restricted} ->
        Lens.multiple([
          Lens.key?(:columns) |> Lens.all() |> Lens.filter(&Compiler.Helpers.aggregated_column?/1),
          Lens.key?(:order_by) |> Lens.all() |> Lens.at(0),
          Lens.key?(:having)
        ])

      _ ->
        Lens.empty()
    end)
    |> expression_unselectable_selected_columns()
  end

  @doc "Lens focusing on all columns of a selected expression which are marked unselectable."
  deflens expression_unselectable_selected_columns(),
    do: expression_potential_unselectable_selected_columns() |> Lens.filter(&unselectable_column?/1)

  # -------------------------------------------------------------------
  # Internal lenses
  # -------------------------------------------------------------------

  defp do_join_condition_lenses({:join, %{lhs: lhs, rhs: rhs}}, path) do
    base = path |> Lens.at(1)

    [base |> Lens.key(:condition)] ++
      do_join_condition_lenses(lhs, base |> Lens.key(:lhs)) ++ do_join_condition_lenses(rhs, base |> Lens.key(:rhs))
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

  deflensp bottom_up_elements() do
    Lens.match(fn
      {:function, "count", :*, _} ->
        Lens.empty()

      {:function, "count_noise", :*, _} ->
        Lens.empty()

      {:function, _, _, _} ->
        Lens.both(Lens.at(2) |> bottom_up_elements(), Lens.root())

      {:distinct, _} ->
        Lens.both(Lens.at(1) |> bottom_up_elements(), Lens.root())

      {_, :as, _} ->
        Lens.at(0) |> bottom_up_elements()

      elements when is_list(elements) ->
        Lens.all() |> bottom_up_elements()

      %Expression{kind: :function} ->
        Lens.both(Lens.key(:args) |> bottom_up_elements, Lens.root())

      _ ->
        Lens.root()
    end)
  end

  deflensp top_down_elements() do
    Lens.match(fn
      {:function, "count", :*, _} ->
        Lens.empty()

      {:function, "count_noise", :*, _} ->
        Lens.empty()

      {:function, _, _, _} ->
        Lens.both(Lens.root(), Lens.at(2) |> top_down_elements())

      {:distinct, _} ->
        Lens.both(Lens.root(), Lens.at(1) |> top_down_elements())

      {_, :as, _} ->
        Lens.at(0) |> top_down_elements()

      elements when is_list(elements) ->
        Lens.all() |> top_down_elements()

      %Expression{kind: :function} ->
        Lens.both(Lens.root(), Lens.key(:args) |> top_down_elements)

      _ ->
        Lens.root()
    end)
  end

  deflensp query_fields() do
    Lens.multiple([
      Lens.keys?([:columns, :group_by, :db_columns, :property, :aggregators]),
      Lens.key?(:noise_layers) |> Lens.all() |> Lens.key(:expressions),
      Lens.key?(:order_by) |> Lens.all() |> Lens.at(0),
      filter_clauses()
    ])
  end

  deflensp expressions() do
    Lens.filter(Lens.root(), fn
      {type, _, _, _} when type in [:identifier, :constant, :function] -> true
      %Expression{} -> true
      _ -> false
    end)
  end

  defp do_leaf_expressions(lens), do: Lens.reject(lens, &Expression.function?/1)

  deflensp join_elements() do
    Lens.match(fn
      {:join, _} ->
        Lens.seq_both(
          Lens.root(),
          Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> join_elements()
        )

      {:subquery, _} ->
        Lens.root()

      nil ->
        Lens.empty()

      _other ->
        Lens.root()
    end)
  end

  deflensp ast_tables_recursive() do
    Lens.match(fn
      {:join, _} -> Lens.at(1) |> Lens.keys([:lhs, :rhs]) |> ast_tables_recursive()
      {:subquery, _} -> Lens.empty()
      {_quoted, _table} -> Lens.root()
      {_identifier, :as, _alias} -> Lens.root()
    end)
  end

  deflensp expression_potential_unselectable_selected_columns() do
    Lens.match(fn
      %Expression{kind: :function, name: "count"} ->
        Lens.empty()

      %Expression{kind: :function, name: "count_noise"} ->
        Lens.empty()

      %Expression{kind: :function} ->
        Lens.key(:args)
        |> Lens.all()
        |> expression_potential_unselectable_selected_columns()

      %Expression{kind: :column} ->
        Lens.root()

      _ ->
        Lens.empty()
    end)
  end

  defp unselectable_column?(%{name: name, table: table}) do
    column = Enum.find(table.columns, &(&1.name == name))
    column != nil && column.access == :unselectable
  end
end
