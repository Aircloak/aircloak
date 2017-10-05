defmodule Cloak.Sql.Compiler.Normalization do
  @moduledoc "Deals with normalizing some expressions so that they are easier to deal with at later stages."

  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.{Expression, Query, LikePattern}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Modifies the query to remove certain expressions without changing semantics. Specifically:

  * Switches NOT IN expressions for an equivalent conjunction of <> expressions
  * Switches complex expressions involving constants (like 1 + 2 + 3) to with their results (6 in this case)
  * Switches upper(x) <> constant to lower(x) <> toggle_case(constant)
  * Removes redundant occurences of "%" from LIKE patterns (for example "%%" -> "%")
  * Normalizes sequences of "%" and "_" in like patterns so that the "%" always precedes a sequence of "_"
  * Normalizes `IN (single_value)` to `= single_value`

  These are useful for noise layers - we want to generate the same layer for semantically identical conditions,
  otherwise we have to fall back to probing.
  """
  @spec normalize(Query.t) :: Query.t
  def normalize(query), do:
    query
    |> Helpers.apply_bottom_up(&expand_not_in/1)
    |> Helpers.apply_bottom_up(&normalize_in/1)
    |> Helpers.apply_bottom_up(&normalize_like_patterns/1)
    |> Helpers.apply_bottom_up(&normalize_like/1)
    |> Helpers.apply_bottom_up(&normalize_constants/1)
    |> Helpers.apply_bottom_up(&normalize_order_by/1)
    |> Helpers.apply_bottom_up(&normalize_upper/1)
    |> Helpers.apply_bottom_up(&normalize_bucket/1)
    |> Helpers.apply_bottom_up(&alias_selected_constants/1)


  # -------------------------------------------------------------------
  # IN normalization
  # -------------------------------------------------------------------

  defp normalize_in(query), do:
    update_in(query, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:in, lhs, [exp]} -> {:comparison, lhs, :=, exp}
      other -> other
    end)

  defp expand_not_in(query), do:
    update_in(query, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:not, {:in, lhs, [exp | exps]}} ->
        Enum.reduce(exps, {:comparison, lhs, :<>, exp}, &{:and, {:comparison, lhs, :<>, &1}, &2})
      other -> other
    end)


  # -------------------------------------------------------------------
  # Collapsing constant expressions
  # -------------------------------------------------------------------

  defp normalize_constants(query), do:
    update_in(query, [Query.Lenses.terminals()], &do_normalize_constants/1)

  defp do_normalize_constants(expression = %Expression{function?: true, aggregate?: false}) do
    if Expression.constant?(expression) do
      Expression.constant(expression.type, Expression.const_value(expression), expression.parameter_index)
      |> put_in([Lens.key(:alias)], expression.alias)
    else
      expression
    end
  end
  defp do_normalize_constants(other), do: other


  # -------------------------------------------------------------------
  # Normalizing upper(x) <> constant
  # -------------------------------------------------------------------

  defp normalize_upper(query), do:
    update_in(query, [Query.Lenses.filter_clauses() |> Query.Lenses.conditions()], fn
      {:comparison, lhs = %Expression{function: "upper"}, :<>, rhs = %Expression{constant?: true}} ->
        {:comparison, %{lhs | function: "lower"}, :<>, update_in(rhs, [Lens.key(:value)], &toggle_case/1)}
      other -> other
    end)

  defp toggle_case(string), do:
    string
    |> String.graphemes()
    |> Enum.map(&toggle_one/1)
    |> Enum.join()

  defp toggle_one(string) do
    if String.upcase(string) == string do
      String.downcase(string)
    else
      String.upcase(string)
    end
  end


  # -------------------------------------------------------------------
  # Normalizing like patterns
  # -------------------------------------------------------------------

  defp normalize_like_patterns(query), do:
    Lens.map(Query.Lenses.like_patterns(), query, &LikePattern.normalize/1)

  defp normalize_like(query), do:
    Query.Lenses.like_clauses()
    |> Lens.satisfy(&trivial_like?/1)
    |> Lens.map(query, fn
      {:like, lhs, rhs} -> {:comparison, lhs, :=, LikePattern.trivial_to_string(rhs)}
      {:ilike, lhs, rhs} -> {:comparison, lowercase(lhs), :=, rhs |> LikePattern.trivial_to_string() |> lowercase()}
      {:not, {:like, lhs, rhs}} -> {:comparison, lhs, :<>, LikePattern.trivial_to_string(rhs)}
      {:not, {:ilike, lhs, rhs}} ->
        {:comparison, lowercase(lhs), :<>, rhs |> LikePattern.trivial_to_string() |> lowercase()}
    end)

  defp trivial_like?({:not, like}), do: trivial_like?(like)
  defp trivial_like?({_kind, _rhs, lhs}), do: LikePattern.trivial?(lhs.value)

  defp lowercase(expression), do:
    Expression.function("lower", [expression], expression.type)


  # -------------------------------------------------------------------
  # Normalizing bucket calls
  # -------------------------------------------------------------------

  defp normalize_bucket(query), do:
    Lens.map(Query.Lenses.buckets(), query, &expand_bucket(&1.function, &1.function_args))

  defp expand_bucket({:bucket, :lower}, [arg1, arg2]), do:
    # floor(arg1 / arg2) * arg2
    Expression.function("*", [
      arg2,
      Expression.function("floor", [
        Expression.function("/", [arg1, arg2])
      ])
    ])
  defp expand_bucket({:bucket, :upper}, [arg1, arg2]), do:
    # floor(arg1 / arg2) * arg2 + arg2
    Expression.function("+", [
      arg2,
      expand_bucket({:bucket, :lower}, [arg1, arg2])
    ])
  defp expand_bucket({:bucket, :middle}, [arg1, arg2]), do:
    # floor(arg1 / arg2) * arg2 + 0.5 * arg2
    Expression.function("+", [
      Expression.function("*", [Expression.constant(:real, 0.5), arg2]),
      expand_bucket({:bucket, :lower}, [arg1, arg2])
    ])


  # -------------------------------------------------------------------
  # Normalizing ORDER BY
  # -------------------------------------------------------------------

  defp normalize_order_by(query = %{subquery?: false}), do: query
  defp normalize_order_by(query) do
    case {query.order_by, remove_constant_ordering(query.order_by)} do
      {[], _} -> query
      {_, []} -> %{query | order_by: [{uid_expression(query), :asc}]}
      {_, order_list} -> %{query | order_by: order_list}
    end
  end

  defp remove_constant_ordering(order_list), do:
    Enum.reject(order_list, fn({expression, _direction}) -> expression.constant? end)

  defp uid_expression(query) do
    [table | _] = query.selected_tables
    uid = Enum.find(table.columns, & &1.name == table.user_id)
    false = is_nil(uid)
    Expression.column(uid, table)
  end

  defp alias_selected_constants(query = %{subquery?: false}), do: query
  defp alias_selected_constants(query) do
    Lens.key(:db_columns)
    |> Lens.all()
    |> Lens.satisfy(& &1.constant? and &1.alias in ["", nil])
    |> Lens.map(query, &Helpers.set_unique_alias/1)
  end
end
