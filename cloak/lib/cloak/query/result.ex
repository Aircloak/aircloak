defmodule Cloak.Query.Result do
  @moduledoc """
  Module for defining a query result struct that in addition to query results
  allows a result to contain meta-data about the query execution.
  """

  alias Cloak.Sql.{Expression, Query}

  @type t :: %__MODULE__{
    buckets: [bucket],
    columns: [String.t],
    types: [atom],
    users_count: non_neg_integer,
    features: map,
  }

  @type bucket :: %{row: [Cloak.DataSource.field], occurrences: pos_integer}

  defstruct buckets: [], columns: [], types: [], users_count: 0, features: %{}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the result struct that corresponds to the given query."
  @spec new(Query.t, [bucket], non_neg_integer) :: t
  def new(query, buckets, users_count \\ 0), do:
    %__MODULE__{buckets: buckets, users_count: users_count, columns: query.column_titles,
      features: Query.extract_features(query)}

  @doc "Eliminates duplicate rows."
  @spec distinct(t, boolean) :: t
  def distinct(%__MODULE__{buckets: buckets} = result, true), do:
    %__MODULE__{result | buckets: Enum.map(buckets, &Map.put(&1, :occurrences, 1))}
  def distinct(result, false), do: result


  @doc "Drops first `amount` rows from the result."
  @spec offset(t, non_neg_integer) :: t
  def offset(%__MODULE__{buckets: buckets} = result, amount), do:
    %__MODULE__{result | buckets: drop(buckets, amount)}

  @doc "Returns at most `amount` rows. If `amount` is `nil`, all rows are returned."
  @spec limit(t, pos_integer | nil) :: t
  def limit(result, nil), do: result
  def limit(%__MODULE__{buckets: buckets} = result, amount) do
    limited_buckets = buckets
      |> take(amount, [])
      |> Enum.reverse()
    %__MODULE__{result | buckets: limited_buckets}
  end

  @doc "Drops the non selected columns from the bucket rows."
  @spec drop_non_selected_columns(t, Query.t) :: t
  def drop_non_selected_columns(result, query) do
    selected_columns_indices = Enum.map(query.columns, &bucket_expression_index!(query, &1))

    %__MODULE__{result |
      buckets: Enum.map(result.buckets, &%{&1 | row: select_columns(&1.row, selected_columns_indices)})
    }
  end

  @doc "Returns the ordered list of bucket row expressions."
  @spec bucket_expressions(Query.t) :: [Expression.t]
  def bucket_expressions(query) do
    non_selected_order_by_expressions = Query.order_by_expressions(query) -- (query.columns ++ query.group_by)
    query.columns ++ non_selected_order_by_expressions
  end


  @doc "Returns the position of the given expression in the bucket row."
  @spec bucket_expression_index!(Query.t, Expression.t) :: non_neg_integer
  def bucket_expression_index!(query, expression) do
    index = Enum.find_index(bucket_expressions(query), &(&1 == expression))
    true = (index != nil)
    index
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp drop(buckets, 0), do: buckets
  defp drop([], _amount), do: []
  defp drop([%{occurrences: occurrences} | rest], amount) when occurrences <= amount, do:
    drop(rest, amount - occurrences)
  defp drop([%{occurrences: occurrences} = bucket | rest], amount), do:
    [%{bucket | occurrences: occurrences - amount} | rest]

  defp take([], _amount, acc), do: acc
  defp take([%{occurrences: occurrences} = bucket | rest], amount, acc) when occurrences < amount, do:
    take(rest, amount - occurrences, [bucket | acc])
  defp take([%{} = bucket | _rest], amount, acc), do: [%{bucket | occurrences: amount} | acc]

  defp select_columns(row, indices), do:
    Enum.map(indices, &Enum.at(row, &1))
end
