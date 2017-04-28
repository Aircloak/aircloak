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
  @spec new(Query.t, [Expression.t], [bucket], non_neg_integer) :: t
  def new(query, columns, buckets, users_count \\ 0), do:
    %__MODULE__{
      buckets: final_buckets(query, columns, buckets),
      users_count: users_count,
      columns: query.column_titles,
      features: Query.extract_features(query)
    }


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp final_buckets(query, bucket_columns, buckets), do:
    buckets
    |> Cloak.Query.Sorter.order_rows(bucket_columns, query.order_by, &(&1.row))
    |> distinct(query.distinct?)
    |> offset(query.offset)
    |> limit(query.limit)
    |> drop_non_selected_columns(bucket_columns, query.columns)

  defp distinct(buckets, true), do: Enum.map(buckets, &Map.put(&1, :occurrences, 1))
  defp distinct(buckets, false), do: buckets

  defp offset(buckets, 0), do: buckets
  defp offset([], _amount), do: []
  defp offset([%{occurrences: occurrences} | rest], amount) when occurrences <= amount, do:
    offset(rest, amount - occurrences)
  defp offset([%{occurrences: occurrences} = bucket | rest], amount), do:
    [%{bucket | occurrences: occurrences - amount} | rest]

  defp limit(buckets, nil), do: buckets
  defp limit(buckets, amount), do:
    buckets
    |> take(amount, [])
    |> Enum.reverse()

  defp take([], _amount, acc), do: acc
  defp take([%{occurrences: occurrences} = bucket | rest], amount, acc) when occurrences < amount, do:
    take(rest, amount - occurrences, [bucket | acc])
  defp take([%{} = bucket | _rest], amount, acc), do: [%{bucket | occurrences: amount} | acc]

  def drop_non_selected_columns(buckets, selected_columns, selected_columns), do:
    # Optimization of the frequent case where selected columns are equal to bucket columns
    buckets
  def drop_non_selected_columns(buckets, bucket_columns, selected_columns) do
    selected_columns_indices =
      Enum.map(selected_columns, fn(selected_column) ->
        index = Enum.find_index(bucket_columns, &(&1 == selected_column))
        true = (index != nil)
        index
      end)

    Enum.map(buckets, &%{&1 | row: Enum.map(selected_columns_indices, fn(index) -> Enum.at(&1.row, index) end)})
  end
end
