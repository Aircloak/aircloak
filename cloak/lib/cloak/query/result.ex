defmodule Cloak.Query.Result do
  @moduledoc """
  Module for defining a query result struct that in addition to query results
  allows a result to contain meta-data about the query execution.
  """

  alias Cloak.Sql.Query

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

  @doc """
    Creates the result struct that corresponds to the given query.

    This function takes the collection of aggregated buckets, and performs the
    final post-processing, according to the query specification. This will
    include sorting, offsetting, limiting rows, and removing non-selected
    columns.

    The result is a fully shaped query result.
  """
  @spec new({[bucket], non_neg_integer}, Query.t, Query.features) :: t
  def new({buckets, users_count}, query, features), do:
    %__MODULE__{
      buckets: final_buckets(query, buckets),
      users_count: users_count,
      columns: query.column_titles,
      features: features,
    }


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp final_buckets(query, buckets) do
    bucket_columns = Query.bucket_columns(query)

    buckets
    |> Cloak.Query.Sorter.order_rows(bucket_columns, query.order_by, &(&1.row))
    |> offset(query.offset)
    |> limit(query.limit)
    |> drop_non_selected_columns(bucket_columns, query.columns)
  end

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
