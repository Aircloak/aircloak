defmodule Cloak.Query.Result do
  @moduledoc """
  Module for defining a query result struct that in addition to query results
  allows a result to contain meta-data about the query execution.
  """

  @type t :: %__MODULE__{
    buckets: [Cloak.Query.Aggregator.bucket],
    columns: [String.t],
    types: [atom],
    users_count: non_neg_integer,
    features: map,
  }

  defstruct buckets: [], columns: [], types: [], users_count: 0, features: %{}
end
