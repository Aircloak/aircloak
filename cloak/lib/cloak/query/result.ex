defmodule Cloak.Query.Result do
  @moduledoc """
  Module for defining a query result struct that in addition to query results
  allows a result to contain meta-data about the query execution.
  """

  alias Air.Query.Aggregator

  @type t :: %__MODULE__{
    rows: [Aggregator.bucket],
    columns: [String.t],
    users_count: non_neg_integer,
  }

  defstruct rows: [], columns: [], users_count: 0
end
