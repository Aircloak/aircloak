defmodule Air.Schemas.Query.Result do
  @moduledoc """
  Schema for the query result.

  This schema corresponds to the `queries` tables, but only returns the `result` field. The reason is that this field
  is potentially large, so we want to retrieve it only when needed.
  """
  use Air.Schemas.Base

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field :result, :map
    field :rows, :binary

    belongs_to :query, Air.Schemas.Query, foreign_key: :id, define_field: false
  end
end
