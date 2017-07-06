defmodule Air.Schemas.ResultChunk do
  @moduledoc "Schema for the query result chunks."
  use Air.Schemas.Base
  require Logger

  @primary_key false
  schema "result_chunks" do
    field :query_id, Ecto.UUID, primary_key: true
    field :offset, :integer, primary_key: true
    field :row_count, :integer
    field :encoded_data, :binary

    belongs_to :query, Air.Schemas.Query, define_field: false
  end

  @type t :: %__MODULE__{
    query_id: String.t,
    offset: integer,
    row_count: integer,
    encoded_data: binary,
    query: %Ecto.Association.NotLoaded{} | Air.Schemas.Query.t
  }
end
