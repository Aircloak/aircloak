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
    offset: non_neg_integer,
    row_count: pos_integer,
    encoded_data: binary,
    query: %Ecto.Association.NotLoaded{} | Air.Schemas.Query.t
  }

  @type decoded_chunk :: %{offset: non_neg_integer, buckets: map}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns offset and decoded buckets associated with this chunk."
  @spec decode(t) :: decoded_chunk
  def decode(chunk), do:
    chunk
    |> Map.take([:offset])
    |> Map.put(:buckets, chunk.encoded_data |> :zlib.gunzip() |> :jiffy.decode([:use_nil, :return_maps]))
end
