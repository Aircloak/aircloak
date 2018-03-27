defmodule Air.Schemas.ResultChunk do
  @moduledoc "Schema for the query result chunks."
  use Air.Schemas.Base
  require Logger

  @primary_key false
  schema "result_chunks" do
    field(:query_id, Ecto.UUID, primary_key: true)
    field(:index, :integer, primary_key: true)
    field(:encoded_data, :binary)

    belongs_to(:query, Air.Schemas.Query, define_field: false)
  end

  @type t :: %__MODULE__{
          query_id: String.t(),
          index: non_neg_integer,
          encoded_data: binary,
          query: %Ecto.Association.NotLoaded{} | Air.Schemas.Query.t()
        }

  @type decoded_chunk :: %{index: non_neg_integer, buckets: map}

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns the json representation chunk buckets."
  @spec buckets_json(t) :: binary
  def buckets_json(chunk), do: :zlib.gunzip(chunk.encoded_data)

  @doc "Decodes the chunk and returns its buckets."
  @spec buckets(t) :: [map]
  def buckets(chunk),
    do:
      chunk
      |> buckets_json()
      |> :jiffy.decode([:use_nil, :return_maps])

  @doc "Eagerly converts buckets to rows."
  @spec rows(Enumerable.t()) :: [[any]]
  def rows(buckets),
    do:
      buckets
      |> rows_stream()
      |> Enum.to_list()

  @doc "Returns a stream of rows represented by the given collection of buckets."
  @spec rows_stream(Enumerable.t()) :: Enumerable.t()
  def rows_stream(buckets),
    do:
      Stream.flat_map(
        buckets,
        &List.duplicate(Map.fetch!(&1, "row"), Map.fetch!(&1, "occurrences"))
      )
end
