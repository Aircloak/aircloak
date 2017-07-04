defmodule Air.Schemas.Query.Rows do
  @moduledoc """
  Schema for the query result.

  This schema corresponds to the `queries` tables, but only returns the `rows` field. The reason is that this field
  is potentially large, so we want to retrieve it only when needed.
  """
  use Air.Schemas.Base
  require Logger

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field :rows, :binary

    belongs_to :query, Air.Schemas.Query, foreign_key: :id, define_field: false
  end

  @type t :: %__MODULE__{rows: binary}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the decoded result with the decoded rows, if they are present."
  @spec decode(t | %Ecto.Association.NotLoaded{}) :: :not_loaded | nil | map
  def decode(%Ecto.Association.NotLoaded{}), do:
    :not_loaded
  def decode(rows), do:
    decode_raw(rows.rows)

  @doc "Decodes encoded rows, as sent by the cloak."
  @spec decode_raw(nil | binary) :: nil | [map]
  def decode_raw(nil), do:
    nil
  def decode_raw(encoded_rows), do:
    Aircloak.measure(:decode_rows,
      fn ->
        encoded_rows
        |> :zlib.gunzip()
        |> :jiffy.decode([:use_nil, :return_maps])
      end
    )
end
