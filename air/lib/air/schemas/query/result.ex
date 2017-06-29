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

  @type t :: %__MODULE__{result: map, rows: binary}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the decoded result with the decoded rows, if they are present."
  @spec decode(t | %Ecto.Association.NotLoaded{}) :: :not_loaded | nil | map
  def decode(%Ecto.Association.NotLoaded{}), do:
    :not_loaded
  def decode(%__MODULE__{result: nil}), do:
    nil
  def decode(%__MODULE__{result: result_map, rows: encoded_rows}), do:
    Map.put(result_map, "rows", decoded_rows(result_map, encoded_rows))

  @doc "Decodes encoded rows."
  @spec decode_rows(binary) :: [map]
  def decode_rows(encoded_rows), do:
    :erlang.binary_to_term(encoded_rows)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # For backwards compatibility reasons, we need to handle the old format, where rows have been encoded directly in
  # the result map.
  defp decoded_rows(%{"rows" => rows}, nil), do:
    # old format
    rows
  defp decoded_rows(_, encoded_rows) when is_binary(encoded_rows), do:
    # new format
    decode_rows(encoded_rows)
  defp decoded_rows(_, _), do:
    nil
end
