defmodule Central.Schemas.AirRPC do
  @moduledoc "Schema for Air RPCs. Allows us to ensure that perform an RPC at most once."

  use Central.Web, :model

  @type t :: %__MODULE__{}

  @primary_key {:id, :string, []}
  schema "air_rpcs" do
    timestamps()
  end
end
