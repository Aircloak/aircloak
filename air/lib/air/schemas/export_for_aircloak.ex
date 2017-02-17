defmodule Air.Schemas.ExportForAircloak do
  @moduledoc "Represents an export for aircloak."
  use Air.Schemas.Base

  @type t :: %__MODULE__{}

  schema "exports_for_aircloak" do
    field :payload, :binary

    timestamps()
  end
end
