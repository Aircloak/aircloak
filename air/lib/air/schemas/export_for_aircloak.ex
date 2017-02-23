defmodule Air.Schemas.ExportForAircloak do
  @moduledoc "Represents an export for aircloak."
  use Air.Schemas.Base

  @type t :: %__MODULE__{}

  schema "exports_for_aircloak" do
    field :payload, :binary

    timestamps()
  end

  def file_name(export), do:
    Timex.format!(export.inserted_at, "export_for_aircloak_{YYYY}{0M}{0D}{0h24}{0m}.acd")
end
