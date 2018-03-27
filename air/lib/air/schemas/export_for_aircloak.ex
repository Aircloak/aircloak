defmodule Air.Schemas.ExportForAircloak do
  @moduledoc "Represents an export for aircloak."
  use Air.Schemas.Base

  @type t :: %__MODULE__{}

  schema "exports_for_aircloak" do
    field(:payload, :binary)

    timestamps()
  end

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the file name for this export."
  @spec file_name(t) :: String.t()
  def file_name(export),
    do: Timex.format!(export.inserted_at, "export_for_aircloak_{YYYY}{0M}{0D}{0h24}{0m}.acd")

  @doc "Returns the binary content of this export."
  @spec content(t) :: binary
  def content(export),
    do:
      :erlang.term_to_binary(%{
        id: export.id,
        payload: export.payload,
        created_at: export.inserted_at
      })
end
