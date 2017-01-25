defmodule Air.Schemas.Settings do
  @moduledoc "Schema for serializing Air.Settings into the DB."

  use Air.Schemas.Base

  schema "settings" do
    field :query_retention_days, :integer

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:query_retention_days])
  end
end
