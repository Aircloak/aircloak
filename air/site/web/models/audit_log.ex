defmodule Air.AuditLog do
  use Air.Web, :model

  schema "audit_logs" do
    field :event, :string
    field :user, :string
    field :metadata, :string

    timestamps
  end

  @required_fields ~w(event user metadata)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
end
